# ENTSO-E grid outages: scrape the currently displayed table into one CSV.
# Open grid-outages.Rproj in RStudio, then run this entire script.

# 1. Packages
for (pkg in c("selenium", "rvest")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# 2. Start Selenium Server automatically if it is not already running
selenium_ready <- function() {
  tryCatch({
    con <- url("http://127.0.0.1:4444/status", open = "r")
    on.exit(close(con), add = TRUE)
    length(readLines(con, n = 1L, warn = FALSE)) > 0L
  }, error = function(e) FALSE)
}

if (!selenium_ready()) {
  server_jar <- path.expand("~/Downloads/selenium-server-4.51.0.jar")
  if (!file.exists(server_jar)) {
    stop("Selenium Server not found at: ", server_jar)
  }
  log_file <- file.path(tempdir(), "selenium-server.log")
  system2(
    "java",
    args = c("-jar", shQuote(server_jar), "standalone", "--selenium-manager", "true"),
    stdout = log_file,
    stderr = log_file,
    wait = FALSE
  )
  for (i in seq_len(30)) {
    if (selenium_ready()) break
    Sys.sleep(1)
  }
  if (!selenium_ready()) stop("Selenium did not start. Check: ", log_file)
}

# 3. Launch Chrome and open ENTSO-E
browser <- selenium::SeleniumSession$new(browser = "chrome", port = 4444L)
browser$navigate("https://transparency.entsoe.eu/outages/transmission")

# 4. Set region + date in Chrome, then return to RStudio
readline("Select ONE region and a seven-day date range in Chrome; wait for the table, then press Enter here: ")

rows_loaded <- FALSE

for (i in seq_len(60)) {
  page <- rvest::read_html(browser$get_page_source())
  
  if (length(rvest::html_elements(page, "tr[data-item-id]")) > 0) {
    rows_loaded <- TRUE
    break
  }
  
  Sys.sleep(1)
}

if (!rows_loaded) stop("No outage rows appeared. Check that the table shows results.")


# 5. Extract the displayed HTML table
fields <- c(
  "affected_areas", "asset_name", "asset_code", "type",
  "status", "start_end_time", "reason", "comments"
)

get_visible_rows <- function() {
  page <- rvest::read_html(browser$get_page_source())
  rows <- rvest::html_elements(page, "tr[data-item-id]")
  
  if (length(rows) == 0) stop("No outage rows found.")
  
  result <- lapply(rows, function(row) {
    values <- trimws(rvest::html_text2(
      rvest::html_elements(row, "td")
    ))
    
    if (length(values) < 9) stop("A row has fewer than 9 cells.")
    
    data.frame(
      id = rvest::html_attr(row, "data-item-id"),
      as.list(stats::setNames(values[2:9], fields)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  
  do.call(rbind, result)
}

scroll_table <- function(to_top = FALSE) {
  script <- if (to_top) {
    "
    let e = document.querySelector('tr[data-item-id]')?.parentElement;
    while (e && e !== document.body && e !== document.documentElement) {
      const s = getComputedStyle(e);
      if (e.scrollHeight > e.clientHeight + 5 &&
          /auto|scroll/.test(s.overflowY)) {
        e.scrollTop = 0;
        break;
      }
      e = e.parentElement;
    }
    "
  } else {
    "
    let e = document.querySelector('tr[data-item-id]')?.parentElement;
    while (e && e !== document.body && e !== document.documentElement) {
      const s = getComputedStyle(e);
      if (e.scrollHeight > e.clientHeight + 5 &&
          /auto|scroll/.test(s.overflowY)) {
        e.scrollTop += Math.floor(e.clientHeight * 0.6);
        break;
      }
      e = e.parentElement;
    }
    "
  }
  
  browser$execute_script(script)
  Sys.sleep(1)
}

outages <- get_visible_rows()[0, , drop = FALSE]

repeat {
  scroll_table(to_top = TRUE)
  Sys.sleep(1)
  page_start_ids <- get_visible_rows()$id
  stable <- 0
  
  repeat {
    before <- nrow(outages)
    outages <- rbind(outages, get_visible_rows())
    outages <- outages[!duplicated(outages$id), , drop = FALSE]
    
    scroll_table()
    outages <- rbind(outages, get_visible_rows())
    outages <- outages[!duplicated(outages$id), , drop = FALSE]
    
    if (nrow(outages) == before) stable <- stable + 1 else stable <- 0
    if (stable == 2) break
  }
  
  write.csv(outages, "data/raw/outages_seven_days.csv", row.names = FALSE)
  
  browser$execute_script("
    const b = [...document.querySelectorAll('button')]
      .find(b => b.querySelector('.uugds-chevron-right'));
    if (b && !b.disabled) b.click();
  ")
  Sys.sleep(2)
  scroll_table(to_top = TRUE)
  Sys.sleep(1)
  
  if (identical(page_start_ids, get_visible_rows()$id)) break
}

cat("Saved", nrow(outages), "rows to data/raw/outages_seven_days.csv\n")