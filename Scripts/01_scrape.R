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
readline("Select ONE region and ONE day in Chrome; wait for the table, then press Enter here: ")

# 5. Extract the displayed HTML table
page <- rvest::read_html(browser$get_page_source())
table_node <- rvest::html_element(page, "table")
if (inherits(table_node, "xml_missing")) stop("No HTML table found on the page.")
raw_table <- rvest::html_table(table_node, fill = TRUE)
if (ncol(raw_table) < 9 || nrow(raw_table) < 2) {
  stop("The expected outage table has not loaded yet.")
}

# 6. Remove header/empty columns and name fields
outages <- raw_table[-1, 2:9, drop = FALSE]
names(outages) <- c(
  "affected_areas", "asset_name", "asset_code", "type",
  "status", "start_end_time", "reason", "comments"
)

# 7. Save to CSV (relative to your RStudio project)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
output_file <- "data/raw/outages_one_day.csv"
write.csv(outages, output_file, row.names = FALSE)

cat("Saved", nrow(outages), "records to", output_file, "\n")
print(utils::head(outages, 3))

# Note: This saves only the currently displayed table page; pagination comes next.
