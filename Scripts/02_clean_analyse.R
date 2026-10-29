# 02_clean_analyse.R
# Run from the grid-outages project.

# Install any required packages that are missing
packages <- c("dplyr", "lubridate", "arrow", "DBI", "duckdb", "httr2")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

library(dplyr)

# 1. Read the original scrape
raw <- read.csv("data/raw/outages_seven_days.csv")

# Check the row count and repeated notice IDs
cat("Raw rows:", nrow(raw), "\n")
cat("Repeated IDs:", sum(duplicated(raw$id)), "\n")

# 2. Clean blank cells and calculate outage durations
clean <- raw |>
  dplyr::mutate(dplyr::across(
    where(is.character),
    ~ na_if(trimws(.x), "")
  )) |>
  dplyr::mutate(
    start_text = sub(" - .*", "", start_end_time),
    end_text   = sub(".* - ", "", start_end_time),
    start_time = lubridate::dmy_hm(start_text, tz = "Europe/Vienna"),
    end_time   = lubridate::dmy_hm(end_text, tz = "Europe/Vienna"),
    duration_hours = as.numeric(
      difftime(end_time, start_time, units = "hours")
    )
  ) |>
  dplyr::select(-start_text, -end_text)

# Check whether any dates failed to parse
cat(
  "Unparsed periods:",
  sum(is.na(clean$start_time) | is.na(clean$end_time)),
  "\n"
)

# 3. Save clean copies as CSV and Parquet
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

write.csv(
  clean,
  "data/clean/outages_seven_days_clean.csv",
  row.names = FALSE
)

arrow::write_parquet(
  clean,
  "data/clean/outages_seven_days_clean.parquet"
)

# 4. Use DuckDB SQL to count notices by reason and type
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

reason_type_counts <- DBI::dbGetQuery(con, "
  SELECT reason, type, COUNT(*) AS notices
  FROM read_parquet('data/clean/outages_seven_days_clean.parquet')
  GROUP BY reason, type
  ORDER BY notices DESC
")

print(reason_type_counts)

# 5. Count categories and calculate their average durations
duration_summary <- DBI::dbGetQuery(con, "
  SELECT
    CASE
      WHEN reason LIKE 'Foreseen Maintenance%' THEN 'Planned'
      WHEN reason LIKE 'Failure%' THEN 'Unplanned'
      WHEN reason = 'Complementary Information' THEN 'Information'
      ELSE 'Review'
    END AS category,
    COUNT(*) AS notices,
    ROUND(AVG(duration_hours), 1) AS average_duration_hours
  FROM read_parquet('data/clean/outages_seven_days_clean.parquet')
  GROUP BY category
  ORDER BY notices DESC
")

print(duration_summary)
DBI::dbDisconnect(con, shutdown = TRUE)

# 6. Ask local Ollama to classify up to six comments
comments_sample <- sort(unique(stats::na.omit(clean$comments)))
comments_sample <- head(comments_sample, 6)

prompt <- paste(
  "Classify each comment as Planned maintenance, Unplanned event,",
  "Information/other, or Unclear. Use only the text.",
  "Return JSON with each comment and its category.",
  paste(comments_sample, collapse = "\n"),
  sep = "\n\n"
)

answer <- httr2::request("http://localhost:11434/api/generate") |>
  httr2::req_body_json(list(
    model = "llama3.2:1b",
    prompt = prompt,
    format = "json",
    stream = FALSE
  )) |>
  httr2::req_perform() |>
  httr2::resp_body_json()

cat(answer$response)