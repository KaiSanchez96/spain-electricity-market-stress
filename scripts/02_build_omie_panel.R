suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

source(file.path("R", "utils_project.R"))

paths <- project_paths(clean_dir = "data_clean", out_dir = "outputs")
RAW_DIR   <- file.path("data_raw", "omie")
CLEAN_DIR <- paths$clean_dir

stopifnot(dir.exists(RAW_DIR))

files <- list.files(
  RAW_DIR,
  pattern = "^marginalpdbc_\\d{8}\\.1$",
  full.names = TRUE
)

stopifnot(length(files) > 0)

read_omie_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  
  raw_rows_below_header <- max(length(lines) - 1L, 0L)
  
  lines <- lines[-1]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[grepl("^\\d{4};\\d{2};\\d{2};\\d{1,2};", lines)]
  
  split_lines <- strsplit(lines, ";", fixed = TRUE)
  split_lines <- split_lines[lengths(split_lines) >= 6]
  
  if (length(split_lines) == 0) {
    return(list(
      data = tibble(),
      qa = tibble(
        file_name = basename(path),
        raw_rows_below_header = raw_rows_below_header,
        parsed_rows = 0L,
        kept_rows = 0L,
        dropped_rows = raw_rows_below_header,
        duplicate_date_hour_rows = 0L,
        min_date = as.Date(NA),
        max_date = as.Date(NA)
      )
    ))
  }
  
  mat <- do.call(
    rbind,
    lapply(split_lines, function(x) {
      vals <- x[1:6]
      vals[vals == ""] <- NA_character_
      vals
    })
  )
  
  parsed_rows <- nrow(mat)
  
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- c(
    "year",
    "month",
    "day",
    "hour",
    "price_spain_eur_mwh",
    "price_portugal_eur_mwh"
  )
  
  df <- df %>%
    mutate(
      year  = suppressWarnings(as.integer(year)),
      month = suppressWarnings(as.integer(month)),
      day   = suppressWarnings(as.integer(day)),
      hour  = suppressWarnings(as.integer(hour)),
      price_spain_eur_mwh    = suppressWarnings(as.numeric(price_spain_eur_mwh)),
      price_portugal_eur_mwh = suppressWarnings(as.numeric(price_portugal_eur_mwh))
    ) %>%
    filter(
      !is.na(year),
      !is.na(month),
      !is.na(day),
      !is.na(hour),
      month >= 1, month <= 12,
      day >= 1, day <= 31,
      hour >= 1, hour <= 24
    ) %>%
    mutate(
      date = as.Date(sprintf("%04d-%02d-%02d", year, month, day)),
      datetime = as.POSIXct(date) + lubridate::hours(hour - 1),
      file_name = basename(path)
    ) %>%
    select(
      date,
      datetime,
      year,
      month,
      day,
      hour,
      price_spain_eur_mwh,
      price_portugal_eur_mwh,
      file_name
    )
  
  dup_n <- df %>%
    count(date, hour, name = "n") %>%
    filter(n > 1) %>%
    nrow()
  
  qa <- tibble(
    file_name = basename(path),
    raw_rows_below_header = raw_rows_below_header,
    parsed_rows = parsed_rows,
    kept_rows = nrow(df),
    dropped_rows = max(raw_rows_below_header - nrow(df), 0L),
    duplicate_date_hour_rows = dup_n,
    min_date = if (nrow(df) > 0) min(df$date, na.rm = TRUE) else as.Date(NA),
    max_date = if (nrow(df) > 0) max(df$date, na.rm = TRUE) else as.Date(NA)
  )
  
  list(data = df, qa = qa)
}

parsed_list <- purrr::map(files, read_omie_file)

omie_prices <- bind_rows(purrr::map(parsed_list, "data")) %>%
  arrange(date, hour, file_name)

price_file_qa <- bind_rows(purrr::map(parsed_list, "qa"))

stopifnot(nrow(omie_prices) > 0)

dup_check <- omie_prices %>%
  count(date, hour, name = "n") %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  omie_prices <- omie_prices %>%
    distinct(date, hour, .keep_all = TRUE) %>%
    arrange(date, hour)
}

omie_spain <- omie_prices %>%
  select(date, datetime, year, month, day, hour, price_spain_eur_mwh)

qa_summary <- tibble(
  n_rows = nrow(omie_prices),
  min_date = min(omie_prices$date, na.rm = TRUE),
  max_date = max(omie_prices$date, na.rm = TRUE),
  n_days = n_distinct(omie_prices$date),
  min_hour = min(omie_prices$hour, na.rm = TRUE),
  max_hour = max(omie_prices$hour, na.rm = TRUE)
)

rows_per_day <- omie_prices %>%
  count(date, name = "n_rows_day")

missing_summary <- omie_prices %>%
  summarise(
    missing_spain = sum(is.na(price_spain_eur_mwh)),
    missing_portugal = sum(is.na(price_portugal_eur_mwh))
  )

price_pipeline_qa <- tibble(
  metric = c(
    "Raw price files processed",
    "Parsed raw rows below header",
    "Rows kept after parsing filters",
    "Rows dropped during parsing",
    "Dropped row share",
    "Duplicate date-hour rows before deduplication",
    "Duplicate date-hour rows after deduplication",
    "Days in price panel",
    "Days with 23 hourly rows",
    "Days with 24 hourly rows"
  ),
  value = c(
    nrow(price_file_qa),
    sum(price_file_qa$raw_rows_below_header, na.rm = TRUE),
    nrow(omie_prices),
    sum(price_file_qa$dropped_rows, na.rm = TRUE),
    round(sum(price_file_qa$dropped_rows, na.rm = TRUE) / pmax(sum(price_file_qa$raw_rows_below_header, na.rm = TRUE), 1), 6),
    sum(price_file_qa$duplicate_date_hour_rows, na.rm = TRUE),
    0,
    n_distinct(omie_prices$date),
    sum(rows_per_day$n_rows_day == 23, na.rm = TRUE),
    sum(rows_per_day$n_rows_day == 24, na.rm = TRUE)
  )
)

stopifnot(all(!is.na(omie_prices$date)))
stopifnot(all(omie_prices$hour >= 1 & omie_prices$hour <= 24, na.rm = TRUE))
stopifnot(all(!is.na(omie_prices$price_spain_eur_mwh)))
stopifnot(nrow(dup_check <- omie_prices %>% count(date, hour, name = "n") %>% filter(n > 1)) == 0)

print(qa_summary)
print(table(rows_per_day$n_rows_day))
print(dup_check)
print(missing_summary)

write_csv(omie_prices, file.path(CLEAN_DIR, "omie_day_ahead_prices_spain_portugal.csv"))
saveRDS(omie_prices, file.path(CLEAN_DIR, "omie_day_ahead_prices_spain_portugal.rds"), compress = TRUE)

write_csv(omie_spain, file.path(CLEAN_DIR, "omie_day_ahead_prices_spain.csv"))
saveRDS(omie_spain, file.path(CLEAN_DIR, "omie_day_ahead_prices_spain.rds"), compress = TRUE)

write_csv(rows_per_day, file.path(CLEAN_DIR, "omie_rows_per_day_check.csv"))
write_csv(qa_summary, file.path(CLEAN_DIR, "omie_qa_summary.csv"))
write_csv(missing_summary, file.path(CLEAN_DIR, "omie_missing_summary.csv"))
write_csv(price_file_qa, file.path(CLEAN_DIR, "omie_price_file_qa.csv"))
write_csv(price_pipeline_qa, file.path(CLEAN_DIR, "omie_price_pipeline_qa.csv"))

cat("DONE.\n")
cat("Saved clean outputs in:", CLEAN_DIR, "\n")
cat("- omie_day_ahead_prices_spain_portugal.csv/.rds\n")
cat("- omie_day_ahead_prices_spain.csv/.rds\n")
cat("- omie_rows_per_day_check.csv\n")
cat("- omie_qa_summary.csv\n")
cat("- omie_missing_summary.csv\n")
cat("- omie_price_file_qa.csv\n")
cat("- omie_price_pipeline_qa.csv\n")
