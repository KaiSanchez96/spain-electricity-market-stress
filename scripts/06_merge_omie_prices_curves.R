suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

source(file.path("R", "utils_project.R"))

# ==========================================================
# 0) Paths
# ==========================================================
paths <- project_paths(clean_dir = "data_clean", out_dir = "outputs")
CLEAN_DIR <- paths$clean_dir

prices_path <- file.path(CLEAN_DIR, "omie_day_ahead_prices_spain.rds")
curves_path <- file.path(CLEAN_DIR, "omie_curves_hourly_summary_wide.rds")

stopifnot(file.exists(prices_path))
stopifnot(file.exists(curves_path))

# ==========================================================
# 1) Load inputs
# ==========================================================
omie_prices <- readRDS(prices_path)
curve_hourly_wide <- readRDS(curves_path)

stopifnot(nrow(omie_prices) > 0)
stopifnot(nrow(curve_hourly_wide) > 0)

assert_required_columns(
  omie_prices,
  c("date", "datetime", "hour", "price_spain_eur_mwh"),
  "omie_prices"
)

assert_required_columns(
  curve_hourly_wide,
  c(
    "date", "datetime", "hour",
    "n_steps_C", "n_steps_V",
    "total_volume_mw_mwh_C", "total_volume_mw_mwh_V",
    "avg_price_eur_mwh_C", "avg_price_eur_mwh_V",
    "buy_sell_volume_gap",
    "total_steps"
  ),
  "curve_hourly_wide"
)

# ==========================================================
# 2) Keep only needed columns and de-duplicate
# ==========================================================
prices_work <- omie_prices %>%
  transmute(
    date,
    datetime,
    hour,
    price_spain_eur_mwh
  ) %>%
  distinct()

curves_work <- curve_hourly_wide %>%
  transmute(
    date,
    datetime,
    hour,
    n_steps_C,
    n_steps_V,
    total_steps,
    total_volume_mw_mwh_C,
    total_volume_mw_mwh_V,
    avg_price_eur_mwh_C,
    avg_price_eur_mwh_V,
    min_price_eur_mwh_C,
    min_price_eur_mwh_V,
    max_price_eur_mwh_C,
    max_price_eur_mwh_V,
    quarter_row_share,
    buy_sell_volume_gap,
    buy_sell_avg_price_gap,
    buy_sell_max_price_gap
  ) %>%
  distinct()

# ==========================================================
# 3) Coverage diagnostics before merge
# ==========================================================
prices_dates <- prices_work %>% distinct(date)
curves_dates <- curves_work %>% distinct(date)

missing_in_curves_from_prices <- prices_dates %>%
  anti_join(curves_dates, by = "date") %>%
  arrange(date)

missing_in_prices_from_curves <- curves_dates %>%
  anti_join(prices_dates, by = "date") %>%
  arrange(date)

write_csv(
  missing_in_curves_from_prices,
  file.path(CLEAN_DIR, "missing_in_curves_from_prices.csv")
)

write_csv(
  missing_in_prices_from_curves,
  file.path(CLEAN_DIR, "missing_in_prices_from_curves.csv")
)

# ==========================================================
# 4) Merge hourly prices with hourly curve summaries
# ==========================================================
market_panel <- prices_work %>%
  inner_join(curves_work, by = c("date", "datetime", "hour")) %>%
  arrange(date, hour)

stopifnot(nrow(market_panel) > 0)
stopifnot(all(!is.na(market_panel$date)))
stopifnot(all(!is.na(market_panel$price_spain_eur_mwh)))
stopifnot(all(market_panel$hour >= 1 & market_panel$hour <= 24, na.rm = TRUE))

# ==========================================================
# 5) Merge QA
# ==========================================================
n_rows_prices_hourly <- nrow(prices_work)
n_rows_curves_hourly <- nrow(curves_work)
n_rows_merged_hourly <- nrow(market_panel)

n_days_prices <- dplyr::n_distinct(prices_work$date)
n_days_curves <- dplyr::n_distinct(curves_work$date)
n_days_merged <- dplyr::n_distinct(market_panel$date)

coverage_retained_vs_prices <- ifelse(n_days_prices > 0, n_days_merged / n_days_prices, NA_real_)
coverage_retained_vs_curves <- ifelse(n_days_curves > 0, n_days_merged / n_days_curves, NA_real_)

merge_pipeline_qa <- tibble(
  metric = c(
    "Hourly rows in prices",
    "Hourly rows in curves",
    "Hourly rows in merged panel",
    "Days in prices",
    "Days in curves",
    "Days in merged panel",
    "Daily coverage retained vs prices",
    "Daily coverage retained vs curves"
  ),
  value = c(
    n_rows_prices_hourly,
    n_rows_curves_hourly,
    n_rows_merged_hourly,
    n_days_prices,
    n_days_curves,
    n_days_merged,
    round(coverage_retained_vs_prices, 6),
    round(coverage_retained_vs_curves, 6)
  )
)

qa_summary_merge <- tibble(
  n_rows = nrow(market_panel),
  min_date = min(market_panel$date, na.rm = TRUE),
  max_date = max(market_panel$date, na.rm = TRUE),
  n_days = n_distinct(market_panel$date),
  min_hour = min(market_panel$hour, na.rm = TRUE),
  max_hour = max(market_panel$hour, na.rm = TRUE)
)

rows_per_day_merge <- market_panel %>%
  count(date, name = "n_rows_day")

dup_check_merge <- market_panel %>%
  count(date, hour, name = "n") %>%
  filter(n > 1)

missing_summary_merge <- market_panel %>%
  summarise(
    missing_price_spain = sum(is.na(price_spain_eur_mwh)),
    missing_total_volume_buy = sum(is.na(total_volume_mw_mwh_C)),
    missing_total_volume_sell = sum(is.na(total_volume_mw_mwh_V)),
    missing_avg_price_buy = sum(is.na(avg_price_eur_mwh_C)),
    missing_avg_price_sell = sum(is.na(avg_price_eur_mwh_V)),
    missing_quarter_row_share = sum(is.na(quarter_row_share))
  )

# ==========================================================
# 6) Derived hourly features
# ==========================================================
high_price_threshold <- quantile(market_panel$price_spain_eur_mwh, 0.95, na.rm = TRUE)
low_price_threshold  <- quantile(market_panel$price_spain_eur_mwh, 0.05, na.rm = TRUE)

market_panel <- market_panel %>%
  mutate(
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    day = as.integer(format(date, "%d")),
    total_matched_volume = coalesce(total_volume_mw_mwh_C, 0) + coalesce(total_volume_mw_mwh_V, 0),
    price_vs_buy_avg_gap = price_spain_eur_mwh - avg_price_eur_mwh_C,
    price_vs_sell_avg_gap = price_spain_eur_mwh - avg_price_eur_mwh_V,
    abs_buy_sell_volume_gap = abs(buy_sell_volume_gap),
    high_price_flag = price_spain_eur_mwh >= high_price_threshold,
    low_price_flag  = price_spain_eur_mwh <= low_price_threshold,
    total_steps = coalesce(total_steps, 0),
    total_steps_from_sides = coalesce(n_steps_C, 0) + coalesce(n_steps_V, 0)
  )

# ==========================================================
# 7) Daily aggregation
# ==========================================================
market_daily <- market_panel %>%
  group_by(date, year, month) %>%
  summarise(
    avg_price_spain_eur_mwh = mean(price_spain_eur_mwh, na.rm = TRUE),
    min_price_spain_eur_mwh = min(price_spain_eur_mwh, na.rm = TRUE),
    max_price_spain_eur_mwh = max(price_spain_eur_mwh, na.rm = TRUE),
    daily_price_range = max_price_spain_eur_mwh - min_price_spain_eur_mwh,
    avg_total_matched_volume = mean(total_matched_volume, na.rm = TRUE),
    avg_buy_sell_volume_gap = mean(buy_sell_volume_gap, na.rm = TRUE),
    avg_total_steps = mean(total_steps, na.rm = TRUE),
    avg_total_steps_from_sides = mean(total_steps_from_sides, na.rm = TRUE),
    avg_quarter_row_share = mean(quarter_row_share, na.rm = TRUE),
    n_high_price_hours = sum(high_price_flag, na.rm = TRUE),
    n_low_price_hours = sum(low_price_flag, na.rm = TRUE),
    .groups = "drop"
  )

# ==========================================================
# 8) Save outputs
# ==========================================================
write_csv(market_panel, file.path(CLEAN_DIR, "omie_market_panel_hourly.csv"))
saveRDS(market_panel, file.path(CLEAN_DIR, "omie_market_panel_hourly.rds"), compress = TRUE)

write_csv(market_daily, file.path(CLEAN_DIR, "omie_market_panel_daily.csv"))
saveRDS(market_daily, file.path(CLEAN_DIR, "omie_market_panel_daily.rds"), compress = TRUE)

write_csv(qa_summary_merge, file.path(CLEAN_DIR, "omie_market_panel_merge_qa.csv"))
write_csv(rows_per_day_merge, file.path(CLEAN_DIR, "omie_market_panel_rows_per_day_check.csv"))
write_csv(missing_summary_merge, file.path(CLEAN_DIR, "omie_market_panel_missing_summary.csv"))
write_csv(merge_pipeline_qa, file.path(CLEAN_DIR, "omie_merge_pipeline_qa.csv"))

cat("DONE.\n")
cat("Saved:\n")
cat("- data_clean/omie_market_panel_hourly.csv/.rds\n")
cat("- data_clean/omie_market_panel_daily.csv/.rds\n")
cat("- data_clean/omie_market_panel_merge_qa.csv\n")
cat("- data_clean/omie_market_panel_rows_per_day_check.csv\n")
cat("- data_clean/omie_market_panel_missing_summary.csv\n")
cat("- data_clean/missing_in_curves_from_prices.csv\n")
cat("- data_clean/missing_in_prices_from_curves.csv\n")
cat("- data_clean/omie_merge_pipeline_qa.csv\n")
