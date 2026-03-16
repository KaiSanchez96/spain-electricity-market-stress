suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
})

source(file.path("R", "utils_project.R"))

# ==========================================================
# 0) Paths
# ==========================================================
CLEAN_DIR <- "data_clean"

curves_path <- file.path(CLEAN_DIR, "omie_day_ahead_curves_es_mi_cleared.rds")
stopifnot(file.exists(curves_path))

dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# 1) Load cleaned ES + MI cleared curves
# ==========================================================
omie_curves_es_mi_cleared <- readRDS(curves_path)

assert_required_columns(
  omie_curves_es_mi_cleared,
  c(
    "date", "datetime", "hour", "period_type", "quarter", "country",
    "offer_type", "volume_mw_mwh", "price_eur_mwh", "offer_status"
  ),
  "omie_curves_es_mi_cleared"
)

stopifnot(nrow(omie_curves_es_mi_cleared) > 0)
stopifnot(all(omie_curves_es_mi_cleared$country %in% c("ES", "MI")))
stopifnot(all(omie_curves_es_mi_cleared$offer_status == "C"))

print(count(omie_curves_es_mi_cleared, period_type))

# ==========================================================
# 2) Standardize and keep only needed columns
# ==========================================================
curves_work <- omie_curves_es_mi_cleared %>%
  transmute(
    date,
    datetime,
    hour,
    period_type,
    quarter,
    country,
    offer_type,
    volume_mw_mwh = as.numeric(volume_mw_mwh),
    price_eur_mwh = as.numeric(price_eur_mwh)
  ) %>%
  filter(
    !is.na(date),
    !is.na(datetime),
    !is.na(hour),
    hour >= 1,
    hour <= 24,
    offer_type %in% c("C", "V")
  )

# ==========================================================
# 3) Hourly summary by offer side
# ==========================================================
curve_hourly_summary <- curves_work %>%
  group_by(date, datetime, hour, offer_type) %>%
  summarise(
    n_steps = n(),
    n_quarter_rows = sum(period_type == "quarter_hourly_new", na.rm = TRUE),
    total_volume_mw_mwh = sum(volume_mw_mwh, na.rm = TRUE),
    avg_price_eur_mwh = {
      valid_idx <- !is.na(price_eur_mwh) & !is.na(volume_mw_mwh)
      valid_weights <- volume_mw_mwh[valid_idx]
      if (length(valid_weights) > 0 && sum(valid_weights, na.rm = TRUE) > 0) {
        weighted.mean(price_eur_mwh[valid_idx], valid_weights, na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    simple_avg_price_eur_mwh = {
      valid_prices <- price_eur_mwh[!is.na(price_eur_mwh)]
      if (length(valid_prices) > 0) mean(valid_prices) else NA_real_
    },
    min_price_eur_mwh = {
      valid_prices <- price_eur_mwh[!is.na(price_eur_mwh)]
      if (length(valid_prices) > 0) min(valid_prices) else NA_real_
    },
    max_price_eur_mwh = {
      valid_prices <- price_eur_mwh[!is.na(price_eur_mwh)]
      if (length(valid_prices) > 0) max(valid_prices) else NA_real_
    },
    share_quarter_rows = mean(period_type == "quarter_hourly_new", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg_price_eur_mwh = ifelse(is.nan(avg_price_eur_mwh), NA_real_, avg_price_eur_mwh),
    simple_avg_price_eur_mwh = ifelse(is.nan(simple_avg_price_eur_mwh), NA_real_, simple_avg_price_eur_mwh),
    min_price_eur_mwh = ifelse(is.infinite(min_price_eur_mwh), NA_real_, min_price_eur_mwh),
    max_price_eur_mwh = ifelse(is.infinite(max_price_eur_mwh), NA_real_, max_price_eur_mwh)
  )

# ==========================================================
# 4) Pivot to wide format
# ==========================================================
curve_hourly_wide <- curve_hourly_summary %>%
  pivot_wider(
    names_from = offer_type,
    values_from = c(
      n_steps,
      n_quarter_rows,
      total_volume_mw_mwh,
      avg_price_eur_mwh,
      simple_avg_price_eur_mwh,
      min_price_eur_mwh,
      max_price_eur_mwh,
      share_quarter_rows
    ),
    names_sep = "_"
  ) %>%
  arrange(date, hour)

# ==========================================================
# 5) Derived hourly market metrics
# ==========================================================
curve_hourly_wide <- curve_hourly_wide %>%
  mutate(
    across(
      c(
        avg_price_eur_mwh_C, avg_price_eur_mwh_V,
        simple_avg_price_eur_mwh_C, simple_avg_price_eur_mwh_V
      ),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    ),
    total_steps = coalesce(n_steps_C, 0) + coalesce(n_steps_V, 0),
    total_quarter_rows = coalesce(n_quarter_rows_C, 0) + coalesce(n_quarter_rows_V, 0),
    quarter_row_share = total_quarter_rows / pmax(total_steps, 1),
    buy_sell_volume_gap = coalesce(total_volume_mw_mwh_C, 0) - coalesce(total_volume_mw_mwh_V, 0),
    buy_sell_avg_price_gap = avg_price_eur_mwh_C - avg_price_eur_mwh_V,
    buy_sell_max_price_gap = max_price_eur_mwh_C - max_price_eur_mwh_V
  )

# ==========================================================
# 6) QA tables
# ==========================================================
qa_summary_curves_hourly <- tibble(
  n_rows = nrow(curve_hourly_wide),
  min_date = min(curve_hourly_wide$date, na.rm = TRUE),
  max_date = max(curve_hourly_wide$date, na.rm = TRUE),
  n_days = dplyr::n_distinct(curve_hourly_wide$date),
  min_hour = min(curve_hourly_wide$hour, na.rm = TRUE),
  max_hour = max(curve_hourly_wide$hour, na.rm = TRUE)
)

rows_per_day_hourly <- curve_hourly_wide %>%
  count(date, name = "n_rows_day")

dup_check_hourly <- curve_hourly_wide %>%
  count(date, hour, name = "n") %>%
  filter(n > 1)

missing_summary_hourly <- curve_hourly_wide %>%
  summarise(
    missing_total_volume_buy = sum(is.na(total_volume_mw_mwh_C)),
    missing_total_volume_sell = sum(is.na(total_volume_mw_mwh_V)),
    missing_avg_price_buy = sum(is.na(avg_price_eur_mwh_C)),
    missing_avg_price_sell = sum(is.na(avg_price_eur_mwh_V)),
    missing_quarter_share = sum(is.na(quarter_row_share))
  )

period_type_summary <- curves_work %>%
  count(period_type)

period_mix_summary <- curves_work %>%
  count(date, period_type) %>%
  tidyr::pivot_wider(names_from = period_type, values_from = n, values_fill = 0) %>%
  mutate(
    has_quarter_hourly = if ("quarter_hourly_new" %in% names(.)) quarter_hourly_new > 0 else FALSE
  ) %>%
  summarise(
    days_with_quarter_hourly = sum(has_quarter_hourly, na.rm = TRUE),
    total_days = n(),
    share_days_with_quarter_hourly = days_with_quarter_hourly / pmax(total_days, 1)
  )

curve_hourly_feature_qa <- tibble(
  metric = c(
    "Hourly rows in curve summary",
    "Days in curve summary",
    "Duplicate date-hour rows",
    "Days with 23 hourly rows",
    "Days with 24 hourly rows",
    "Days with quarter-hourly source rows",
    "Share of days with quarter-hourly source rows",
    "Average quarter-row share within hourly summaries"
  ),
  value = c(
    nrow(curve_hourly_wide),
    n_distinct(curve_hourly_wide$date),
    nrow(dup_check_hourly),
    sum(rows_per_day_hourly$n_rows_day == 23, na.rm = TRUE),
    sum(rows_per_day_hourly$n_rows_day == 24, na.rm = TRUE),
    period_mix_summary$days_with_quarter_hourly,
    round(period_mix_summary$share_days_with_quarter_hourly, 6),
    round(mean(curve_hourly_wide$quarter_row_share, na.rm = TRUE), 6)
  )
)

print(qa_summary_curves_hourly)
print(table(rows_per_day_hourly$n_rows_day))
print(dup_check_hourly)
print(missing_summary_hourly)
print(period_type_summary)
print(period_mix_summary)

# ==========================================================
# 7) Save outputs
# ==========================================================
write_csv(curve_hourly_summary, file.path(CLEAN_DIR, "omie_curves_hourly_summary_long.csv"))
write_csv(curve_hourly_wide, file.path(CLEAN_DIR, "omie_curves_hourly_summary_wide.csv"))

saveRDS(curve_hourly_summary, file.path(CLEAN_DIR, "omie_curves_hourly_summary_long.rds"), compress = TRUE)
saveRDS(curve_hourly_wide, file.path(CLEAN_DIR, "omie_curves_hourly_summary_wide.rds"), compress = TRUE)

write_csv(qa_summary_curves_hourly, file.path(CLEAN_DIR, "omie_curves_hourly_summary_qa.csv"))
write_csv(rows_per_day_hourly, file.path(CLEAN_DIR, "omie_curves_hourly_rows_per_day_check.csv"))
write_csv(missing_summary_hourly, file.path(CLEAN_DIR, "omie_curves_hourly_missing_summary.csv"))
write_csv(period_type_summary, file.path(CLEAN_DIR, "omie_curves_period_type_summary.csv"))
write_csv(period_mix_summary, file.path(CLEAN_DIR, "omie_curves_period_mix_summary.csv"))
write_csv(curve_hourly_feature_qa, file.path(CLEAN_DIR, "omie_curves_hourly_feature_qa.csv"))

cat("DONE.\n")
cat("Saved:\n")
cat("- data_clean/omie_curves_hourly_summary_long.csv/.rds\n")
cat("- data_clean/omie_curves_hourly_summary_wide.csv/.rds\n")
cat("- data_clean/omie_curves_hourly_summary_qa.csv\n")
cat("- data_clean/omie_curves_hourly_rows_per_day_check.csv\n")
cat("- data_clean/omie_curves_hourly_missing_summary.csv\n")
cat("- data_clean/omie_curves_period_type_summary.csv\n")
cat("- data_clean/omie_curves_period_mix_summary.csv\n")
cat("- data_clean/omie_curves_hourly_feature_qa.csv\n")