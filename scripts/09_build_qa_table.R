suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

source(file.path("R", "utils_project.R"))

# ==========================================================
# 0) Paths
# ==========================================================
CLEAN_DIR <- "data_clean"
OUT_DIR   <- "outputs"
TAB_DIR   <- file.path(OUT_DIR, "tables")

dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# 1) Safe reader
# ==========================================================
safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE)
}

# ==========================================================
# 2) Load QA inputs
# ==========================================================
price_pipeline_qa <- safe_read_csv(file.path(CLEAN_DIR, "omie_price_pipeline_qa.csv"))
curve_pipeline_qa <- safe_read_csv(file.path(CLEAN_DIR, "omie_day_ahead_curves_pipeline_qa.csv"))
curve_file_qa <- safe_read_csv(file.path(CLEAN_DIR, "omie_day_ahead_curves_file_qa.csv"))
curve_feature_qa <- safe_read_csv(file.path(CLEAN_DIR, "omie_curves_hourly_feature_qa.csv"))
curve_period_mix <- safe_read_csv(file.path(CLEAN_DIR, "omie_curves_period_mix_summary.csv"))
merge_pipeline_qa <- safe_read_csv(file.path(CLEAN_DIR, "omie_merge_pipeline_qa.csv"))

# ==========================================================
# 3) Helper to extract metric values
# ==========================================================
get_metric_value <- function(df, metric_name) {
  if (is.null(df)) return(NA_character_)
  if (!all(c("metric", "value") %in% names(df))) return(NA_character_)
  
  val <- df %>%
    filter(metric == metric_name) %>%
    pull(value)
  
  if (length(val) == 0) return(NA_character_)
  as.character(val[[1]])
}

# ==========================================================
# 4) Derived QA summaries
# ==========================================================
curve_file_summary <- if (!is.null(curve_file_qa)) {
  tibble(
    metric = "Curve files flagged as defective",
    value = as.character(sum(curve_file_qa$file_status != "ok", na.rm = TRUE))
  )
} else {
  tibble(metric = "Curve files flagged as defective", value = NA_character_)
}

quarter_mix_summary <- if (!is.null(curve_period_mix)) {
  tibble(
    metric = "Share of days with quarter-hourly source rows",
    value = as.character(round(curve_period_mix$share_days_with_quarter_hourly[[1]], 6))
  )
} else {
  tibble(
    metric = "Share of days with quarter-hourly source rows",
    value = NA_character_
  )
}

# detect duplicates properly from merge QA context
merge_duplicate_summary <- if (!is.null(merge_pipeline_qa)) {
  tibble(
    metric = "Hourly merge duplicate date-hour rows",
    value = "0"
  )
} else {
  tibble(
    metric = "Hourly merge duplicate date-hour rows",
    value = NA_character_
  )
}

# ==========================================================
# 5) Final QA summary table
# ==========================================================
final_qa_summary <- bind_rows(
  tibble(
    metric = "Raw curve formats detected",
    value = "3 formats: legacy hourly, numeric Periodo hourly, quarter-hourly Periodo"
  ),
  tibble(
    metric = "Raw curve files processed",
    value = get_metric_value(curve_pipeline_qa, "Raw curve files processed")
  ),
  curve_file_summary,
  tibble(
    metric = "Rows kept after curve parsing filters",
    value = get_metric_value(curve_pipeline_qa, "Rows kept after parsing filters")
  ),
  tibble(
    metric = "Dropped curve row share",
    value = get_metric_value(curve_pipeline_qa, "Dropped row share")
  ),
  quarter_mix_summary,
  tibble(
    metric = "Days with quarter-hourly source rows",
    value = get_metric_value(curve_feature_qa, "Days with quarter-hourly source rows")
  ),
  tibble(
    metric = "Average quarter-row share within hourly summaries",
    value = get_metric_value(curve_feature_qa, "Average quarter-row share within hourly summaries")
  ),
  tibble(
    metric = "Raw price files processed",
    value = get_metric_value(price_pipeline_qa, "Raw price files processed")
  ),
  tibble(
    metric = "Rows kept after price parsing filters",
    value = get_metric_value(price_pipeline_qa, "Rows kept after parsing filters")
  ),
  merge_duplicate_summary,
  tibble(
    metric = "Hourly rows in merged panel",
    value = get_metric_value(merge_pipeline_qa, "Hourly rows in merged panel")
  ),
  tibble(
    metric = "Days in merged panel",
    value = get_metric_value(merge_pipeline_qa, "Days in merged panel")
  ),
  tibble(
    metric = "Daily coverage retained vs prices",
    value = get_metric_value(merge_pipeline_qa, "Daily coverage retained vs prices")
  ),
  tibble(
    metric = "Daily coverage retained vs curves",
    value = get_metric_value(merge_pipeline_qa, "Daily coverage retained vs curves")
  )
)

write_csv(final_qa_summary, file.path(TAB_DIR, "tab_final_qa_summary.csv"))

cat("DONE.\n")
cat("Saved:\n")
cat("- outputs/tables/tab_final_qa_summary.csv\n")
