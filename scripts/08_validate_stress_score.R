suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(scales)
})

source(file.path("R", "utils_project.R"))

# ==========================================================
# 0) Local helper for saving plots
# ==========================================================
save_plot_local <- function(plot_obj, path, width = 9, height = 5, dpi = 300) {
  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
}

# ==========================================================
# 1) Paths
# ==========================================================
CLEAN_DIR <- "data_clean"
OUT_DIR   <- "outputs"
FIG_DIR   <- file.path(OUT_DIR, "figures")
TAB_DIR   <- file.path(OUT_DIR, "tables")

dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

daily_path <- file.path(CLEAN_DIR, "omie_market_panel_daily_enriched.rds")
stopifnot(file.exists(daily_path))

# ==========================================================
# 2) Load input
# ==========================================================
market_daily <- readRDS(daily_path)

assert_required_columns(
  market_daily,
  c(
    "date",
    "avg_price_spain_eur_mwh",
    "daily_price_range",
    "n_high_price_hours",
    "avg_total_steps",
    "score_core",
    "score_extended",
    "z_avg_price",
    "z_daily_range",
    "z_high_price_hours",
    "z_steps"
  ),
  "market_daily"
)

stopifnot(nrow(market_daily) > 0)

market_daily <- market_daily %>%
  mutate(score_base = score_extended)

# ==========================================================
# 3) Alternative score schemes
# ==========================================================
market_daily <- market_daily %>%
  mutate(
    score_no_steps =
      0.40 * z_avg_price +
      0.40 * z_daily_range +
      0.20 * z_high_price_hours,
    
    score_price_range_only =
      0.50 * z_avg_price +
      0.50 * z_daily_range,
    
    score_price_only = z_avg_price,
    score_range_only = z_daily_range
  )

score_variants <- list(
  base = market_daily$score_base,
  core = market_daily$score_core,
  no_steps = market_daily$score_no_steps,
  price_range_only = market_daily$score_price_range_only
)

flag_top_n <- function(x, n = NULL, share = 0.05) {
  if (is.null(n)) {
    n <- max(1, round(length(x) * share))
  }
  rank(-x, ties.method = "first") <= n
}

top_n_overlap <- function(base_scores, alt_scores, n = 20) {
  base_flag <- flag_top_n(base_scores, n = n)
  alt_flag  <- flag_top_n(alt_scores, n = n)
  
  base_idx <- which(base_flag)
  alt_idx  <- which(alt_flag)
  
  length(intersect(base_idx, alt_idx)) / n
}

weight_sensitivity <- tibble(
  variant = names(score_variants),
  weights = c(
    "35/35/20/10",
    "40/40/20/0",
    "40/40/20/0",
    "50/50/0/0"
  ),
  correlation_with_base = sapply(
    score_variants,
    function(x) cor(market_daily$score_base, x, use = "complete.obs")
  ),
  overlap_top20 = sapply(
    score_variants,
    function(x) top_n_overlap(market_daily$score_base, x, n = 20)
  )
)

write_csv(weight_sensitivity, file.path(TAB_DIR, "tab_weight_sensitivity_summary.csv"))

# ==========================================================
# 4) Internal event definition
# ==========================================================
price_cut <- quantile(market_daily$avg_price_spain_eur_mwh, 0.95, na.rm = TRUE)
range_cut <- quantile(market_daily$daily_price_range, 0.95, na.rm = TRUE)
hours_cut <- quantile(market_daily$n_high_price_hours, 0.95, na.rm = TRUE)

validated_df <- market_daily %>%
  mutate(
    internal_stress_event =
      avg_price_spain_eur_mwh >= price_cut |
      daily_price_range >= range_cut |
      n_high_price_hours >= hours_cut
  )

# ==========================================================
# 5) Benchmark comparison
# ==========================================================
n_flag <- max(1, round(nrow(validated_df) * 0.05))
n_internal_events <- sum(validated_df$internal_stress_event, na.rm = TRUE)

benchmark_flags <- tibble(
  date = validated_df$date,
  flag_price = flag_top_n(validated_df$avg_price_spain_eur_mwh, n = n_flag),
  flag_range = flag_top_n(validated_df$daily_price_range, n = n_flag),
  flag_extended = flag_top_n(validated_df$score_base, n = n_flag),
  flag_core = flag_top_n(validated_df$score_core, n = n_flag),
  flag_no_steps = flag_top_n(validated_df$score_no_steps, n = n_flag),
  flag_price_range = flag_top_n(validated_df$score_price_range_only, n = n_flag)
)

benchmark_eval <- tibble(
  metric = c(
    "Average daily price",
    "Daily range",
    "Composite score (extended)",
    "Composite score (core)",
    "Composite without steps",
    "Price-range combination"
  ),
  flag = list(
    benchmark_flags$flag_price,
    benchmark_flags$flag_range,
    benchmark_flags$flag_extended,
    benchmark_flags$flag_core,
    benchmark_flags$flag_no_steps,
    benchmark_flags$flag_price_range
  )
) %>%
  rowwise() %>%
  mutate(
    flagged_days = sum(flag, na.rm = TRUE),
    captures_internal_stress = sum(flag & validated_df$internal_stress_event, na.rm = TRUE),
    precision_internal_stress = ifelse(flagged_days > 0, captures_internal_stress / flagged_days, NA_real_),
    recall_internal_stress = ifelse(n_internal_events > 0, captures_internal_stress / n_internal_events, NA_real_)
  ) %>%
  ungroup() %>%
  select(-flag)

write_csv(benchmark_eval, file.path(TAB_DIR, "tab_benchmark_comparison.csv"))

# ==========================================================
# 6) Benchmark recall figure
# ==========================================================
p14 <- ggplot(
  benchmark_eval,
  aes(x = reorder(metric, recall_internal_stress), y = recall_internal_stress)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Benchmark comparison: recall of internally defined stress events",
    x = NULL,
    y = "Recall"
  ) +
  theme_minimal()

save_plot_local(
  p14,
  file.path(FIG_DIR, "14_benchmark_recall_comparison.png"),
  width = 9,
  height = 5
)

# ==========================================================
# 7) Incremental value analysis
#    IMPORTANT: use the SAME flag logic as benchmark
# ==========================================================
comp_only_vs_price_dates <- benchmark_flags$date[benchmark_flags$flag_extended & !benchmark_flags$flag_price]
price_only_vs_comp_dates <- benchmark_flags$date[benchmark_flags$flag_price & !benchmark_flags$flag_extended]

comp_only_vs_range_dates <- benchmark_flags$date[benchmark_flags$flag_extended & !benchmark_flags$flag_range]
range_only_vs_comp_dates <- benchmark_flags$date[benchmark_flags$flag_range & !benchmark_flags$flag_extended]

incremental_summary <- tibble(
  comparison = c(
    "Composite-only vs price",
    "Price-only vs composite",
    "Composite-only vs range",
    "Range-only vs composite"
  ),
  n_days = c(
    length(comp_only_vs_price_dates),
    length(price_only_vs_comp_dates),
    length(comp_only_vs_range_dates),
    length(range_only_vs_comp_dates)
  )
)

write_csv(incremental_summary, file.path(TAB_DIR, "tab_incremental_value_summary.csv"))

p15 <- ggplot(incremental_summary, aes(x = comparison, y = n_days)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Incremental value: uniquely flagged days by KPI comparison",
    x = NULL,
    y = "Number of unique days"
  ) +
  theme_minimal()

save_plot_local(
  p15,
  file.path(FIG_DIR, "15_incremental_value_unique_days.png"),
  width = 9,
  height = 5
)

# ==========================================================
# 8) Profile summaries for unique groups
# ==========================================================
make_profile <- function(df, dates, label) {
  df %>%
    filter(date %in% dates) %>%
    summarise(
      group = label,
      n_days = n(),
      avg_price_mean = mean(avg_price_spain_eur_mwh, na.rm = TRUE),
      daily_range_mean = mean(daily_price_range, na.rm = TRUE),
      high_price_hours_mean = mean(n_high_price_hours, na.rm = TRUE),
      score_mean = mean(score_base, na.rm = TRUE)
    )
}

profile_summary <- bind_rows(
  make_profile(validated_df, comp_only_vs_price_dates, "Composite-only vs price"),
  make_profile(validated_df, price_only_vs_comp_dates, "Price-only vs composite"),
  make_profile(validated_df, comp_only_vs_range_dates, "Composite-only vs range"),
  make_profile(validated_df, range_only_vs_comp_dates, "Range-only vs composite")
)

write_csv(profile_summary, file.path(TAB_DIR, "tab_unique_day_profile_summary.csv"))

comp_vs_price_df <- validated_df %>% filter(date %in% comp_only_vs_price_dates)
price_vs_comp_df <- validated_df %>% filter(date %in% price_only_vs_comp_dates)
range_vs_comp_df <- validated_df %>% filter(date %in% range_only_vs_comp_dates)

write_csv(comp_vs_price_df, file.path(TAB_DIR, "tab_composite_unique_vs_price.csv"))
write_csv(price_vs_comp_df, file.path(TAB_DIR, "tab_price_unique_vs_composite.csv"))
write_csv(range_vs_comp_df, file.path(TAB_DIR, "tab_range_unique_vs_composite.csv"))

# ==========================================================
# 9) Alert buckets
# ==========================================================
validated_df <- validated_df %>%
  mutate(
    alert_bucket = case_when(
      score_base >= 0.95 ~ "Extreme",
      score_base >= 0.80 ~ "Red",
      score_base >= 0.50 ~ "Amber",
      TRUE ~ "Green"
    )
  )

bucket_distribution <- validated_df %>%
  count(alert_bucket) %>%
  mutate(share = n / sum(n)) %>%
  arrange(match(alert_bucket, c("Green", "Amber", "Red", "Extreme")))

p16 <- ggplot(bucket_distribution, aes(x = alert_bucket, y = share)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Illustrative alert bucket distribution",
    x = NULL,
    y = "Share of days"
  ) +
  theme_minimal()

save_plot_local(
  p16,
  file.path(FIG_DIR, "16_alert_bucket_distribution.png"),
  width = 8,
  height = 5
)

alert_thresholds <- tibble(
  bucket = c("Green", "Amber", "Red", "Extreme"),
  rule = c(
    "score < 0.50",
    "0.50 <= score < 0.80",
    "0.80 <= score < 0.95",
    "score >= 0.95"
  ),
  interpretation = c(
    "Normal monitoring conditions",
    "Elevated stress worth monitoring",
    "High-priority review tier",
    "Extreme stress escalation tier"
  )
)

decision_rules <- tibble(
  condition = c(
    "High score + high average price",
    "High score + wide daily range + fewer high-price hours",
    "Low score + high average price",
    "High range-only profile"
  ),
  interpretation = c(
    "Broad and persistent expensive conditions",
    "Multidimensional stress without uniformly high prices",
    "Persistently expensive but less multidimensional day",
    "Sharp intraday dispersion rather than broad stress"
  ),
  suggested_action = c(
    "Escalate for analyst review",
    "Review intraday structure and specific hours",
    "Track as headline expensive day",
    "Flag for volatility-focused review"
  )
)

write_csv(alert_thresholds, file.path(TAB_DIR, "tab_alert_thresholds.csv"))
write_csv(decision_rules, file.path(TAB_DIR, "tab_decision_rules.csv"))

# ==========================================================
# 10) Save validated daily panel
# ==========================================================
write_csv(validated_df, file.path(CLEAN_DIR, "omie_market_panel_daily_validated.csv"))
saveRDS(validated_df, file.path(CLEAN_DIR, "omie_market_panel_daily_validated.rds"), compress = TRUE)

cat("DONE.\n")
cat("Saved:\n")
cat("- data_clean/omie_market_panel_daily_validated.csv/.rds\n")
cat("- outputs/tables/tab_weight_sensitivity_summary.csv\n")
cat("- outputs/tables/tab_benchmark_comparison.csv\n")
cat("- outputs/tables/tab_incremental_value_summary.csv\n")
cat("- outputs/tables/tab_unique_day_profile_summary.csv\n")
cat("- outputs/tables/tab_composite_unique_vs_price.csv\n")
cat("- outputs/tables/tab_price_unique_vs_composite.csv\n")
cat("- outputs/tables/tab_range_unique_vs_composite.csv\n")
cat("- outputs/tables/tab_alert_thresholds.csv\n")
cat("- outputs/tables/tab_decision_rules.csv\n")
cat("- outputs/figures/14_benchmark_recall_comparison.png\n")
cat("- outputs/figures/15_incremental_value_unique_days.png\n")
cat("- outputs/figures/16_alert_bucket_distribution.png\n")