suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
  library(tibble)
  library(scales)
})

source(file.path("R", "utils_project.R"))

# ==========================================================
# 0) Local helper: winsorize + rescale to [0, 1]
# ==========================================================
rescale01_winsor <- function(x, probs = c(0.01, 0.99)) {
  x <- as.numeric(x)
  
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  xw <- pmin(pmax(x, qs[1]), qs[2])
  
  rng <- range(xw, na.rm = TRUE)
  
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(0, length(x)))
  }
  
  (xw - rng[1]) / (rng[2] - rng[1])
}

save_plot_local <- function(plot_obj, path, width = 10, height = 5, dpi = 300) {
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

daily_path  <- file.path(CLEAN_DIR, "omie_market_panel_daily.rds")
hourly_path <- file.path(CLEAN_DIR, "omie_market_panel_hourly.rds")

stopifnot(file.exists(daily_path))
stopifnot(file.exists(hourly_path))

# ==========================================================
# 2) Load inputs
# ==========================================================
market_daily  <- readRDS(daily_path)
market_hourly <- readRDS(hourly_path)

assert_required_columns(
  market_daily,
  c(
    "date", "year", "month",
    "avg_price_spain_eur_mwh",
    "daily_price_range",
    "avg_total_steps",
    "n_high_price_hours"
  ),
  "market_daily"
)

assert_required_columns(
  market_hourly,
  c("date", "hour", "price_spain_eur_mwh", "total_steps"),
  "market_hourly"
)

stopifnot(nrow(market_daily) > 0)
stopifnot(nrow(market_hourly) > 0)

has_avg_total_steps_from_sides <- "avg_total_steps_from_sides" %in% names(market_daily)
has_avg_quarter_row_share <- "avg_quarter_row_share" %in% names(market_daily)

# ==========================================================
# 3) Normalize score components
# ==========================================================
market_daily <- market_daily %>%
  mutate(
    z_avg_price        = rescale01_winsor(avg_price_spain_eur_mwh),
    z_daily_range      = rescale01_winsor(daily_price_range),
    z_high_price_hours = rescale01_winsor(n_high_price_hours),
    z_steps            = rescale01_winsor(avg_total_steps)
  )

market_daily <- market_daily %>%
  mutate(
    score_core =
      0.40 * z_avg_price +
      0.40 * z_daily_range +
      0.20 * z_high_price_hours,
    score_extended =
      0.35 * z_avg_price +
      0.35 * z_daily_range +
      0.20 * z_high_price_hours +
      0.10 * z_steps,
    score_base = score_extended
  )

# ==========================================================
# 4) Descriptive summaries
# ==========================================================
annual_summary <- market_daily %>%
  group_by(year) %>%
  summarise(
    avg_daily_price = mean(avg_price_spain_eur_mwh, na.rm = TRUE),
    avg_daily_range = mean(daily_price_range, na.rm = TRUE),
    avg_high_price_hours = mean(n_high_price_hours, na.rm = TRUE),
    avg_total_steps = mean(avg_total_steps, na.rm = TRUE),
    avg_total_steps_from_sides = if (has_avg_total_steps_from_sides) mean(avg_total_steps_from_sides, na.rm = TRUE) else NA_real_,
    avg_quarter_row_share = if (has_avg_quarter_row_share) mean(avg_quarter_row_share, na.rm = TRUE) else NA_real_,
    avg_stress_score_core = mean(score_core, na.rm = TRUE),
    avg_stress_score_extended = mean(score_extended, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

monthly_summary <- market_daily %>%
  mutate(month_label = month.abb[month]) %>%
  group_by(year, month, month_label) %>%
  summarise(
    avg_daily_price = mean(avg_price_spain_eur_mwh, na.rm = TRUE),
    avg_daily_range = mean(daily_price_range, na.rm = TRUE),
    avg_total_steps = mean(avg_total_steps, na.rm = TRUE),
    avg_quarter_row_share = if (has_avg_quarter_row_share) mean(avg_quarter_row_share, na.rm = TRUE) else NA_real_,
    avg_stress_score_core = mean(score_core, na.rm = TRUE),
    avg_stress_score_extended = mean(score_extended, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  arrange(year, month)

write_csv(annual_summary, file.path(TAB_DIR, "annual_summary.csv"))
write_csv(monthly_summary, file.path(TAB_DIR, "monthly_summary.csv"))

# ==========================================================
# 5) Figure 1: Daily average price
# ==========================================================
p1 <- ggplot(market_daily, aes(x = date, y = avg_price_spain_eur_mwh)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Daily average price in the Spanish day-ahead market",
    x = NULL,
    y = "EUR/MWh"
  ) +
  theme_minimal()

save_plot_local(p1, file.path(FIG_DIR, "01_daily_average_price.png"), width = 10, height = 5)

# ==========================================================
# 6) Figure 2: Daily range
# ==========================================================
p2 <- ggplot(market_daily, aes(x = date, y = daily_price_range)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Daily intraday price range",
    x = NULL,
    y = "EUR/MWh"
  ) +
  theme_minimal()

save_plot_local(p2, file.path(FIG_DIR, "02_daily_price_range.png"), width = 10, height = 5)

# ==========================================================
# 7) Figure 3: Average price vs range
# ==========================================================
price_range_corr <- cor(
  market_daily$avg_price_spain_eur_mwh,
  market_daily$daily_price_range,
  use = "complete.obs"
)

p3 <- ggplot(
  market_daily,
  aes(x = avg_price_spain_eur_mwh, y = daily_price_range)
) +
  geom_point(alpha = 0.35, size = 1) +
  labs(
    title = paste0("Average daily price vs daily range (corr = ", round(price_range_corr, 3), ")"),
    x = "Average daily price (EUR/MWh)",
    y = "Daily range (EUR/MWh)"
  ) +
  theme_minimal()

save_plot_local(p3, file.path(FIG_DIR, "03_avg_price_vs_range.png"), width = 8, height = 6)

# ==========================================================
# 8) Figure 4: Monthly heatmap of average hourly prices
# ==========================================================
hourly_heatmap <- market_hourly %>%
  mutate(
    ym = as.Date(format(date, "%Y-%m-01"))
  ) %>%
  group_by(ym, hour) %>%
  summarise(
    avg_hourly_price = mean(price_spain_eur_mwh, na.rm = TRUE),
    .groups = "drop"
  )

p4 <- ggplot(hourly_heatmap, aes(x = hour, y = ym, fill = avg_hourly_price)) +
  geom_tile() +
  scale_x_continuous(breaks = seq(1, 24, by = 2)) +
  labs(
    title = "Monthly heatmap of average hourly prices",
    x = "Hour of day",
    y = NULL,
    fill = "EUR/MWh"
  ) +
  theme_minimal()

save_plot_local(p4, file.path(FIG_DIR, "04_hourly_price_heatmap.png"), width = 10, height = 7)

# ==========================================================
# 9) Figure 6: Main score figure
# ==========================================================
p6 <- ggplot(market_daily, aes(x = date, y = score_extended)) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Composite market stress score",
    x = NULL,
    y = "Extended stress score"
  ) +
  theme_minimal()

save_plot_local(p6, file.path(FIG_DIR, "06_market_stress_score.png"), width = 10, height = 5)

# ==========================================================
# 10) Hourly profiles
# ==========================================================
hourly_price_profile <- market_hourly %>%
  group_by(hour) %>%
  summarise(
    avg_price = mean(price_spain_eur_mwh, na.rm = TRUE),
    .groups = "drop"
  )

p9 <- ggplot(hourly_price_profile, aes(x = hour, y = avg_price)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_x_continuous(breaks = 1:24) +
  labs(
    title = "Average price by hour of day",
    x = "Hour",
    y = "EUR/MWh"
  ) +
  theme_minimal()

save_plot_local(p9, file.path(FIG_DIR, "09_hourly_average_price_profile.png"), width = 9, height = 5)

hourly_steps_profile <- market_hourly %>%
  group_by(hour) %>%
  summarise(
    avg_steps = mean(total_steps, na.rm = TRUE),
    .groups = "drop"
  )

p10 <- ggplot(hourly_steps_profile, aes(x = hour, y = avg_steps)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_x_continuous(breaks = 1:24) +
  labs(
    title = "Average cleared steps by hour of day",
    x = "Hour",
    y = "Average total steps"
  ) +
  theme_minimal()

save_plot_local(p10, file.path(FIG_DIR, "10_hourly_average_steps_profile.png"), width = 9, height = 5)

# ==========================================================
# 11) Monthly seasonality
# ==========================================================
monthly_stress <- market_daily %>%
  group_by(month) %>%
  summarise(
    avg_score_core = mean(score_core, na.rm = TRUE),
    avg_score_extended = mean(score_extended, na.rm = TRUE),
    .groups = "drop"
  )

p11 <- ggplot(monthly_stress, aes(x = month, y = avg_score_extended)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Monthly stress seasonality",
    x = "Month",
    y = "Average extended stress score"
  ) +
  theme_minimal()

save_plot_local(p11, file.path(FIG_DIR, "11_monthly_stress_seasonality.png"), width = 9, height = 5)

# ==========================================================
# 12) Save enriched daily panel
# ==========================================================
write_csv(market_daily, file.path(CLEAN_DIR, "omie_market_panel_daily_enriched.csv"))
saveRDS(market_daily, file.path(CLEAN_DIR, "omie_market_panel_daily_enriched.rds"), compress = TRUE)

cat("DONE.\n")
cat("Saved:\n")
cat("- data_clean/omie_market_panel_daily_enriched.csv/.rds\n")
cat("- outputs/tables/annual_summary.csv\n")
cat("- outputs/tables/monthly_summary.csv\n")
cat("- outputs/figures/01_daily_average_price.png\n")
cat("- outputs/figures/02_daily_price_range.png\n")
cat("- outputs/figures/03_avg_price_vs_range.png\n")
cat("- outputs/figures/04_hourly_price_heatmap.png\n")
cat("- outputs/figures/06_market_stress_score.png\n")
cat("- outputs/figures/09_hourly_average_price_profile.png\n")
cat("- outputs/figures/10_hourly_average_steps_profile.png\n")
cat("- outputs/figures/11_monthly_stress_seasonality.png\n")