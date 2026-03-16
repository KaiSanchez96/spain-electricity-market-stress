suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(ggplot2)
})

project_paths <- function(clean_dir = "data_clean", out_dir = "outputs") {
  out_fig <- file.path(out_dir, "figures")
  out_tab <- file.path(out_dir, "tables")

  dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)

  list(
    clean_dir = clean_dir,
    out_dir = out_dir,
    out_fig = out_fig,
    out_tab = out_tab
  )
}

save_plot <- function(plot, filename, out_fig, w = 8, h = 5, dpi = 300) {
  ggsave(
    filename = file.path(out_fig, filename),
    plot = plot,
    width = w,
    height = h,
    dpi = dpi
  )
}

winsorize <- function(x, p = 0.01) {
  qs <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

assert_required_columns <- function(df, required_cols, df_name = deparse(substitute(df))) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "%s is missing required columns: %s",
        df_name,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

safe_pct_rank <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  dplyr::percent_rank(x)
}

build_score_from_weights <- function(df, scheme, components) {
  weighted_cols <- purrr::imap(
    components,
    ~ df[[.y]] * .x
  )
  df[[scheme]] <- Reduce(`+`, weighted_cols)
  df
}

make_scheme_label <- function(x) {
  dplyr::case_when(
    x == "score_base" ~ "Base",
    x == "score_core" ~ "Core",
    x == "score_extended" ~ "Extended",
    x == "score_equal" ~ "Equal weights",
    x == "score_priceheavy" ~ "Price-heavier",
    x == "score_rangeheavy" ~ "Range-heavier",
    x == "score_no_steps" ~ "No steps",
    x == "score_range_plus" ~ "Range-plus",
    x == "score_simple_pr" ~ "Simple price-range",
    TRUE ~ x
  )
}

compute_top_overlap <- function(df, score_vars, n_top = 20) {
  get_top_dates <- function(var) {
    df %>%
      arrange(desc(.data[[var]]), date) %>%
      slice_head(n = n_top) %>%
      pull(date)
  }

  expand.grid(
    scheme_a = score_vars,
    scheme_b = score_vars,
    stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    rowwise() %>%
    mutate(
      overlap_top20 = length(intersect(get_top_dates(scheme_a), get_top_dates(scheme_b))),
      overlap_share = overlap_top20 / n_top
    ) %>%
    ungroup()
}

flag_top_quantile <- function(x, p = 0.95) {
  x >= quantile(x, p, na.rm = TRUE)
}
