suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

source(file.path("R", "utils_project.R"))

# ==========================================================
# 0) Paths
# ==========================================================
RAW_DIR   <- file.path("data_raw", "omie_curves")
CLEAN_DIR <- "data_clean"

stopifnot(dir.exists(RAW_DIR))
dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# 1) List files
# ==========================================================
files_curves <- list.files(
  RAW_DIR,
  pattern = "^curva_pbc_\\d{8}\\.1$",
  full.names = TRUE
)

stopifnot(length(files_curves) > 0)

n_raw_curve_files <- length(files_curves)

# ==========================================================
# 2) Reader for one curve file + file-level QA
# ==========================================================
read_curve_file_with_qa <- function(path) {
  file_name <- basename(path)
  
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "latin1"),
    error = function(e) NULL
  )
  
  if (is.null(lines)) {
    qa <- tibble(
      file_name = file_name,
      file_status = "read_failed",
      header_found = FALSE,
      detected_format = NA_character_,
      rows_raw_below_header = NA_integer_,
      rows_kept = NA_integer_,
      countries = NA_character_,
      offer_types = NA_character_,
      offer_statuses = NA_character_
    )
    return(list(data = tibble(), qa = qa))
  }
  
  lines <- lines[nzchar(trimws(lines))]
  
  if (length(lines) < 3) {
    qa <- tibble(
      file_name = file_name,
      file_status = "too_short",
      header_found = FALSE,
      detected_format = NA_character_,
      rows_raw_below_header = 0L,
      rows_kept = 0L,
      countries = NA_character_,
      offer_types = NA_character_,
      offer_statuses = NA_character_
    )
    return(list(data = tibble(), qa = qa))
  }
  
  header_idx <- which(grepl("^(Hora|Periodo);Fecha;Pais;", lines))
  
  if (length(header_idx) == 0) {
    qa <- tibble(
      file_name = file_name,
      file_status = "header_not_found",
      header_found = FALSE,
      detected_format = NA_character_,
      rows_raw_below_header = 0L,
      rows_kept = 0L,
      countries = NA_character_,
      offer_types = NA_character_,
      offer_statuses = NA_character_
    )
    return(list(data = tibble(), qa = qa))
  }
  
  header_idx <- header_idx[[1]]
  lines_data <- lines[header_idx:length(lines)]
  rows_raw_below_header <- max(length(lines_data) - 1L, 0L)
  
  df <- tryCatch(
    readr::read_delim(
      file = I(lines_data),
      delim = ";",
      col_names = TRUE,
      locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
      show_col_types = FALSE,
      trim_ws = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(df) || nrow(df) == 0) {
    qa <- tibble(
      file_name = file_name,
      file_status = "parsed_but_empty",
      header_found = TRUE,
      detected_format = NA_character_,
      rows_raw_below_header = rows_raw_below_header,
      rows_kept = 0L,
      countries = NA_character_,
      offer_types = NA_character_,
      offer_statuses = NA_character_
    )
    return(list(data = tibble(), qa = qa))
  }
  
  # quitar columna final vacía si existe
  if (ncol(df) > 0) {
    last_col <- df[[ncol(df)]]
    if (all(is.na(last_col) | trimws(as.character(last_col)) == "")) {
      df <- df[, -ncol(df), drop = FALSE]
    }
  }
  
  # normalizar nombres
  raw_names <- names(df)
  raw_names <- iconv(raw_names, from = "latin1", to = "UTF-8", sub = "")
  raw_names <- trimws(raw_names)
  names(df) <- raw_names
  
  # convertir columnas lógicas vacías a texto
  df <- df %>%
    mutate(across(where(is.logical), as.character))
  
  detected_format <- NA_character_
  
  # -------- formato viejo: Hora --------
  if ("Hora" %in% names(df)) {
    detected_format <- "hourly_legacy"
    
    if (ncol(df) < 8) {
      qa <- tibble(
        file_name = file_name,
        file_status = "unexpected_old_format_columns",
        header_found = TRUE,
        detected_format = detected_format,
        rows_raw_below_header = rows_raw_below_header,
        rows_kept = 0L,
        countries = NA_character_,
        offer_types = NA_character_,
        offer_statuses = NA_character_
      )
      return(list(data = tibble(), qa = qa))
    }
    
    df <- df[, 1:8, drop = FALSE]
    names(df) <- c(
      "period_raw",
      "date_raw",
      "country",
      "unit",
      "offer_type",
      "volume_mw_mwh",
      "price_eur_mwh",
      "offer_status"
    )
    
    df <- df %>%
      mutate(
        period_type = "hourly_legacy",
        hour = suppressWarnings(as.integer(period_raw)),
        quarter = NA_integer_,
        offer_topology = NA_character_
      )
    
    # -------- formatos con Periodo --------
  } else if ("Periodo" %in% names(df)) {
    if (ncol(df) < 9) {
      qa <- tibble(
        file_name = file_name,
        file_status = "unexpected_periodo_format_columns",
        header_found = TRUE,
        detected_format = "periodo_unknown",
        rows_raw_below_header = rows_raw_below_header,
        rows_kept = 0L,
        countries = NA_character_,
        offer_types = NA_character_,
        offer_statuses = NA_character_
      )
      return(list(data = tibble(), qa = qa))
    }
    
    df <- df[, 1:9, drop = FALSE]
    names(df) <- c(
      "period_raw",
      "date_raw",
      "country",
      "unit",
      "offer_type",
      "volume_mw_mwh",
      "price_eur_mwh",
      "offer_status",
      "offer_topology"
    )
    
    period_values <- df$period_raw[!is.na(df$period_raw)]
    period_values <- trimws(as.character(period_values))
    
    # Caso A: Periodo numérico 1..24
    if (length(period_values) > 0 && all(grepl("^\\d{1,2}$", period_values))) {
      detected_format <- "hourly_periodo_numeric"
      
      df <- df %>%
        mutate(
          period_type = "hourly_periodo_numeric",
          hour = suppressWarnings(as.integer(period_raw)),
          quarter = NA_integer_
        )
      
      # Caso B: Periodo tipo H1Q1
    } else {
      detected_format <- "quarter_hourly_new"
      
      df <- df %>%
        mutate(
          period_type = "quarter_hourly_new",
          hour = suppressWarnings(as.integer(stringr::str_extract(period_raw, "(?<=H)\\d+"))),
          quarter = suppressWarnings(as.integer(stringr::str_extract(period_raw, "(?<=Q)\\d+")))
        )
    }
    
  } else {
    qa <- tibble(
      file_name = file_name,
      file_status = "unknown_format",
      header_found = TRUE,
      detected_format = NA_character_,
      rows_raw_below_header = rows_raw_below_header,
      rows_kept = 0L,
      countries = NA_character_,
      offer_types = NA_character_,
      offer_statuses = NA_character_
    )
    return(list(data = tibble(), qa = qa))
  }
  
  df <- df %>%
    mutate(
      date = suppressWarnings(lubridate::dmy(date_raw)),
      unit = dplyr::na_if(trimws(as.character(unit)), ""),
      country = trimws(as.character(country)),
      offer_type = trimws(as.character(offer_type)),
      offer_status = trimws(as.character(offer_status)),
      offer_topology = trimws(as.character(offer_topology)),
      period_raw = trimws(as.character(period_raw)),
      volume_mw_mwh = suppressWarnings(as.numeric(volume_mw_mwh)),
      price_eur_mwh = suppressWarnings(as.numeric(price_eur_mwh)),
      datetime = as.POSIXct(date) + lubridate::hours(hour - 1),
      file_name = file_name
    ) %>%
    filter(
      !is.na(date),
      !is.na(hour),
      hour >= 1,
      hour <= 24
    ) %>%
    select(
      date,
      datetime,
      period_raw,
      period_type,
      hour,
      quarter,
      country,
      unit,
      offer_type,
      volume_mw_mwh,
      price_eur_mwh,
      offer_status,
      offer_topology,
      date_raw,
      file_name
    )
  
  file_status <- if (nrow(df) > 0) "ok" else "parsed_but_empty"
  
  qa <- tibble(
    file_name = file_name,
    file_status = file_status,
    header_found = TRUE,
    detected_format = detected_format,
    rows_raw_below_header = rows_raw_below_header,
    rows_kept = nrow(df),
    countries = if (nrow(df) > 0) paste(sort(unique(na.omit(df$country))), collapse = ", ") else NA_character_,
    offer_types = if (nrow(df) > 0) paste(sort(unique(na.omit(df$offer_type))), collapse = ", ") else NA_character_,
    offer_statuses = if (nrow(df) > 0) paste(sort(unique(na.omit(df$offer_status))), collapse = ", ") else NA_character_
  )
  
  list(data = df, qa = qa)
}

# ==========================================================
# 3) Read and bind all files
# ==========================================================
parsed_list <- vector("list", length(files_curves))
qa_list <- vector("list", length(files_curves))

for (i in seq_along(files_curves)) {
  res <- read_curve_file_with_qa(files_curves[[i]])
  parsed_list[[i]] <- res$data
  qa_list[[i]] <- res$qa
  
  if (i %% 250 == 0 || i == length(files_curves)) {
    cat("Processed", i, "of", length(files_curves), "files\n")
  }
}

omie_curves <- bind_rows(parsed_list)
curve_file_qa <- bind_rows(qa_list)

# ==========================================================
# 3B) Pipeline QA summary
# ==========================================================
raw_rows_total <- sum(curve_file_qa$rows_raw_below_header, na.rm = TRUE)
rows_kept_total <- nrow(omie_curves)
rows_dropped_total <- raw_rows_total - rows_kept_total
rows_dropped_share <- ifelse(raw_rows_total > 0, rows_dropped_total / raw_rows_total, NA_real_)

pipeline_qa_curves <- tibble(
  metric = c(
    "Raw curve files processed",
    "Files with status ok",
    "Files parsed but empty",
    "Parsed raw rows below header",
    "Rows kept after parsing filters",
    "Rows dropped during parsing",
    "Dropped row share",
    "Unique period types detected"
  ),
  value = c(
    n_raw_curve_files,
    sum(curve_file_qa$file_status == "ok", na.rm = TRUE),
    sum(curve_file_qa$file_status == "parsed_but_empty", na.rm = TRUE),
    raw_rows_total,
    rows_kept_total,
    rows_dropped_total,
    round(rows_dropped_share, 6),
    n_distinct(omie_curves$period_type)
  )
)

# ==========================================================
# 4) QA
# ==========================================================
stopifnot(nrow(omie_curves) > 0)
stopifnot(all(!is.na(omie_curves$date)))
stopifnot(all(omie_curves$hour >= 1 & omie_curves$hour <= 24, na.rm = TRUE))

qa_summary_curves <- tibble(
  n_rows = nrow(omie_curves),
  min_date = min(omie_curves$date, na.rm = TRUE),
  max_date = max(omie_curves$date, na.rm = TRUE),
  n_days = n_distinct(omie_curves$date),
  countries = paste(sort(unique(na.omit(omie_curves$country))), collapse = ", "),
  offer_types = paste(sort(unique(na.omit(omie_curves$offer_type))), collapse = ", "),
  offer_statuses = paste(sort(unique(na.omit(omie_curves$offer_status))), collapse = ", "),
  period_types = paste(sort(unique(na.omit(omie_curves$period_type))), collapse = ", ")
)

missing_summary <- omie_curves %>%
  summarise(
    missing_volume = sum(is.na(volume_mw_mwh)),
    missing_price = sum(is.na(price_eur_mwh)),
    missing_unit = sum(is.na(unit)),
    missing_offer_topology = sum(is.na(offer_topology))
  )

print(qa_summary_curves)
print(count(omie_curves, country))
print(count(omie_curves, offer_type))
print(count(omie_curves, offer_status))
print(count(omie_curves, period_type))
print(missing_summary)
print(count(curve_file_qa, file_status))

# ==========================================================
# 5) Save clean outputs
# ==========================================================

# ---- ES only ----
omie_curves_es <- omie_curves %>%
  filter(country == "ES")

saveRDS(
  omie_curves_es,
  file.path(CLEAN_DIR, "omie_day_ahead_curves_es.rds"),
  compress = TRUE
)

saveRDS(
  omie_curves_es %>% filter(offer_status == "C"),
  file.path(CLEAN_DIR, "omie_day_ahead_curves_es_cleared.rds"),
  compress = TRUE
)

rm(omie_curves_es)
gc()

# ---- ES + MI cleared only ----
# No guardamos el objeto completo ES+MI porque no hace falta para 05
omie_curves_es_mi_cleared <- omie_curves %>%
  filter(country %in% c("ES", "MI"), offer_status == "C")

saveRDS(
  omie_curves_es_mi_cleared,
  file.path(CLEAN_DIR, "omie_day_ahead_curves_es_mi_cleared.rds"),
  compress = TRUE
)

rm(omie_curves_es_mi_cleared)
gc()

write_csv(
  qa_summary_curves,
  file.path(CLEAN_DIR, "omie_day_ahead_curves_qa_summary.csv")
)

write_csv(
  pipeline_qa_curves,
  file.path(CLEAN_DIR, "omie_day_ahead_curves_pipeline_qa.csv")
)

write_csv(
  curve_file_qa,
  file.path(CLEAN_DIR, "omie_day_ahead_curves_file_qa.csv")
)

write_csv(
  missing_summary,
  file.path(CLEAN_DIR, "omie_day_ahead_curves_missing_summary.csv")
)

cat("DONE.\n")
cat("Saved:\n")
cat("- data_clean/omie_day_ahead_curves_es.rds\n")
cat("- data_clean/omie_day_ahead_curves_es_cleared.rds\n")
cat("- data_clean/omie_day_ahead_curves_es_mi_cleared.rds\n")
cat("- data_clean/omie_day_ahead_curves_pipeline_qa.csv\n")
cat("- data_clean/omie_day_ahead_curves_qa_summary.csv\n")
cat("- data_clean/omie_day_ahead_curves_file_qa.csv\n")
cat("- data_clean/omie_day_ahead_curves_missing_summary.csv\n")