suppressPackageStartupMessages({
  library(httr)
})

CURVE_DIR <- file.path("data_raw", "omie_curves")
dir.create(CURVE_DIR, recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# 1) Download annual files
# ==========================================================
years_zip <- 2018:2022

for (yr in years_zip) {
  filename <- paste0("curva_pbc_", yr, ".zip")
  url <- paste0(
    "https://www.omie.es/en/file-download?filename=",
    filename,
    "&parents=curva_pbc"
  )
  
  dest <- file.path(CURVE_DIR, filename)
  
  try({
    res <- GET(url, timeout(30))
    if (status_code(res) == 200 && length(content(res, "raw")) > 0) {
      writeBin(content(res, "raw"), dest)
      message("Downloaded ZIP: ", filename)
    } else {
      message("Failed ZIP: ", filename, " (", status_code(res), ")")
    }
  }, silent = TRUE)
}

# ==========================================================
# 2) Download daily files .1
# ==========================================================
dates_daily <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = as.Date("2026-03-10"),
  by   = "day"
)

for (d in dates_daily) {
  ymd <- format(as.Date(d), "%Y%m%d")
  filename <- paste0("curva_pbc_", ymd, ".1")
  url <- paste0(
    "https://www.omie.es/en/file-download?filename=",
    filename,
    "&parents=curva_pbc"
  )
  
  dest <- file.path(CURVE_DIR, filename)
  
  try({
    res <- GET(url, timeout(30))
    if (status_code(res) == 200 && length(content(res, "raw")) > 0) {
      writeBin(content(res, "raw"), dest)
      message("Downloaded daily: ", filename)
    } else {
      message("Failed daily: ", filename, " (", status_code(res), ")")
    }
  }, silent = TRUE)
}

# ==========================================================
# 3) Unzip
# ==========================================================
zip_files <- list.files(CURVE_DIR, pattern = "\\.zip$", full.names = TRUE)

for (zf in zip_files) {
  extracted <- unzip(zf, exdir = CURVE_DIR)
  
  if (length(extracted) > 0 && all(file.exists(extracted))) {
    ok <- file.remove(zf)
    if (ok) {
      message("Unzipped and removed: ", basename(zf))
    } else {
      warning("Unzipped, but could not remove ZIP: ", basename(zf))
    }
  } else {
    warning("Unzip may have failed, keeping ZIP: ", basename(zf))
  }
}

cat("DONE.\n")


files_curves <- list.files(
  file.path("data_raw", "omie_curves"),
  pattern = "^curva_pbc_\\d{8}\\.1$",
  full.names = TRUE
)

head(files_curves, 3)

test_curve <- files_curves[1]
readLines(test_curve, n = 30)
