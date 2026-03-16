library(httr)

dir.create("data_raw/omie", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1)  anual files ZIP
# -----------------------------
years_zip <- 2018:2022

for (yr in years_zip) {
  filename <- paste0("marginalpdbc_", yr, ".zip")
  url <- paste0(
    "https://www.omie.es/en/file-download?filename=",
    filename,
    "&parents=marginalpdbc"
  )
  
  dest <- file.path("data_raw/omie", filename)
  
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

# -----------------------------
# 2) daily files .1
# -----------------------------
dates_daily <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = as.Date("2026-03-09"),   
  by   = "day"
)

for (d in dates_daily) {
  d <- as.Date(d)
  ymd <- format(d, "%Y%m%d")
  
  filename <- paste0("marginalpdbc_", ymd, ".1")
  url <- paste0(
    "https://www.omie.es/en/file-download?filename=",
    filename,
    "&parents=marginalpdbc"
  )
  
  dest <- file.path("data_raw/omie", filename)
  
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

# -----------------------------
# 3) unzip files 
# -----------------------------

zip_files <- list.files("data_raw/omie", pattern = "\\.zip$", full.names = TRUE)

for (zf in zip_files) {
  out_dir <- "data_raw/omie"
  
  message("Processing: ", basename(zf))
  
  extracted <- unzip(zf, exdir = out_dir)
  
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