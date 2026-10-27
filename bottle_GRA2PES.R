# part of GRA2PES-VINEYARD to simplify GRA2PES files to read surface CO and aggregate by month
# Erica Whiting (erwh@umich.edu)

# ------------------------------------------------------------
# LOAD LIBRARIES & ARGUMENTS
# ------------------------------------------------------------
library(ncdf4)
library(stringr)
library(terra)

hours_to_average <- c(0:23)

# Arguments
args <- commandArgs(trailingOnly = TRUE)
# Expects 2 commandArgs
yearmonth_list_file <- args[1]
GRA2PES_CO_path <- args[2]
GRA2PES_CO_path <- sub("/+$", "", GRA2PES_CO_path) # removes dash at end if included
# ------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------
# find year month of GRA2PES files
read_txt_list <- function(file) {
  lines <- readLines(file)
  lines <- lines[!grepl("^\\s*#", lines)] # skip lines that are commented out, skip whitespace characters
  lines <- trimws(lines) # trim whitespace, including trailing
  lines <- lines[lines != ""]
  lines
}

# stop if file does not exist. expecting same number of files per month
check_file_exists <- function(file) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
}

process_one_month <- function(year_month, GRA2PES_CO_path, hours_to_average) {

  message("")
  message("============================================================")
  message("Processing year_month: ", year_month)
  message("============================================================")

  # -------------------------
  # Input files
  # -------------------------
  wk_00_11_file <- file.path(GRA2PES_CO_path,
                             paste0("GRA2PESv1.1_total_", year_month, "_weekdy_00to11Z_COsurface.nc"))

  wk_12_23_file <- file.path(GRA2PES_CO_path,
                             paste0("GRA2PESv1.1_total_", year_month, "_weekdy_12to23Z_COsurface.nc"))

  sa_00_11_file <- file.path(GRA2PES_CO_path,
                             paste0("GRA2PESv1.1_total_", year_month, "_satdy_00to11Z_COsurface.nc"))

  sa_12_23_file <- file.path(GRA2PES_CO_path,
                             paste0("GRA2PESv1.1_total_", year_month, "_satdy_12to23Z_COsurface.nc"))

  su_00_11_file <- file.path(GRA2PES_CO_path,
                             paste0("GRA2PESv1.1_total_", year_month, "_sundy_00to11Z_COsurface.nc"))

  su_12_23_file <- file.path(GRA2PES_CO_path,
                             paste0("GRA2PESv1.1_total_", year_month, "_sundy_12to23Z_COsurface.nc"))

  # check if the files all exist
  input_files <- c(wk_00_11_file,
                   wk_12_23_file,
                   sa_00_11_file,
                   sa_12_23_file,
                   su_00_11_file,
                   su_12_23_file)

  invisible(lapply(input_files, check_file_exists))

  # -------------------------
  # Read CO & combine 00-11 and 12-23 into 24hours cropped to hours of interest
  # -------------------------

  message("Reading CO files...")
  hours_idx <- hours_to_average + 1

  wk_00_23_CO <- subset(rast(list(rast(wk_00_11_file),
                                  rast(wk_12_23_file))),
                        hours_idx)
  sa_00_23_CO <- subset(rast(list(rast(sa_00_11_file),
                                  rast(sa_12_23_file))),
                        hours_idx)
  su_00_23_CO <- subset(rast(list(rast(su_00_11_file),
                                  rast(su_12_23_file))),
                        hours_idx)

  CO_00_23 <- rast(list(mean(wk_00_23_CO),
                        mean(sa_00_23_CO),
                        mean(su_00_23_CO))) # mean across hours

  message("Selected hours: ", paste(hours_to_average, collapse = ", "))
  # -------------------------
  # Create calendar for month
  # -------------------------

  year <- as.numeric(str_sub(year_month, 1, 4))
  month <- as.numeric(str_sub(year_month, 5, 6))

  dates <- seq(as.Date(sprintf("%04d-%02d-01", year, month)),
               as.Date(sprintf("%04d-%02d-01", year, month)) + 31,
               by = "day")
  dates <- dates[format(dates, "%m") == sprintf("%02d", month)] # cuts to only this month

  # Find day of week
  wday <- as.POSIXlt(dates)$wday # wday: 0 = Sunday, 6 = Saturday

  daytypes <- ifelse(wday == 6, "sat", # if 6 then set to saturday
                     ifelse(wday == 0, "sun", "wk")) # if 0 set to sunday, if not 0 or 6, set to weekday

  # Calendar day countrs to weight contribution from type of day
  n_wk <- sum(daytypes == "wk")
  n_sat <- sum(daytypes == "sat")
  n_sun <- sum(daytypes == "sun")

  # -------------------------
  # Weighted monthly mean
  # -------------------------

  weighted_mean_CO <- terra::weighted.mean(CO_00_23, c(n_wk, n_sat, n_sun))

  # -------------------------
  # Save as netcdf4
  # -------------------------
  monthly_path <- "/no_backup/erwh/GRA2PES/CO_surface_regridded_monthly/"
  filename <- paste0(monthly_path,
                      "/GRA2PESv1.1_total_", year_month, "_COsurface_",
                      sprintf("%02d", min(hours_to_average)), "_", sprintf("%02d", max(hours_to_average)), "Z.nc")
  writeCDF(weighted_mean_CO, filename, "CO", longname = "Carbon Monoxide", unit = "", split = FALSE)

  message("Finished month: ", year_month)
}

# ============================================================
# RUN ALL MONTHS
# ============================================================

year_month_list <- read_txt_list(yearmonth_list_file)

message("")
message("Months to process:")
print(year_month_list)

for (year_month in year_month_list) {
  process_one_month(
    year_month = year_month,
    GRA2PES_CO_path = GRA2PES_CO_path,
    hours_to_average = hours_to_average
  )
}

message("")
message("All months complete.")