# -------------------------
# LOAD LIBRARIES
# -------------------------
library(lubridate)
library(ncdf4)
library(stringr)
library(abind)
library(foreach)
library(doParallel)
# -------------------------
# Find year, month of grapes files
read_txt_list <- function(file) {
  lines <- readLines(file)
  lines <- lines[!grepl("^\\s*#", lines)] # skip lines that are commented out, skip whitespace characters
  lines <- trimws(lines) # trim whitespace, including trailing
  lines <- lines[lines != ""]
  lines
}
year_month_list <- read_txt_list("GRA2PES_months.txt")


# function to read in year_month
create_monthly_file_from_filtered_GRA2PES <- function(year_month) {
  GRA2PES_CO_path <- "/no_backup/erwh/GRA2PES/CO/"
  # -------------------------
  # weekday files
  wk_00_11 <- nc_open(paste0(GRA2PES_CO_path, "GRA2PESv1.0_total_", year_month, "_weekdy_00to11Z_weekdy_COsurface.nc"))
  wk_12_23 <- nc_open(paste0(GRA2PES_CO_path, "GRA2PESv1.0_total_", year_month, "_weekdy_12to23Z_weekdy_COsurface.nc"))
  # saturday files
  sa_00_11 <- nc_open(paste0(GRA2PES_CO_path, "GRA2PESv1.0_total_", year_month, "_satdy_00to11Z_satdy_COsurface.nc"))
  sa_12_23 <- nc_open(paste0(GRA2PES_CO_path, "GRA2PESv1.0_total_", year_month, "_satdy_12to23Z_satdy_COsurface.nc"))
  # sunday files
  su_00_11 <- nc_open(paste0(GRA2PES_CO_path, "GRA2PESv1.0_total_", year_month, "_sundy_00to11Z_sundy_COsurface.nc"))
  su_12_23 <- nc_open(paste0(GRA2PES_CO_path, "GRA2PESv1.0_total_", year_month, "_sundy_12to23Z_sundy_COsurface.nc"))
  # -------------------------
  # var: "XLAT"  "XLONG" "Times" "CO"
  wk_00_11_xlat <- ncvar_get(wk_00_11, "XLAT")
  wk_12_23_xlat <- ncvar_get(wk_12_23, "XLAT")
  sa_00_11_xlat <- ncvar_get(sa_00_11, "XLAT")
  sa_12_23_xlat <- ncvar_get(sa_12_23, "XLAT")
  su_00_11_xlat <- ncvar_get(su_00_11, "XLAT")
  su_12_23_xlat <- ncvar_get(su_12_23, "XLAT")

  wk_00_11_xlong <- ncvar_get(wk_00_11, "XLONG")
  wk_12_23_xlong <- ncvar_get(wk_12_23, "XLONG")
  sa_00_11_xlong <- ncvar_get(sa_00_11, "XLONG")
  sa_12_23_xlong <- ncvar_get(sa_12_23, "XLONG")
  su_00_11_xlong <- ncvar_get(su_00_11, "XLONG")
  su_12_23_xlong <- ncvar_get(su_12_23, "XLONG")

  wk_00_11_Time <- ncvar_get(wk_00_11, "Time")
  wk_12_23_Time <- ncvar_get(wk_12_23, "Time")
  sa_00_11_Time <- ncvar_get(sa_00_11, "Time")
  sa_12_23_Time <- ncvar_get(sa_12_23, "Time")
  su_00_11_Time <- ncvar_get(su_00_11, "Time")
  su_12_23_Time <- ncvar_get(su_12_23, "Time")

  wk_00_11_CO <- ncvar_get(wk_00_11, "CO")
  wk_12_23_CO <- ncvar_get(wk_12_23, "CO")
  sa_00_11_CO <- ncvar_get(sa_00_11, "CO")
  sa_12_23_CO <- ncvar_get(sa_12_23, "CO")
  su_00_11_CO <- ncvar_get(su_00_11, "CO")
  su_12_23_CO <- ncvar_get(su_12_23, "CO")

  # save units
  var_units <- ncatt_get(wk_00_11, "CO", "units")$value
  xlat_units <- ncatt_get(wk_00_11, "XLAT", "units")$value
  xlong_units <- ncatt_get(wk_00_11, "XLONG", "units")$value
  # -------------------------
  # close files
  nc_close(wk_00_11)
  nc_close(wk_12_23)
  nc_close(sa_00_11)
  nc_close(sa_12_23)
  nc_close(su_00_11)
  nc_close(su_12_23)
  # -------------------------
  # check that spatial dimensions match
  latlon_dims <- list(dim(wk_00_11_xlat), dim(wk_12_23_xlat), dim(sa_00_11_xlat), dim(sa_12_23_xlat), dim(su_00_11_xlat), dim(su_12_23_xlat),
                      dim(wk_00_11_xlong), dim(wk_12_23_xlong), dim(sa_00_11_xlong), dim(sa_12_23_xlong), dim(su_00_11_xlong), dim(su_12_23_xlong))

  time_dims <- list(dim(wk_00_11_Time), dim(wk_12_23_Time), dim(sa_00_11_Time), dim(sa_12_23_Time), dim(su_00_11_Time), dim(su_12_23_Time))

  var_dims <- list(dim(wk_00_11_CO), dim(wk_12_23_CO), dim(sa_00_11_CO), dim(sa_12_23_CO), dim(su_00_11_CO), dim(su_12_23_CO))

  stopifnot(all(sapply(latlon_dims, function(x) identical(x, latlon_dims[[1]]))))
  stopifnot(all(sapply(time_dims, function(x) identical(x, time_dims[[1]]))))
  stopifnot(all(sapply(var_dims, function(x) identical(x, var_dims[[1]]))))

  # -------------------------
  # Stack by day of week:
  wk_00_23_CO <- abind(wk_00_11_CO, wk_12_23_CO, along = 3)
  sa_00_23_CO <- abind(sa_00_11_CO, sa_12_23_CO, along = 3)
  su_00_23_CO <- abind(su_00_11_CO, su_12_23_CO, along = 3)
  # -------------------------
  # create calendar of days of week for year_month
  year <- as.numeric(str_sub(year_month, 1, 4))
  month <- as.numeric(str_sub(year_month, 5, 6))
  dates <- seq(as.Date(sprintf("%04d-%02d-01", year, month)),
            as.Date(sprintf("%04d-%02d-01", year, month)) + 31, by = "day")
  dates <- dates[format(dates, "%m") == sprintf("%02d", month)]
  daytypes <- ifelse(weekdays(dates) == "Saturday", "sat",
                      ifelse(weekdays(dates) == "Sunday", "sun",
                            "wk"))
  # -------------------------
  # arrange nc file in calendary order and stack
  day_arrays <- list(wk = wk_00_23_CO, sat = sa_00_23_CO, sun = su_00_23_CO)
  month_list <- lapply(daytypes, function(dt) day_arrays[[dt]])

  # stack:
  month_array <- abind(month_list, along = 4) # new dim: day

  # collapse hour and day into just hour of month
  dim(month_array) <- c(dim(month_array)[1:2], 24 * length(dates)) # [lon, lat, 24*n_days]
  # -------------------------
  # Write to NetCDF

  # Find x, y, time dimensions
  nx <- dim(month_array)[1]
  ny <- dim(month_array)[2]
  nt <- dim(month_array)[3]

  # Define file dimensions
  west_east_dim <- ncdim_def("west_east", "index", 1:nx)
  south_north_dim <- ncdim_def("south_north", "index", 1:ny)
  time_dim <- ncdim_def("Time", "index", 1:nt, unlim = TRUE)

  # Prep time formatting
  # time_vec <- as.POSIXct(rep(dates, each = 24)) + rep(0:23 * 60 * 60, times = length(dates)) # create 24 hours for each day
  # time_str <- format(time_vec, "%Y-%m-%d %H:%M:%S")
  # max_str_len <- max(nchar(time_str))
  # char_split <- strsplit(time_str, "")
  # time_matrix <- do.call(cbind, char_split)

  # Define vars
  XLAT_var <- ncvar_def("XLAT",  units = xlat_units, dim = list(west_east_dim, south_north_dim), missval = NA, prec = "float")
  XLONG_var <- ncvar_def("XLONG", units = xlong_units,  dim = list(west_east_dim, south_north_dim), missval = NA, prec = "float")

  # time_str_dim <- ncdim_def("DateStrLen", "", 1:max_str_len)
  # Time_str_var <- ncvar_def("Times",
  #                       paste0("hours since ", dates[1], " 00:00:00"),
  #                       list(time_str_dim, time_dim), prec = "char")


  CO_var <- ncvar_def("CO", units = var_units, dim = list(west_east_dim, south_north_dim, time_dim), missval = NA, prec = "float")

  # Create file
  monthly_path <- paste0(GRA2PES_CO_path, "monthly")
  if (!dir.exists(monthly_path)) {
    dir.create(monthly_path)
  }

  nc_monthly <- nc_create(paste0(monthly_path, "/GRA2PESv1.0_total_", year_month, "_COsurface.nc"),
                          list(XLAT_var, XLONG_var, CO_var))

  # add in data
  ncvar_put(nc_monthly, XLAT_var, wk_00_11_xlat)
  ncvar_put(nc_monthly, XLONG_var, wk_00_11_xlong)
  #ncvar_put(nc_monthly, Time_str_var, t(time_matrix))
  ncvar_put(nc_monthly, CO_var, month_array)

  # add attributed
  ncatt_put(nc_monthly, "CO", "_FillValue", NaN)
  ncatt_put(nc_monthly, "XLAT", "_FillValue", NaN)
  ncatt_put(nc_monthly, "XLONG", "_FillValue", NaN)

  ncatt_put(nc_monthly, "CO", "units", var_units)
  ncatt_put(nc_monthly, "XLAT", "units", xlat_units)
  ncatt_put(nc_monthly, "XLONG", "units", xlong_units)

  #ncatt_put(nc_monthly, "Times", "long_name", "char_times")

  # close nc files
  nc_close(nc_monthly)
  print(paste0("GRA2PES bottled for ", year_month))
}


num_cores <- 5
registerDoParallel(cores = num_cores)

foreach(i = seq(year_month_list)) %dopar% {
  create_monthly_file_from_filtered_GRA2PES(year_month_list[i])
}
