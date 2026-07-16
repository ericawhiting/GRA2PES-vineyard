# part of GRA2PES-VINEYARD to simplify GRA2PES files to read surface CO and aggregate by month
# Erica Whiting (erwh@umich.edu)

# ------------------------------------------------------------
# LOAD LIBRARIES & ARGUMENTS
# ------------------------------------------------------------
library(ncdf4)
library(stringr)
library(abind)

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

# stop if file does not exist
check_file_exists <- function(file) {
  if (!file.exists(file)) {
    stop("Required file does not exist: ", file)
  }
}


read_co_surface <- function(file) {
  nc <- nc_open(file)
  on.exit(nc_close(nc))

  co <- ncvar_get(nc, "CO", collapse_degen = FALSE)

  dim_names <- vapply(nc$var$CO$dim, function(d) d$name, character(1))

  # Drop level dimension becuase cropped to single level already
  level_i <- match("level", dim_names)

  if (!is.na(level_i)) {
    subs <- lapply(dim(co), seq_len)
    subs[[level_i]] <- 1
    # uses indexing operator, equiv: co[,,1,, drop = TRUE]
    co <- do.call(`[`, c(list(co), subs, list(drop = TRUE)))
    dim_names <- dim_names[-level_i]
  }

  # We want consistent array order:
  # time, y, x
  desired_order <- c("time", "y", "x")

  if (!all(desired_order %in% dim_names)) {
    stop(
      "CO dimensions are not as expected in file: ", file, "\n",
      "Found dimensions: ", paste(dim_names, collapse = ", "), "\n",
      "Expected dimensions including: time, y, x"
    )
  }
  # permuting 3d array
  co <- aperm(co, match(desired_order, dim_names))

  return(co)
}

get_att_value <- function(nc, var, att, default = NULL) {
  out <- try(ncatt_get(nc, var, att), silent = TRUE)

  if (inherits(out, "try-error")) {
    return(default)
  }

  if (!is.null(out$hasatt) && out$hasatt) {
    return(out$value)
  }

  default
}
copy_atts <- function(nc_in, nc_out, var_in, var_out = var_in,
                      skip = c("_FillValue")) {
  atts <- ncatt_get(nc_in, var_in)

  for (att_name in names(atts)) {
    if (att_name %in% skip) next
    if (att_name %in% c("hasatt", "value")) next

    ncatt_put(nc_out, var_out, att_name, atts[[att_name]])
  }
}
copy_global_atts <- function(nc_in, nc_out, skip = character()) {
  atts <- ncatt_get(nc_in, 0)

  for (att_name in names(atts)) {
    if (att_name %in% skip) next
    if (att_name %in% c("hasatt", "value")) next

    ncatt_put(nc_out, 0, att_name, atts[[att_name]])
  }
}

# to compute weighted monthly mean
selected_sum_count <- function(arr, hour_idx) {
  # arr dimensions must be:
  # time, y, x

  selected <- arr[hour_idx, , , drop = FALSE]

  nt <- dim(selected)[1]
  ny <- dim(selected)[2]
  nx <- dim(selected)[3]

  # Convert to matrix:
  # rows = selected hours
  # columns = grid cells
  selected_mat <- matrix(selected, nrow = nt, ncol = ny * nx)

  selected_sum <- colSums(selected_mat, na.rm = TRUE)
  selected_count <- colSums(!is.na(selected_mat))

  selected_sum <- matrix(selected_sum, nrow = ny, ncol = nx)
  selected_count <- matrix(selected_count, nrow = ny, ncol = nx)

  list(
    sum = selected_sum,
    count = selected_count
  )
}


# ------------------------------------------------------------
# MAIN MONTHLY PROCESSING FUNCTION
# ------------------------------------------------------------

process_one_month <- function(year_month, GRA2PES_CO_path, hours_to_average) {

  message("")
  message("============================================================")
  message("Processing year_month: ", year_month)
  message("============================================================")

  # -------------------------
  # Input files
  # -------------------------

  wk_00_11_file <- file.path(
    GRA2PES_CO_path,
    paste0("GRA2PESv1.1_total_", year_month, "_weekdy_00to11Z_COsurface.nc")
  )

  wk_12_23_file <- file.path(
    GRA2PES_CO_path,
    paste0("GRA2PESv1.1_total_", year_month, "_weekdy_12to23Z_COsurface.nc")
  )

  sa_00_11_file <- file.path(
    GRA2PES_CO_path,
    paste0("GRA2PESv1.1_total_", year_month, "_satdy_00to11Z_COsurface.nc")
  )

  sa_12_23_file <- file.path(
    GRA2PES_CO_path,
    paste0("GRA2PESv1.1_total_", year_month, "_satdy_12to23Z_COsurface.nc")
  )

  su_00_11_file <- file.path(
    GRA2PES_CO_path,
    paste0("GRA2PESv1.1_total_", year_month, "_sundy_00to11Z_COsurface.nc")
  )

  su_12_23_file <- file.path(
    GRA2PES_CO_path,
    paste0("GRA2PESv1.1_total_", year_month, "_sundy_12to23Z_COsurface.nc")
  )

  input_files <- c(
    wk_00_11_file,
    wk_12_23_file,
    sa_00_11_file,
    sa_12_23_file,
    su_00_11_file,
    su_12_23_file
  )

  invisible(lapply(input_files, check_file_exists))

  ref_file <- wk_00_11_file # assumes all files have same spatial info to base new netcdf format on

  # -------------------------
  # Read CO arrays
  # -------------------------

  message("Reading CO files...")

  wk_00_11_CO <- read_co_surface(wk_00_11_file)
  wk_12_23_CO <- read_co_surface(wk_12_23_file)

  sa_00_11_CO <- read_co_surface(sa_00_11_file)
  sa_12_23_CO <- read_co_surface(sa_12_23_file)

  su_00_11_CO <- read_co_surface(su_00_11_file)
  su_12_23_CO <- read_co_surface(su_12_23_file)

  #message("wk_00_11_CO dim: ", paste(dim(wk_00_11_CO), collapse = " x "))
  #message("wk_12_23_CO dim: ", paste(dim(wk_12_23_CO), collapse = " x "))

  # -------------------------
  # Combine 00-11 and 12-23 into 24-hour daytype arrays
  # -------------------------

  wk_00_23_CO <- abind(wk_00_11_CO, wk_12_23_CO, along = 1)
  sa_00_23_CO <- abind(sa_00_11_CO, sa_12_23_CO, along = 1)
  su_00_23_CO <- abind(su_00_11_CO, su_12_23_CO, along = 1)

  stopifnot(dim(wk_00_23_CO)[1] == 24)
  stopifnot(dim(sa_00_23_CO)[1] == 24)
  stopifnot(dim(su_00_23_CO)[1] == 24)

  stopifnot(identical(dim(wk_00_23_CO)[2:3], dim(sa_00_23_CO)[2:3]))
  stopifnot(identical(dim(wk_00_23_CO)[2:3], dim(su_00_23_CO)[2:3]))

  ny <- dim(wk_00_23_CO)[2]
  nx <- dim(wk_00_23_CO)[3]

  message("Spatial dimensions: y = ", ny, ", x = ", nx)

  # -------------------------
  # Create calendar for this month
  # -------------------------

  year <- as.numeric(str_sub(year_month, 1, 4))
  month <- as.numeric(str_sub(year_month, 5, 6))

  dates <- seq(
    as.Date(sprintf("%04d-%02d-01", year, month)),
    as.Date(sprintf("%04d-%02d-01", year, month)) + 31,
    by = "day"
  )

  dates <- dates[format(dates, "%m") == sprintf("%02d", month)]

  # Use POSIX wday to avoid locale issues with weekdays()
  # wday: 0 = Sunday, 6 = Saturday
  wday <- as.POSIXlt(dates)$wday

  daytypes <- ifelse(
    wday == 6, "sat",
    ifelse(wday == 0, "sun", "wk")
  )

  n_wk <- sum(daytypes == "wk")
  n_sat <- sum(daytypes == "sat")
  n_sun <- sum(daytypes == "sun")

  message("Calendar day counts:")
  message("  Weekdays : ", n_wk)
  message("  Saturdays: ", n_sat)
  message("  Sundays  : ", n_sun)

  # -------------------------
  # Memory-efficient weighted monthly mean
  # -------------------------
  #
  # This is equivalent to repeating the weekday, Saturday, and Sunday
  # day-type arrays according to the calendar, but avoids creating a huge
  # full month array.
  #

  hour_idx <- hours_to_average + 1

  message("Selected hours: ", paste(hours_to_average, collapse = ", "))
  #message("R hour indices: ", paste(hour_idx, collapse = ", "))

  wk_sc <- selected_sum_count(wk_00_23_CO, hour_idx)
  sat_sc <- selected_sum_count(sa_00_23_CO, hour_idx)
  sun_sc <- selected_sum_count(su_00_23_CO, hour_idx)

  sum_array <-
    n_wk  * wk_sc$sum +
    n_sat * sat_sc$sum +
    n_sun * sun_sc$sum

  count_array <-
    n_wk  * wk_sc$count +
    n_sat * sat_sc$count +
    n_sun * sun_sc$count

  month_array_mean <- sum_array / count_array
  month_array_mean[count_array == 0] <- NA

  stopifnot(identical(dim(month_array_mean), c(ny, nx)))

  message("Monthly mean dimensions: ", paste(dim(month_array_mean), collapse = " x "))

  # Free large arrays before writing
  rm(
    wk_00_11_CO, wk_12_23_CO,
    sa_00_11_CO, sa_12_23_CO,
    su_00_11_CO, su_12_23_CO,
    wk_00_23_CO, sa_00_23_CO, su_00_23_CO,
    wk_sc, sat_sc, sun_sc,
    sum_array, count_array
  )
  gc()

  # -------------------------
  # Read spatial metadata from reference file
  # -------------------------

  message("Reading spatial metadata from: ", ref_file)

  ref_nc <- nc_open(ref_file)
  nc_monthly <- NULL

  tryCatch({

    # ==========================================================
    # Read spatial variables and key metadata from reference file
    # ==========================================================

    x_vals <- ncvar_get(ref_nc, "x")
    y_vals <- ncvar_get(ref_nc, "y")
    lat_vals <- ncvar_get(ref_nc, "lat")
    lon_vals <- ncvar_get(ref_nc, "lon")

    x_units <- get_att_value(ref_nc, "x", "units", "Meter")
    y_units <- get_att_value(ref_nc, "y", "units", "Meter")

    lat_units <- get_att_value(ref_nc, "lat", "units", "degrees_north")
    lon_units <- get_att_value(ref_nc, "lon", "units", "degrees_east")

    lat_long_name <- get_att_value(ref_nc, "lat", "long_name", "latitude coordinate")
    lon_long_name <- get_att_value(ref_nc, "lon", "long_name", "longitude coordinate")

    lat_standard_name <- get_att_value(ref_nc, "lat", "standard_name", "latitude")
    lon_standard_name <- get_att_value(ref_nc, "lon", "standard_name", "longitude")

    co_units <- get_att_value(ref_nc, "CO", "units", "")
    co_fill <- get_att_value(ref_nc, "CO", "_FillValue", -9999)
    co_long_name <- get_att_value(ref_nc, "CO", "long_name", "Carbon Monoxide")

    grid_mapping_name <- get_att_value(
      ref_nc,
      "CO",
      "grid_mapping",
      "lambert_conformal_conic"
    )

    message("x_units: ", x_units)
    message("y_units: ", y_units)
    message("lat_units: ", lat_units)
    message("lon_units: ", lon_units)
    message("co_units: ", co_units)
    message("co_long_name: ", co_long_name)
    message("Grid mapping variable: ", grid_mapping_name)

    if (!grid_mapping_name %in% names(ref_nc$var)) {
      stop("Grid mapping variable not found in reference file: ", grid_mapping_name)
    }

    # ==========================================================
    # Read Lambert Conformal Conic projection attributes
    # ==========================================================

    lcc_grid_mapping_name <- get_att_value(
      ref_nc,
      grid_mapping_name,
      "grid_mapping_name",
      "lambert_conformal_conic"
    )

    lcc_lon0 <- get_att_value(
      ref_nc,
      grid_mapping_name,
      "longitude_of_central_meridian",
      NULL
    )

    lcc_lat0 <- get_att_value(
      ref_nc,
      grid_mapping_name,
      "latitude_of_projection_origin",
      NULL
    )

    lcc_standard_parallel <- get_att_value(
      ref_nc,
      grid_mapping_name,
      "standard_parallel",
      NULL
    )

    lcc_earth_radius <- get_att_value(
      ref_nc,
      grid_mapping_name,
      "earth_radius",
      NULL
    )

    lcc_false_easting <- get_att_value(
      ref_nc,
      grid_mapping_name,
      "false_easting",
      0
    )

    lcc_false_northing <- get_att_value(
      ref_nc,
      grid_mapping_name,
      "false_northing",
      0
    )

    # ==========================================================
    # Sanity checks
    # ==========================================================

    stopifnot(length(x_vals) == nx)
    stopifnot(length(y_vals) == ny)

    # In R/ncdf4, lat/lon are typically returned as x,y,
    # even though ncdump shows lat(y,x), lon(y,x).
    message("dim(lat_vals): ", paste(dim(lat_vals), collapse = " x "))
    message("dim(lon_vals): ", paste(dim(lon_vals), collapse = " x "))
    message("expected R-order lat/lon dim: ", nx, " x ", ny)

    stopifnot(identical(dim(lat_vals), c(nx, ny)))
    stopifnot(identical(dim(lon_vals), c(nx, ny)))

    # month_array_mean is currently y,x from our calculation.
    # Convert to x,y for ncdf4 writing.
    month_array_mean_xy <- t(month_array_mean)

    message("dim(month_array_mean): ", paste(dim(month_array_mean), collapse = " x "))
    message("dim(month_array_mean_xy): ", paste(dim(month_array_mean_xy), collapse = " x "))

    stopifnot(identical(dim(month_array_mean_xy), c(nx, ny)))

    # Catch obviously bad metadata before writing
    if (!grepl("degree", lat_units, ignore.case = TRUE)) {
      stop("lat_units looks wrong: ", lat_units)
    }

    if (!grepl("degree", lon_units, ignore.case = TRUE)) {
      stop("lon_units looks wrong: ", lon_units)
    }

    if (!grepl("mole", co_units, ignore.case = TRUE)) {
      warning("CO units do not contain 'mole': ", co_units)
    }

    # ==========================================================
    # Output path
    # ==========================================================

    monthly_path <- file.path(GRA2PES_CO_path, "monthly")

    if (!dir.exists(monthly_path)) {
      dir.create(monthly_path, recursive = TRUE)
    }

    out_file <- file.path(
      monthly_path,
      paste0(
        "GRA2PESv1.1_total_",
        year_month,
        "_COsurface_",
        sprintf("%02d", min(hours_to_average)),
        "_",
        sprintf("%02d", max(hours_to_average)),
        "Z.nc"
      )
    )

    message("Writing output file: ", out_file)

    # ==========================================================
    # Define dimensions using true projected x/y coordinate values
    # ==========================================================

    x_dim <- ncdim_def(
      name = "x",
      units = x_units,
      vals = x_vals,
      create_dimvar = TRUE,
      longname = "x coordinate of projection"
    )

    y_dim <- ncdim_def(
      name = "y",
      units = y_units,
      vals = y_vals,
      create_dimvar = TRUE,
      longname = "y coordinate of projection"
    )

    # ==========================================================
    # Define variables
    #
    # Important:
    # Use dim = list(x_dim, y_dim) for ncdf4/R writing.
    # ncdump will still show these as lat(y,x), lon(y,x), CO(y,x).
    # ==========================================================

    lat_var <- ncvar_def(
      name = "lat",
      units = lat_units,
      dim = list(x_dim, y_dim),
      missval = NULL,
      longname = lat_long_name,
      prec = "float"
    )

    lon_var <- ncvar_def(
      name = "lon",
      units = lon_units,
      dim = list(x_dim, y_dim),
      missval = NULL,
      longname = lon_long_name,
      prec = "float"
    )

    CO_var <- ncvar_def(
      name = "CO",
      units = co_units,
      dim = list(x_dim, y_dim),
      missval = co_fill,
      longname = co_long_name,
      prec = "float"
    )

    # Scalar CF grid mapping variable.
    lcc_var <- ncvar_def(
      name = grid_mapping_name,
      units = "",
      dim = list(),
      missval = NULL,
      longname = grid_mapping_name,
      prec = "integer"
    )

    nc_monthly <- nc_create(
      filename = out_file,
      vars = list(lat_var, lon_var, CO_var, lcc_var),
      force_v4 = TRUE
    )

    # ==========================================================
    # Write data
    # ==========================================================

    ncvar_put(nc_monthly, "lat", lat_vals)
    ncvar_put(nc_monthly, "lon", lon_vals)
    ncvar_put(nc_monthly, "CO", month_array_mean_xy)

    # Scalar metadata container for projection attributes
    ncvar_put(nc_monthly, grid_mapping_name, 0L)

    # ==========================================================
    # Explicitly write coordinate variable attributes
    # ==========================================================

    ncatt_put(nc_monthly, "x", "units", x_units)
    ncatt_put(nc_monthly, "x", "long_name", "x coordinate of projection")
    ncatt_put(nc_monthly, "x", "standard_name", "projection_x_coordinate")

    ncatt_put(nc_monthly, "y", "units", y_units)
    ncatt_put(nc_monthly, "y", "long_name", "y coordinate of projection")
    ncatt_put(nc_monthly, "y", "standard_name", "projection_y_coordinate")

    ncatt_put(nc_monthly, "lat", "units", lat_units)
    ncatt_put(nc_monthly, "lat", "long_name", lat_long_name)
    ncatt_put(nc_monthly, "lat", "standard_name", lat_standard_name)

    ncatt_put(nc_monthly, "lon", "units", lon_units)
    ncatt_put(nc_monthly, "lon", "long_name", lon_long_name)
    ncatt_put(nc_monthly, "lon", "standard_name", lon_standard_name)

    # ==========================================================
    # Explicitly write CO attributes
    # ==========================================================

    ncatt_put(nc_monthly, "CO", "units", co_units)
    ncatt_put(nc_monthly, "CO", "long_name", co_long_name)
    ncatt_put(nc_monthly, "CO", "grid_mapping", grid_mapping_name)
    ncatt_put(nc_monthly, "CO", "coordinates", "lon lat")

    ncatt_put(
      nc_monthly,
      "CO",
      "cell_methods",
      paste0(
        "time: mean over selected hours ",
        paste(hours_to_average, collapse = ","),
        " for all calendar days in month"
      )
    )

    # ==========================================================
    # Explicitly write Lambert Conformal Conic attributes
    # ==========================================================

    ncatt_put(
      nc_monthly,
      grid_mapping_name,
      "grid_mapping_name",
      lcc_grid_mapping_name
    )

    if (!is.null(lcc_lon0)) {
      ncatt_put(
        nc_monthly,
        grid_mapping_name,
        "longitude_of_central_meridian",
        lcc_lon0
      )
    }

    if (!is.null(lcc_lat0)) {
      ncatt_put(
        nc_monthly,
        grid_mapping_name,
        "latitude_of_projection_origin",
        lcc_lat0
      )
    }

    if (!is.null(lcc_standard_parallel)) {
      ncatt_put(
        nc_monthly,
        grid_mapping_name,
        "standard_parallel",
        as.numeric(lcc_standard_parallel),
        prec ="float"
      )
    }

    if (!is.null(lcc_earth_radius)) {
      ncatt_put(
        nc_monthly,
        grid_mapping_name,
        "earth_radius",
        lcc_earth_radius
      )
    }

    ncatt_put(
      nc_monthly,
      grid_mapping_name,
      "false_easting",
      lcc_false_easting
    )

    ncatt_put(
      nc_monthly,
      grid_mapping_name,
      "false_northing",
      lcc_false_northing
    )

    # ==========================================================
    # Global attributes
    # ==========================================================

    # Optional: copy global attributes from reference file.
    # This should be safe because these are global, not variable-specific.
    copy_global_atts(ref_nc, nc_monthly)

    # Add processing metadata
    ncatt_put(
      nc_monthly,
      0,
      "monthly_processing",
      "Monthly mean surface CO created from GRA2PES day-type files"
    )

    ncatt_put(nc_monthly, 0, "year_month", year_month)
    ncatt_put(nc_monthly, 0, "hours_averaged", paste(hours_to_average, collapse = ","))
    ncatt_put(nc_monthly, 0, "source_reference_file", basename(ref_file))

    ncatt_put(
      nc_monthly,
      0,
      "history_monthly",
      paste(Sys.time(), "created monthly mean CO surface file in R")
    )

    message("Finished writing: ", out_file)

  }, finally = {

    if (!is.null(nc_monthly)) {
      nc_close(nc_monthly)
    }

    nc_close(ref_nc)
  })


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