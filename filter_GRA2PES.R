# file called in harvest_GRA2PES.sh
# part of GRA2PES-VINEYARD to simplify GRA2PES files to read surface CO and aggregate by month
# Erica Whiting (erwh@umich.edu)

# -------------------------
# LOAD LIBRARIES
# -------------------------
suppressPackageStartupMessages({
    library(ncdf4)
    library(stringr)
})

# -------------------------
# DATA FORMAT:
# -------------------------
# each month, sector file has with sub directories: satdy, sundy, weekdy
# each sub directory has two files, 00-11 and 12-23 HH

# -------------------------
# LOAD IN ARGS
# -------------------------
args <- commandArgs(trailingOnly = TRUE)
GRA2PES_sector <- args[1] # nolint: object_name_linter.
GRA2PES_year_month <- args[2] # nolint: object_name_linter.
GRA2PES_path <- args [3] # nolint: object_name_linter.

# -------------------------
# Get GRA2PES DATA REPOSITORY
# -------------------------
GRA2PES_directory <- paste0(GRA2PES_path, "/", GRA2PES_year_month) # nolint: object_name_linter.

# -------------------------
# OPEN GRA2PES FILE AND SAVE SURFACE CO IN NEW .NC FILE
# -------------------------
open_and_pull_surface_co <- function(file_name, GRA2PES_path) {
    #' open and save out surface-level CO emissions data from GRA2PES files
    #' @param file_name
    #' @param GRA2PES_path path to where GRA2PES files can be accessed on device and where CO dir will be created for output
    #' no return but writes nc file for each file (weekdy, satdy, sundy; 00to11Z, 12to23Z)
    #' output nc files are saved in /CO/ dir following GRA2PES_path param
    #' CO surface data saved as mole km^-2 hr^-1 for 4km x 4km cells

    nc <- nc_open(file_name)
    # read dimensions
    west_east <- nc$dim$west_east$len
    south_north <- nc$dim$south_north$len
    Time_len <- nc$dim$Time$len

    # read variables
    XLAT <- ncvar_get(nc, "XLAT")    # dim: west_east, south_north
    XLONG <- ncvar_get(nc, "XLONG")  # dim: west_east, south_north
    Times <- ncvar_get(nc, "Times")  # dim: character, Time
    Time <- ncvar_get(nc, "Time")    # dim: Time

    # pull CO at BOTTOM (surface) level: dim: west_east, south_north, 1, Time
    CO_surf <- ncvar_get(nc, "CO", start = c(1, 1, 1, 1), count = c(west_east, south_north, 1, Time_len))

    # Close GRA2PES nc file
    nc_close(nc)

    # -------------------------
    # Write surface CO to NetCDF
    # -------------------------
    # Define dimensions
    dim_west_east <- ncdim_def("west_east", units = "index", vals = 1:west_east)
    dim_south_north <- ncdim_def("south_north", units = "index", vals = 1:south_north)
    dim_Time <- ncdim_def("Time", units = "hours since 2021-01-01 00:00:00", vals = Time, unlim = TRUE)

    # Define variables
    var_xlat <- ncvar_def("XLAT", "degree_north", list(dim_west_east, dim_south_north), missval = NA, prec = "float")
    var_xlong <- ncvar_def("XLONG", "degree_east", list(dim_west_east, dim_south_north), missval = NA, prec = "float")
    var_times <- ncvar_def("Times", "", list(dim_Time), prec = "char", longname = "char_times")
    var_co <- ncvar_def("CO", "mole km^-2 hr^-1", list(dim_west_east, dim_south_north, dim_Time), missval = NA, prec = "float")

    # create file name and path, store in new directory called CO
    original_name <- str_split(file_name, "/")[[1]][7]
    newfile <- paste0(GRA2PES_path, "/CO/", str_split(original_name, "Z")[[1]][1], "Z_COsurface.nc")

    # create file
    ncnew <- nc_create(newfile, list(var_xlat, var_xlong, var_times, var_co))

    # add in data to nc file
    ncvar_put(ncnew, var_xlat, XLAT)
    ncvar_put(ncnew, var_xlong, XLONG)
    ncvar_put(ncnew, var_times, Times)
    ncvar_put(ncnew, var_co, CO_surf)

    # close nc files
    nc_close(ncnew)
}


# -------------------------
# ITERATE OVER FILES IN SECTOR/MONTH
# -------------------------
day_of_week_list <- c("weekdy", "satdy", "sundy")
for (day_of_week in day_of_week_list) {
    for (file in list.files(paste0(GRA2PES_directory, "/", day_of_week), full.names = TRUE)) {
        open_and_pull_surface_co(file, GRA2PES_path)
    }
}
