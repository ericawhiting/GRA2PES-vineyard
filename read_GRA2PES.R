# used gunzip on the .tar.gz file to get a .tar file
# then tar -xf on the .tar file to get a directory for the month
# each month, sector file has with sub directories: satdy, sundy, weekdy
# each day has two files, 00-11 and 12-23 HH, with dimensions: Time, south_north, west_east, bottom_top_stag, bottom_top
library(ncdf4)
library(stringr)


file_name <- "/no_backup/erwh/GRA2PES/202101/weekdy/GRA2PESv1.0_ONROAD_GAS_202101_weekdy_00to11Z.nc"

# OPEN GRA2PES FILE AND SAVE SURFACE CO IN NEW .NC FILE
open_and_pull_surface_co <- function(file_name) {
    nc <- nc_open(file_name)
    # Dimensions
    west_east <- nc$dim$west_east$len   # 1332
    south_north <- nc$dim$south_north$len # 1008
    # bottom_top <- nc$dim$bottom_top$len   # 20
    Time_len <- nc$dim$Time$len           # 12

    # Variables
    XLAT <- ncvar_get(nc, "XLAT")               # [west_east, south_north]
    XLONG <- ncvar_get(nc, "XLONG")             # [west_east, south_north]
    Times <- ncvar_get(nc, "Times")             # [character, Time]
    Time <- ncvar_get(nc, "Time")               # [Time]

    # CO at BOTTOM (surface) level: [west_east, south_north, 1, Time]
    CO_surf <- ncvar_get(nc, "CO", start = c(1, 1, 1, 1), count = c(west_east, south_north, 1, Time_len))
    # This will return an array [west_east, south_north, Time]
    dim(CO_surf) # 1332 x 1008 x 12

    nc_close(nc)

    # Define dimensions
    dimX <- ncdim_def("west_east", units = "index", vals = 1:west_east)
    dimY <- ncdim_def("south_north", units = "index", vals = 1:south_north)
    dimTime <- ncdim_def("Time", units = "hours since 2021-01-01 00:00:00", vals = Time, unlim=TRUE)

    # Define variables
    var_xlat <- ncvar_def("XLAT", "degree_north", list(dimX, dimY), missval = NA, prec = "float")
    var_xlong <- ncvar_def("XLONG", "degree_east", list(dimX, dimY), missval = NA, prec = "float")
    var_times <- ncvar_def("Times", "", list(dimTime), prec = "char", longname = "char_times")
    var_co <- ncvar_def("CO", "mole km^-2 hr^-1", list(dimX, dimY, dimTime), missval = NA, prec = "float")

    # Create new file and write variables
    day_of_week <- str_split(file_name, "/")[[1]][6]
    original_name <- str_split(file_name, "/")[[1]][7]
    newfile <- paste0("/no_backup/erwh/GRA2PES/CO/", str_split(original_name, "Z")[[1]][1], "Z_", day_of_week, "_COsurface.nc")

    ncnew <- nc_create(newfile, list(var_xlat, var_xlong, var_times, var_co))

    ncvar_put(ncnew, var_xlat, XLAT)
    ncvar_put(ncnew, var_xlong, XLONG)
    ncvar_put(ncnew, var_times, Times)
    ncvar_put(ncnew, var_co, CO_surf)

    nc_close(ncnew)
}


# ITERATE OVER FILES IN SECTOR/MONTH
# 2 files in each
# weekdy, sundy, satdy
GRA2PES_path <- "/no_backup/erwh/GRA2PES/"
# for directory that has format /d{6}
directory <- ""


# DOWNLOAD AND SAVE GRA2PES FILES, UNZIP & DELETE OLD