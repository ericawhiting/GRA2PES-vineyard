# part of GRA2PES-VINEYARD to simplify GRA2PES files to read surface CO and aggregate by month
# Erica Whiting (erwh@umich.edu)
# reading in monthly mean surface CO fluxes (sector = total)
# -------------------------
# DEFINE WHERE NC FILES FROM bottle_GRA2PES.R ARE STORED
# -------------------------
bottled_path <- "/no_backup/erwh/GRA2PES/CO/monthly"

# -------------------------
# LOAD LIBRARIES
# -------------------------
library(ggplot2)
library(stringr)
library(dplyr)
library(terra)
library(ncdf4)

# -------------------------
# DEFINE URBAN DOMAIN
# example: Washington DC & Baltimore Metropolitan area
# -------------------------
city_latlon <- c(39.0, -77.0)
delta_lon <- 0.8
delta_lat <- 0.6
lonmin <- city_latlon[2] - delta_lon
lonmax <- city_latlon[2] + delta_lon
latmin <- city_latlon[1] - delta_lat
latmax <- city_latlon[1] + delta_lat

# -------------------------
# ITERATE OVER MONTHLY FILES
# -------------------------
GRA2PES_df <- data.frame(NULL)
for (file in list.files(bottled_path, full.names = TRUE)) {
    # file name has format with YYYYMM defining the year, month
    year_month <- str_split(basename(file), "_")[[1]][3]
    month <- str_sub(year_month, 5, 6)

    # open nc file based on year_month
    nc <- nc_open(file)
    XLAT <- ncvar_get(nc, "XLAT")
    XLAT <- t(XLAT[, ncol(XLAT):1])
    XLONG <- ncvar_get(nc, "XLONG")
    XLONG <- t(XLONG[, ncol(XLONG):1])
    CO <- ncvar_get(nc, "CO")
    CO <- t(CO[, ncol(CO):1]) # confirmed orientation by plotting plot(log(rast(CO)))
    # convert from fluxes to emissions
    CO <- CO * 4 * 4  * 16.04 / (3600 * 1000) # (mole km^-2 hr^-1) * (km^2) * (g mole^-1) * (hr s^-1) * (kg g^-1) => kgCO s^-1
    nc_close(nc)

    # crop out urban domain
    mask <- XLONG >= lonmin & XLONG <= lonmax & XLAT >= latmin & XLAT <= latmax
    CO_in_rectangle <- CO[mask]

    # sum monthly CO Emissions [kg s^-1]
    GRA2PES_df <- rbind(GRA2PES_df, data.frame(month = month, CO = sum(CO_in_rectangle)))
}