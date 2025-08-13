Erica Whiting
erwh@umich.edu

The goal of this set of files is to complement the NIST GRA2PES dataset. NIST GRA2PES READ ME: https://www.nist.gov/system/files/documents/2024/09/18/Readme_GRA2PES_20240917.pdf

harvest_GRA2PES.sh is tailored to download files for a select set of sectors and months and save out the surface level CO fluxes for each available file (weekdy, satdy, sundy) [00-11Z, 12-23Z]. The files are downloaded one at a time to reduce impact on local memory, and then the surface CO data is pulled out and saved as a separate nc file (using filter_GRA2PES.R called within the harvest script). The original files are removed before moving on to the next month.

bottle_GRA2PES.R pulls the newly created surface CO nc files for each day of week and set of 12 hours, correctly arranges the hourly data by day of week following a monthly calendar and find monthly mean surface CO flux at each grid cell. User could adjust which hours of the day contribute to the monthly average. The mean monthly CO flux over the extent of GRA2PES is saved as a nc file. CO emissions are mole km^-2 hr^-1 for 4km x 4km cells
