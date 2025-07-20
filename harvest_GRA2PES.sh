#!/bin/bash
GRA2PES_storage = "/no_backup/erwh/GRA2PES" # where to store GRA2PES data

# Read in total emissions files, could swap out for sectors
# Read codes from codes.txt
while read -r SECTOR; do
    # skip commented out sector or blank lines, see GRA2PES_sectors.R
    [[ -z "$SECTOR" || "$SECTOR" =~ ^# ]] && continue

    while read -r YEAR_MONTH; do
        [[ -z "$YEAR_MONTH" || "$YEAR_MONTH" =~ ^# ]] && continue

        echo "Harvesting GRA2PES from $YEAR_MONTH, $SECTOR"
        
        # assign url to download files
        GRA2PES_file_url = "https://data.nist.gov/od/ds/mds2-3520/GRA2PESv1.0_${SECTOR}_${YEAR_MONTH}.tar.gz"
        # assign file to download as
        GRA2PES_for_squashing = "${GRA2PES_storage}/GRA2PESv1.0_${SECTOR}_${YEAR_MONTH}.tar.gz"

        # transfer GRA2PES file that matches month_year (ex: 202101)
        curl -L -0 "$GRA2PES_for_squashing" "$GRA2PES_file_url" || { echo "Uh oh! Can't find the GRA2PES file of that vintage ($SECTOR $YEAR_MONTH)"; exit 1; }

        gunzip "$GRA2PES_file_url" || { echo "Did you misplace your grapes?"; exit 1; }

        tar -xf "${GRA2PES_file_url%.gz}" || { echo "404 grapes not found"; exit 1; }
        # remove tar file, now data exists in repository with YEARMONTH format
        rm -rf "${GRA2PES_file_url%.gz}" || { echo "cleanup failed...grapes everywhere!"; exit 1; }
        
        # Process GRA2PES
        Rscript filter_GRA2PES.R "$SECTOR" "$MONTH" "$GRA2PES_storage" || { echo "R script failed for $SECTOR $MONTH"; exit 1; }

        # remove GRA2PES YEAR_MONTH directory
        rm -rf "${GRA2PES_storage}/${MONTH}"

        echo "Finished harvesting GRA2PES from $YEAR_MONTH, $SECTOR"
        echo "--------------------------------------------------------------"

    done < GRA2PES_months.txt

done < GRA2PES_sectors.txt

echo "Batch processing finished."