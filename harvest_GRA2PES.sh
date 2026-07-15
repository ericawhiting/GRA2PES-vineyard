#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Read in total emissions files, could swap out for sectors
# Read codes from codes.txt
while read -r SECTOR; do
    # skip commented out sector or blank lines, see GRA2PES_sectors.R
    [[ -z "$SECTOR" || "$SECTOR" =~ ^# ]] && continue

    while read -r YEAR_MONTH; do
        [[ -z "$YEAR_MONTH" || "$YEAR_MONTH" =~ ^# ]] && continue

        echo "Harvesting GRA2PES from $YEAR_MONTH, $SECTOR"
        
        # assign url to download files
        YEAR="${YEAR_MONTH:0:4}"
        # now downloaded through noaa csl rather than nist.gov
        # GRA2PES_file_url="https://data.nist.gov/od/ds/mds2-3520/GRA2PESv1.0_${SECTOR}_${YEAR_MONTH}.tar.gz"
        GRA2PES_file_url="https://csl.noaa.gov/groups/csl4/gra2pes/datasets/data_v1.1/${YEAR}/GRA2PESv1.1_${SECTOR}_${YEAR_MONTH}.tar.gz"
        
        # assign file to download as
        GRA2PES_dir="/no_backup/erwh/GRA2PES" # where to store GRA2PES data
        GRA2PES_file="${GRA2PES_dir}/GRA2PESv1.1_${SECTOR}_${YEAR_MONTH}.tar.gz" # for squashing

        # transfer GRA2PES file that matches month_year (ex: 202101)
        wget -nv -O "$GRA2PES_file" "$GRA2PES_file_url" || { echo "Uh oh! Can't find the GRA2PES file of that vintage ($SECTOR $YEAR_MONTH)"; exit 1; }
        
        echo "GRA2PES picked successfully from NIST"

        gunzip "$GRA2PES_file" || { echo "Did you misplace your grapes?"; exit 1; }
        
        tar -xf "${GRA2PES_file%.gz}" -C $GRA2PES_dir || { echo "404 grapes not found"; exit 1; }
    
        # remove tar file, now data exists in repository with YEARMONTH format
        rm "${GRA2PES_file%.gz}" || { echo "cleanup failed...grapes everywhere!"; exit 1; }
       
        echo "Cleaning Complete"
        
        # -------------------------
        # Process GRA2PES
        # -------------------------
        outdir="${GRA2PES_dir}/CO"

        # iterate over day types
        day_of_week_list=("weekdy" "satdy" "sundy")

        for day_of_week in "${day_of_week_list[@]}"; do

            indir="${GRA2PES_dir}/${YEAR_MONTH}/${day_of_week}"

            # loop over NetCDF files
            for file in "$indir"/*.nc; do
                
                original_name="$(basename "$file")"
                
                prefix="${original_name%%Z*}"

                newfile="${outdir}/${prefix}Z_COsurface.nc"

                # trim to lowe
                ncks -O \
                    -v CO,lat,lon,time \
                    -d level,0,0 \
                    "$file" \
                    "$newfile"
            done # closes iterating over time files

        done # closes day of week loops

        # Rscript filter_GRA2PES.R "$SECTOR" "$YEAR_MONTH" "$GRA2PES_dir" || { echo "R script failed for $SECTOR $YEAR_MONTH"; exit 1; }
        echo "Filtering Complete"
        
        # -------------------------
        
        # Remove GRA2PES YEAR_MONTH Directory now that variable is saved out
        if [[ -n "$GRA2PES_dir" && -n "$YEAR_MONTH" ]]; then
            rm -r "${GRA2PES_dir}/${YEAR_MONTH}"
            echo "hi"
        else
            echo "There aren't any grapes over here! Not deleting."
        fi
    

        echo "Finished harvesting GRA2PES from $YEAR_MONTH, $SECTOR"
        echo "--------------------------------------------------------------"

    done < "$SCRIPT_DIR/GRA2PES_months.txt"

done < "$SCRIPT_DIR/GRA2PES_sectors.txt"
