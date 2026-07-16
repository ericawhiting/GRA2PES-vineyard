#!/bin/bash
set -euo pipefail

MONTH_LIST="GRA2PES_months.txt"
GRA2PES_CO_path="/no_backup/erwh/GRA2PES/CO"

mkdir -p logs

echo "Starting monthly GRA2PES CO processing"

Rscript bottle_GRA2PES.R \
    "$MONTH_LIST" \
    "$GRA2PES_CO_path" \
    2>&1 | tee logs/make_monthly_GRA2PES_CO.log

echo "Done."
