#!/bin/bash

# Removes records from the gridcoverage database table that contain bad values
# in the majoraxis, minoraxis, crs, and the_geom fields. Also removes
# appropriate records from the referencing tables (grid_info and grid).

# Removes the FFMP source .bin files

# This script should be run on dx3 or dx4. 


# Exit on a command returning a non-zero status
set -e

echo "INFO: Running delta script for RODO DR #8026"

echo "INFO: Removing problematic grid data from database"
/awips2/psql/bin/psql -U awipsadmin -d metadata -h dx1 -v ON_ERROR_STOP=on -f removeBadGridCoverageRecords.sql
echo "INFO: Successfully removed data from database"

echo "INFO: Removing FFMP source files"
rm -f /awips2/edex/data/utility/common_static/configured/*/ffmp/sources/*
echo "INFO: Successfully removed FFMP source files"

echo "INFO: Done"
