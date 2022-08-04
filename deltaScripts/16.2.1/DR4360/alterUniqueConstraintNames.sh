#!/bin/bash 
# DR #4360 - This scripts assigns a name to tables' unique constraints

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh

# Does not include tables generated from classes in the AWIP2_NCEP branch

tables=("acars" "acarssounding" "airep" "binlightning" "bufrncwf" "bufrmos_location" "bufrmosavn" "bufrmoseta" "bufrmosgfs" "bufrmoshpc"
"bufrmoslamp" "bufrmosmrf" "bufrmosngm" "bufrmthdw" "bufrquikscat" "bufrascat" "bufrhdw" "bufrsigwx" "bufrssmi" "bufrua" "cwa" "cwat" "ffmp" "fog"
"ccfp" "crimss" "fssobs" "goessounding" "grid" "grid_info" "ldad_manual" "ldadhydro" "ldadmesonet" "ldadprofiler" "lsr" "madis" "modelsounding"
"ncpafm" "ncscd" "nctaf" "ncuair" "nucaps" "obs" "pirep" "poessounding"
"practicewarning" "profiler" "qc" "sfcobs" "svrwx" "tcg" "tcs" "vaa" "warning")
echo "INFO: rename tables unique constraints"

for table in ${tables[@]} ; do
	renameConstraint ${table} uk_${table}_datauri_fields
done

echo "INFO: Done rename tables unique constraints"
