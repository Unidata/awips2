#! /bin/bash
# DR #4890 - This script drops the table taf_change_group and its sub-tables

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

# takes one arg: table
function tableExists {
${PSQL} -U awips -d metadata -q \
   -c "SELECT count(*) from information_schema.tables where table_schema='awips' and table_name='${1}' ; " \
   2>1 | grep -q '^\s*1$'
}

# takes one arg: table
function drop_table {
   table=${1}
   if tableExists ${table} ; then
      echo "INFO Dropping ${table}"
      ${PSQL} -U awips -d metadata -c "DROP TABLE ${table} ;"
   else
      echo "INFO: table ${table} does not exist"
   fi
}

tables=( "taf_icing_layers" "taf_sky_cover" "taf_temperature_forecasts" "taf_turbulence_layers"
"taf_weather_conditions" "taf_change_groups")

for table in ${tables[@]} ; do 
   drop_table ${table}
done
