#!/bin/bash
# DR #2473 drops all deprecated grib data from the database

PSQL="/awips2/psql/bin/psql"

SQL_COMMAND="
delete from plugin_info where name = 'grib';
delete from plugin_info where name = 'ncgrib';
drop table if exists grib, grib_models;
drop table if exists ncgrib, ncgrib1_parameter_map, ncgrib_genprocess, ncgrib_models, ncgrib_parameters, ncgrib_surfaces, ncgridcoverage;
drop sequence if exists gribseq;
drop sequence if exists ncgribseq;

"

echo "INFO: Dropping grib tables."

${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"

echo "INFO: Removing grib site localization files"

find /awips2/edex/data/utility/edex_static/site/ -iname 'gribPathkeys.xml' -exec rm -v {} \;
find /awips2/edex/data/utility/edex_static/site -iname 'gribPurgeRules.xml' -exec rm -v {} \;

rm -rv /awips2/edex/data/utility/common_static/site/*/grib/

echo "INFO: The update finished successfully."
exit 0