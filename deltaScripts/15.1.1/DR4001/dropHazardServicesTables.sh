#!/bin/bash
# DR #4001 - force hazard services tables to be recreated

PSQL="/awips2/psql/bin/psql"

echo "INFO: Dropping hazard services tables"

${PSQL} -U awips -d metadata -q -c "DROP TABLE IF EXISTS productdata, producttext, practice_hazards, practice_hazards_attributes, hazards_interoperability, hazards_interoperability_gfe CASCADE;"
${PSQL} -U awips -d metadata -q -c "DROP TABLE IF EXISTS practice_hazards_interoperability, practice_hazards_interoperability_gfe CASCADE;"
${PSQL} -U awips -d metadata -q -c "UPDATE plugin_info SET initialized=false WHERE name='hazards' OR name ='com.raytheon.uf.common.hazards.productgen';"

echo "INFO: Hazard services tables successfully dropped."
