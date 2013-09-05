#!/bin/bash
# DR #2316 drop pirep,ncpirep,airep,ncairep

PSQL="/awips2/psql/bin/psql"

${PSQL} -U awips -d metadata -c "delete from plugin_info where name = 'pirep' or name = 'ncpirep';"
${PSQL} -U awips -d metadata -c "drop table pirep, pirep_anc_data, ncpirep, ncpirep_anc_data;"
