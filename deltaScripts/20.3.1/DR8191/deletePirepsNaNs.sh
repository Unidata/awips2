#! /bin/bash
# This script should be run on dv1
# It deletes any pirep records with NaN in the latitude/longitude fields
echo "Running delta script ${0} for RODO DR 8191"
/awips2/psql/bin/psql -U awips -d metadata -c "delete from pirep where latitude = 'NaN' or longitude = 'NaN';"
echo "Delta script ${0} complete"

