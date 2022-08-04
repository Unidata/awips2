#!/bin/bash
#
# This delta script should be run on dv1
#
echo "Running delta script for RODO DR 8457"

/awips2/psql/bin/psql -U awips -d metadata -c "update parameter set unit='dBZ' where unit='dB' and abbreviation not like 'ABSRB%';"

echo "delta script for RODO 8457 complete"
