#!/bin/bash


# 1917 Removes old aggregate format/layout
echo "Removing old stat aggregates"
rm -rf /awips2/edex/data/utility/common_static/site/*/stats/aggregates

# run full vacuum on stats table, code keeps table more stable
PSQL="/awips2/psql/bin/psql"
echo "Running full vacuum on stats"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE events.stats;"
