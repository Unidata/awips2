#! /bin/bash
# Removes FFMP bin files created too large for regenerating more reasonably
echo "RODO DR #7627: remove bad FFMP bin files for proper regeneration"

rm -rf /awips2/edex/data/utility/common_static/configured/*/ffmp/sources/*

echo "RODO DR #7627: Successfully removed FFMP bin files"
echo "Please start/restart ingestDat JVM to restart them"
