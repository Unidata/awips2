#! /bin/bash
#
# Removes the cave_static configured ffmp and scan menu files.
# common_static is now used and will be generated by edex request.
#
echo "RODO DR #6720: remove FFMP and SCAN cave_static/configured menu files"

rm -rf /awips2/edex/data/utility/cave_static/configured/*/menus/ffmp
rm -rf /awips2/edex/data/utility/cave_static/configured/*/menus/scan

echo "RODO DR #6720: Successfully removed FFMP and SCAN menu files."
echo "Please start/restart EDEX request JVM to regenerate them."
