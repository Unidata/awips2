#!/bin/bash
# DR8751
# Legacy Climate migration, HMDB database removal script
#
# Do not Ctrl+C or otherwise interrupt the script while it is running as this
# may prevent proper cleanup.
#
# Required Setup:
# Run on dv1 (postgres host).
#
# Tasks:
# Deletes the hmdb database
# Migrates legacy climate support files

PSQL="/awips2/psql/bin/psql"

echo "INFO: running delta script 'removeHmdb.sh' for RODO DR 8751."

echo "INFO: removing HMDB Database."
${PSQL} --username awipsadmin --dbname metadata --command "DROP DATABASE IF EXISTS hmdb;"
if [ $? -ne 0 ]; then
    echo "FATAL: removing hmdb database failed!"
    echo "ERROR: delta script for RODO DR 8751 complete with errors."
    exit 1
fi

# move legacy climate files
echo "INFO: moving legacy climate files."

# source for AW_SITE_IDENTIFIER
source /awips2/edex/bin/setup.env

oldClimateDir=/awips/adapt/climate/support
newClimateDir=/awips2/edex/data/utility/common_static/site/${AW_SITE_IDENTIFIER}/climate/support

if  [ ! -d ${oldClimateDir} ] ; then
    echo "INFO: ${oldClimateDir} directory does not exist, there is no legacy climate directory to migrate."
    echo "INFO: delta script for RODO DR 8751 complete."
    exit 0
fi

mkdir --parents ${newClimateDir}
if [ ! -d ${newClimateDir} ] ; then
    echo "ERROR: unable to create ${newClimateDir}."
    echo "FATAL: the legacy climate migration has failed."
    echo "ERROR: delta script for RODO DR 8751 complete with errors."
    exit 1
fi

mv "${oldClimateDir}"/* "${newClimateDir}"

if [ $? -ne 0 ] ; then
    echo "ERROR: moving ${oldClimateDir} to ${newClimateDir} failed."
    echo "FATAL: the legacy climate migration has failed."
    echo "ERROR: delta script for RODO DR 8751 complete with errors."
    exit 1
fi

rm --recursive --force ${oldClimateDir}

if [ $? -ne 0 ] ; then
    echo "WARN: deleting ${oldClimateDir} directory failed."
fi

echo "INFO: delta script for RODO DR 8751 complete."
