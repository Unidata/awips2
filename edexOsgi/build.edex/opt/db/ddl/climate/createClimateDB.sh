#!/bin/bash
##
# This script will be good for migrating from hmdb to climate
# TODO: This script need to be updated when the hmdb is retired
# -----------------------------------------------------------------
# ! script to create the Climate database
# !
# ! $1 = psql install directory
# ! $2 = postgresql install directory
# ! $3 = DB port number
# ! $4 = username
# ! $5 = script directory
# ! $6 = log file path
# !
# -----------------------------------------------------------------
echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Creating Climate Database..."
echo "--------------------------------------------------------------------------------"
${1}/bin/psql -d postgres -U ${4} -q -p ${3} -f ${5}/createClimateDb.sql >> ${6} 2>&1
echo "--------------------------------------------------------------------------------"
echo "\| Copying schema and data from hmdb to climate DB ... "
echo "--------------------------------------------------------------------------------"
${2}/bin/pg_dump -U ${4} hmdb --clean | ${1}/bin/psql -U ${4} climate >> ${6} 2>&1

echo "\| Creating cpg_session and sent_prod_record tables..."
echo "--------------------------------------------------------------------------------"
${1}/bin/psql -d climate -U ${4} -q -p ${3} -f ${5}/createAdditionalTables.sql >> ${6} 2>&1