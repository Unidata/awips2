#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Starting CAVE Configuration - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

echo "Setting permissions on basemaps folder"
chmod -R 777 ${CAVEINSTALLFOLDER}/basemaps

DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto CAVE Configuration Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit
