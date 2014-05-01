#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Auto Radar Server Stop & Uninstall Started - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

echo "----Checking for running RadarServer:----"
COUNT=`ps aux | grep -c "RadarServer"`
if [ $COUNT -gt 1 ]
then
	echo "Found running RadarServer...stopping"
	echo "/etc/init.d/edex_rcm stop"
	/etc/init.d/edex_rcm stop
else
	echo "No RadarServer Running"
fi

if [ -d ${RCMBAKFOLDER} ]
then
	echo "Removing ${RCMBAKFOLDER}"
	rm -rf ${RCMBAKFOLDER}
else
	echo "No previous ${RCMBAKFOLDER} to cleanup"
fi

if [ -d ${RCMINSTALLFOLDER} ]
then
	echo "Backingup ${RCMINSTALLFOLDER} to ${RCMBAKFOLDER}"
	mv ${RCMINSTALLFOLDER} ${RCMBAKFOLDER}
else
	echo "**** Warning: No existing ${RCMINSTALLFOLDER} to backup, no config files to copy after installing ****"
fi

DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto Radar Server Stop & Uninstall Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit
