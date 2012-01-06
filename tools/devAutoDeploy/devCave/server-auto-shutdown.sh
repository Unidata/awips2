#!/bin/sh
#----------------------------------------------------------------------
# Auto shutdown script for the EDEX server. 
#----------------------------------------------------------------------


PID='/sbin/pidof java'

DATE=`date`

echo "**************************************************************************************"
echo "Auto Server Stop - $DATE"
echo "**************************************************************************************"

echo "----Checking for running Server:----"
if [ -z "${PID}" ]; then 
	echo "Found running server...stopping"
	echo "/etc/init.d/edex_camel stop"
    service edex_camel stop
else
	echo "No RadarServer Running"
fi
DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto Server Stop Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit

