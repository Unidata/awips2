#!/bin/sh
#----------------------------------------------------------------------
# Auto shutdown script for a qpid server. 
#----------------------------------------------------------------------

PAUSE=3
PID=`/sbin/pidof qpidd`
DATE=`date`

echo "**************************************************************************************"
echo "Stopping QPID - $DATE"
echo "**************************************************************************************"

service edex_qpid stop

sleep ${PAUSE}

if [ -z "${PID}" ]; then
   echo "    WARNING: Qpid daemon not running"
   let ERROR_COUNT=ERROR_COUNT+1
else
   echo "    Qpid daemon running"
fi

DATE=`date`

echo "**************************************************************************************"
echo "QPID Stopped At $DATE"
echo "**************************************************************************************"
echo ""

exit

