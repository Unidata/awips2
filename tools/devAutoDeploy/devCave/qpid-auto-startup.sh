#!/bin/sh
#----------------------------------------------------------------------
# Auto startup script for a qpid server. 
#--------------------------------------------------------------------

PAUSE=8
PID=`/sbin/pidof qpidd`
DATE=`date`

echo "**************************************************************************************"
echo "Starting QPID - $DATE"
echo "**************************************************************************************"


echo "/etc/init.d/edex_qpid start"

/etc/init.d/edex_qpid start

sleep ${PAUSE}

if [ -z "${PID}" ]; then
   echo "    WARNING: Qpid daemon not running"
   let ERROR_COUNT=ERROR_COUNT+1
else
   echo "    Qpid daemon running"
fi
DATE=`date`

echo "**************************************************************************************"
echo "QPID Completed At $DATE"
echo "**************************************************************************************"
echo ""

exit

