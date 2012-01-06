#!/bin/sh
#----------------------------------------------------------------------
# Auto uninstall and shutdown script for a qpid server. 
#----------------------------------------------------------------------
PAUSE=8
PID=`/awips2/qpid/sbin/pidof qpidd`
DATE=`date`

echo "**************************************************************************************"
echo "Stopping QPID - $DATE"
echo "**************************************************************************************"

#Shutdown qpid server
service awips2_qpidd_cluster stop

sleep ${PAUSE}

if [ -z "${PID}" ]; then
   echo "    WARNING: Qpid daemon not running"
   let ERROR_COUNT=ERROR_COUNT+1
else
   echo "    Qpid daemon running"
fi
DATE=`date`

#Uninstall qpid server
yum groupremove 'AWIPS II Message Broker Server' -y
rm -Rf /awips2

echo "**************************************************************************************"
echo "QPID Stopped At $DATE"
echo "**************************************************************************************"
echo ""

exit

