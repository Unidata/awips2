#!/bin/sh
#----------------------------------------------------------------------
# Auto installation and startup script for a qpid server. 
#--------------------------------------------------------------------

PAUSE=8
PID=`/awips2/qpid/sbin/pidof qpidd`
DATE=`date`

echo "**************************************************************************************"
echo "Installing QPID - $DATE"
echo "**************************************************************************************"


echo "awips2_qpidd_cluster installing"

yum groupinstall 'AWIPS II Message Broker Server' -y

echo "**************************************************************************************"
echo "Starting QPID"
echo "**************************************************************************************"
service awips2_qpidd_cluster start

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

