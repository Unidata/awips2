#!/bin/sh
#----------------------------------------------------------------------
# Auto install and startup script for the LDM server. 
#----------------------------------------------------------------------

LDMUSER=ldm
DATE=`date`

echo "**************************************************************************************"
echo "Starting AWIPS LDM - $DATE"
echo "**************************************************************************************"

#Create a symbolic link
ln -s /usr/local/ldm-6.8.1 ldm

#Install using YUM
yum groupinstall 'AWIPS II LDM Server' -y

echo "su - ${LDMUSER} -c \"ldmadmin start\" "
su - ${LDMUSER} -c "ldmadmin start"
	
echo "----LDM restarted----"

DATE=`date`

echo "**************************************************************************************"
echo "EDEX LDM Start Completed At $DATE"
echo "**************************************************************************************"
echo ""
exit
