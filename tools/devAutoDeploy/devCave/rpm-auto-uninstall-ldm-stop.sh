#!/bin/sh
#----------------------------------------------------------------------
# Auto uninstall and shutdown script for a ldm server. 
#----------------------------------------------------------------------
LDMUSER=ldm
DATE=`date`

echo "**************************************************************************************"
echo "Auto LDM Stop - $DATE"
echo "**************************************************************************************"

echo $LDMUSER
echo "----Stopping the ldm:----"

#echo "su - ${LDMUSER} -c \"ldmadmin stop\""
su - ldm -c "ldmadmin stop"

#Remove the symbolic link
rm /usr/local/ldm

#Uninstall LDm
yum groupremove 'AWIPS II LDM Server' -y

#Verify all RPMs are uninstalled
# rpm -qa | grep awips2
#Remove existing rpms
#rpm -e [name of rpm]


DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto LDM Stop Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit

