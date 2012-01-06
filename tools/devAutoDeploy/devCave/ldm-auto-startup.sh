#!/bin/sh
#----------------------------------------------------------------------
# Auto startup script for the LDM server. 
#----------------------------------------------------------------------

LDMUSER=ldm
DATE=`date`

echo "**************************************************************************************"
echo "Starting AWIPS LDM - $DATE"
echo "**************************************************************************************"

echo "LDM ${LDMUSER}"
echo "----Starting LDM----"

#echo "su - ${LDMUSER} -c \"ldmadmin start\" "
#su - ${LDMUSER} -c "ldmadmin start"
	
ldmadmin start
echo "----LDM restarted----"

DATE=`date`

echo "**************************************************************************************"
echo "EDEX LDM Start Completed At $DATE"
echo "**************************************************************************************"
echo ""

exit

