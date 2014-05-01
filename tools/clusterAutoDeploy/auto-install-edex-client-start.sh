#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Starting AWIPS on EDEX Client - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

echo "Mounting to processing on ${NASSERVER}"
mount ${NASSERVER}:/processing $EDEXINSTALLFOLDER/edex/data/processing -o noac,nfsvers=3
echo "Mounting to uEngine on ${NASSERVER}"
mount ${NASSERVER}:/uEngine $EDEXINSTALLFOLDER/edex/data/uEngine -o noac,nfsvers=3
echo "Mounting to hdf5 on ${NASSERVER}"
mount ${NASSERVER}:/hdf5 $EDEXINSTALLFOLDER/edex/data/hdf5 -o nfsvers=3

echo "mount -l (verify mount of processing, uEngine, and hdf5 directories worked)"
mount
echo ""

echo "----Starting the EDEX services----"

echo "----Starting Camel----"
echo "/etc/init.d/edex_camel start"
/etc/init.d/edex_camel start

echo "----Waiting for the EDEX ESB to be operational----"
COUNT=0
TIMEOUTCOUNT=0
while [ $COUNT -eq 0 -a $TIMEOUTCOUNT -lt 200 ]
do
	let TIMEOUTCOUNT=TIMEOUTCOUNT+1
	if [ -e $EDEXINSTALLFOLDER/edex/logs/edex*.log ]
	then
        	COUNT=`grep -c "* EDEX ESB is now operational" $EDEXINSTALLFOLDER/edex/logs/edex*.log`
		echo "----Still haven't gotten 'EDEX ESB is now operational', pausing. $TIMEOUTCOUNT----"
	else
		echo "EDEX Log doesn't exist yet, pausing. $TIMEOUTCOUNT----"
		COUNT=0
        fi
	sleep 5
	
done
if [ $TIMEOUTCOUNT = 200 ]
then
	echo "----Timed out waiting for EDEX ESB to be operational...exiting----"	
	echo "----Sending Warning Email----"
	echo "`uname -n`: At `date`, Int Client Auto Start script failed to start Camel normally, before continuing with the install--the client should be checked." > email.txt
	mail -s "Warning EDEX Int Client start failed on `uname -n`" ${EMAILLIST} < email.txt	
	exit 99
fi
echo "----Found 'EDEX ESB is now operational' in EDEX Camel log.----"

DATE=`date`

echo "**************************************************************************************"
echo "EDEX Server Start Completed At $DATE"
echo "**************************************************************************************"
echo ""

exit

