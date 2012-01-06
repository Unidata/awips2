#!/bin/sh

# Script to stop the EDEX client

DATE=`date`

echo "**************************************************************************************"
echo "Auto Int Client Stop & Uninstall Started - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

echo "----Stopping Camel----"
echo "/etc/init.d/edex_camel stop &"
/etc/init.d/edex_camel stop &

echo "----Waiting 30 seconds for all java processes to stop----"
sleep 30

echo "----Checking to see if the EDEX Wrapper has stopped----"
echo "PID='/sbin/pidof wrapper'"
PID=`/sbin/pidof wrapper`
if [ "$PID" != "" ]
then
	echo "----Found EDEX Wrapper PID killing $PID----"
	echo "kill -9 ${PID}"
	kill -9 ${PID}
else
	echo "----No EDEX Wrapper PID found----"
fi

echo "----Checking for any java processes that did not stop correctly----"
echo "PID='/sbin/pidof java'"
PID=`/sbin/pidof java`

while [ "$PID" != "" ]
do
        echo "----Found java PID killing $PID ----"
	echo "`ps -p ${PID}`"
	echo "kill -9 ${PID}"
        kill -9 ${PID}
        sleep 10
	echo "PID='/sbin/pidof java'"
        PID=`/sbin/pidof java`
done
echo "----No more java processes running----"

echo "----Services stopped----"

echo "----Umounting from NAS----"
umount -l ${EDEXINSTALLFOLDER}/edex/data/processing
umount -l ${EDEXINSTALLFOLDER}/edex/data/uEngine
umount -l ${EDEXINSTALLFOLDER}/edex/data/hdf5
echo "mount -l (verify umount worked)"
mount
echo ""

echo "----Copying log files----"

echo "DATEDIR=`date +\"%m-%d-%y\"`"
DATEDIR=`date +"%m-%d-%y"`
if [ ! -d ${EDEXCLIENTBACKUPLOGFOLDER}/${DATEDIR} ]
then
	echo "mkdir ${EDEXCLIENTBACKUPLOGFOLDER}/${DATEDIR}"
       mkdir ${EDEXCLIENTBACKUPLOGFOLDER}/${DATEDIR}
fi
echo "copying regular edex logs"
echo "cp -R $EDEXINSTALLFOLDER/edex/logs/* ${EDEXCLIENTBACKUPLOGFOLDER}/${DATEDIR}/"
cp -R $EDEXINSTALLFOLDER/edex/logs/* ${EDEXCLIENTBACKUPLOGFOLDER}/${DATEDIR}/
echo "copy the edex error logs"
echo "cp $EDEXINSTALLFOLDER/edex/bin/*.log ${EDEXCLIENTBACKUPLOGFOLDER}/${DATEDIR}/"
cp $EDEXINSTALLFOLDER/edex/bin/*.log ${EDEXCLIENTBACKUPLOGFOLDER}/${DATEDIR}/

echo "----Running the uninstaller----"
echo "${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $EDEXINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f"
${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $EDEXINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f

echo "----Waiting for the uninstaller to clean up the previous install.----"
TIMEOUTCOUNT=0
while [ -d $EDEXINSTALLFOLDER -a $TIMEOUTCOUNT -lt 200 ]
do
  sleep 5
  let TIMEOUTCOUNT=TIMEOUTCOUNT+1 
  echo "----${EDEXINSTALLFOLDER} folder still exists; pausing. $TIMEOUTCOUNT----"
done

if [ $TIMEOUTCOUNT = 200 ]
then
  	echo "****TIMED OUT WAITING FOR UNINSTALLER TO DELETE ${INSTALLFOLDER} FOLDER...****"

	echo "----Killing any java processes to stop the uninstaller----"
	echo "PID='/sbin/pidof java'"
	PID=`/sbin/pidof java`

	while [ "$PID" != "" ]
	do
        	echo "----Found java PID killing $PID ----"
		echo "`ps -p ${PID}`"
		echo "kill -9 ${PID}"
        	kill -9 ${PID}
        	sleep 10
		echo "PID='/sbin/pidof java'"
        	PID=`/sbin/pidof java`
	done
	echo "----No more java processes running----"

	echo "----Deleting the ${EDEXINSTALLFOLDER} directory----"
	rm -rf ${EDEXINSTALLFOLDER}

	echo "----Sending Warning Email----"
	echo "`uname -n`: At `date`, Int Client Auto Stop script failed to stop Edex normally--Uninstall was killed and the ${EDEXINSTALLFOLDER} directory was deleted manually, before continuing with the install--the client should be checked." > email.txt
	mail -s "Warning EDEX Int Client uninstall failed on `uname -n`" ${EMAILLIST} < email.txt
fi

DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto Int Client Stop & Uninstall Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit
