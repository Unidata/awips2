#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Auto Int Server Stop & Uninstall Started - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

echo "----Stopping the dataflow:----"

echo "su - ldm -c \"ldmadmin stop\""
su - ldm -c "ldmadmin stop"

echo "/etc/init.d/edex_rcm stop"
/etc/init.d/edex_rcm stop

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

echo "----Saving Hydro Data----"
if [ -f $HYDROSAVEFILE ];then
	/bin/rm -f $HYDROSAVEFILE
fi
/bin/tar -cf $HYDROSAVEFILE -C $EDEXINSTALLFOLDER/edex/data/hdf5/hydroapps precip_proc/local/data whfs/local/data

echo "---Cleaning out files from procesing, uEngine and hdf5 folders---"
# remove processing files as root user
find ${EDEXINSTALLFOLDER}/edex/data/processing -name "*" -exec rm -f {} \;
# remove uEngine files as edex user
su - ${EDEXUSER} -c "find ${EDEXINSTALLFOLDER}/edex/data/processing -name \"*\" -exec rm -f {} \;"
# for hdf5 first pass remove as root user, then remove remaining files as edex user
rm -rf ${EDEXINSTALLFOLDER}/edex/data/hdf5/hydroapps
rm -rf ${EDEXINSTALLFOLDER}/edex/data/hdf5/topo
su - ${EDEXUSER} -c "find ${EDEXINSTALLFOLDER}/edex/data/hdf5 -name \"*\" -exec rm -rf {} \;"
find ${EDEXINSTALLFOLDER}/edex/data/hdf5 -name "*" -exec rm -rf {} \;

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
if [ ! -d ${EDEXSERVERBACKUPLOGFOLDER}/${DATEDIR} ]
then
	echo "mkdir ${EDEXSERVERBACKUPLOGFOLDER}/${DATEDIR}"
       mkdir ${EDEXSERVERBACKUPLOGFOLDER}/${DATEDIR}
fi
echo "copying regular edex logs"
echo "cp -R $EDEXINSTALLFOLDER/edex/logs/* ${EDEXSERVERBACKUPLOGFOLDER}/${DATEDIR}/"
cp -R $EDEXINSTALLFOLDER/edex/logs/* ${EDEXSERVERBACKUPLOGFOLDER}/${DATEDIR}/
echo "copy the edex error logs"
echo "cp $EDEXINSTALLFOLDER/edex/bin/*.log ${EDEXSERVERBACKUPLOGFOLDER}/${DATEDIR}/"
cp $EDEXINSTALLFOLDER/edex/bin/*.log ${EDEXSERVERBACKUPLOGFOLDER}/${DATEDIR}/


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
	echo "`uname -n`: At `date`, Int Server Auto Stop script failed to stop Edex normally--Uninstall was killed and the ${EDEXINSTALLFOLDER} directory was deleted manually, before continuing with the install--the server should be checked." > email.txt
	mail -s "Warning EDEX Int Server uninstall failed on `uname -n`" ${EMAILLIST} < email.txt
fi

DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto Int Server Stop & Uninstall Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit
