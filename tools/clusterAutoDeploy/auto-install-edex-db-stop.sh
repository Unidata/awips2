##!/bin/sh

echo "**************************************************************"
echo " Stopping and Uninstalling AWIPS on DB Server"
echo "**************************************************************"
echo "Getting variables from env.txt"
. env.txt

echo "----Stopping IRT Server----"
echo "/etc/init.d/edex_irt stop"
/etc/init.d/edex_irt stop
wait

echo "----Waiting 30 seconds for the irt processes to stop----"
sleep 30

echo "----Checking to see if the IRT Process has stopped----"
echo "PID='/sbin/pidof ${EDEXINSTALLFOLDER}/bin/python RoutingTableSvc.py'"
PID=`/sbin/pidof ${EDEXINSTALLFOLDER}/bin/python RoutingTableSvc.py`
if [ "$PID" != "" ]
then
	echo "----Found IRT PID killing $PID----"
	echo "kill -9 ${PID}"
	kill -9 ${PID}
else
	echo "----No IRT PID found----"
fi

echo "----Exporting IHFS Database----"
echo "${INSTALLERPATH}/dump-ihfs.sh ${EDEXUSER} ${IHFS} ${IHFS_SQL_FILE} ${POSTGRESPORT}"
${INSTALLERPATH}/dump-ihfs.sh ${EDEXUSER} ${IHFS} ${IHFS_SQL_FILE} ${POSTGRESPORT}
echo "----IHFS Export Complete----"

echo "----Stopping Postgres----"
echo "/etc/init.d/edex_postgres_${POSTGRESPORT} stop &"
/etc/init.d/edex_postgres_${POSTGRESPORT} stop
wait

echo "----Waiting 30 seconds for the PostgreSQL process to stop----"
sleep 30

echo "----Checking to see if the PostgreSQL Process has stopped----"
echo "PID='/sbin/pidof postmaster'"
PID=`/sbin/pidof postmaster`
if [ "$PID" != "" ]
then
	echo "----Found PostgreSQL PID killing $PID----"
	echo "kill -9 ${PID}"
	kill -9 ${PID}
else
	echo "----No PostgreSQL PID found----"
fi

echo "----DB Services stopped----"


echo "DATEDIR=`date +\"%m-%d-%y\"`"
DATEDIR=`date +"%m-%d-%y"`
if [ ! -d ${EDEXDBBACKUPLOGFOLDER}/${DATEDIR} ]
then
	echo "mkdir ${EDEXDBBACKUPLOGFOLDER}/${DATEDIR}"
       mkdir ${EDEXDBBACKUPLOGFOLDER}/${DATEDIR}
fi
echo "copying regular edex logs"
echo "cp -R $EDEXINSTALLFOLDER/edex/logs/* ${EDEXDBBACKUPLOGFOLDER}/${DATEDIR}/"
cp -R $EDEXINSTALLFOLDER/edex/logs/* ${EDEXDBBACKUPLOGFOLDER}/${DATEDIR}/


echo "----Running the database uninstaller----"
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
	echo "`uname -n`: At `date`, Int DB Auto Stop script failed to stop Edex normally--Uninstall was killed and the ${EDEXINSTALLFOLDER} directory was deleted manually, before continuing with the install--the DB Server should be checked." > email.txt
	mail -s "Warning EDEX Int DB uninstall failed on `uname -n`" ${EMAILLIST} < email.txt
fi

DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto Installer DB Stop and Uninstall Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
