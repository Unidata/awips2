#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Auto Cave Uninstall Started - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

if [ -d ${CAVEINSTALLFOLDER} ]
then
	echo "----Running the uninstaller----"
	echo "${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $CAVEINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f"
	${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $CAVEINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f
	echo "----Waiting for the uninstaller to clean up the previous install.----"
	TIMEOUTCOUNT=0
	while [ -d $CAVEINSTALLFOLDER -a $TIMEOUTCOUNT -lt 200 ]
	do
  		sleep 5
  		let TIMEOUTCOUNT=TIMEOUTCOUNT+1 
  		echo "----${CAVEINSTALLFOLDER} folder still exists; pausing. $TIMEOUTCOUNT----"
	done

	if [ $TIMEOUTCOUNT = 200 ]
	then
  		echo "****TIMED OUT WAITING FOR UNINSTALLER TO DELETE ${CAVEINSTALLFOLDER} FOLDER...****"

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

		echo "----Deleting the ${CAVEINSTALLFOLDER} directory----"
		rm -rf ${CAVEINSTALLFOLDER}

		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, CAVE Auto Stop script failed--Uninstall was killed and the ${CAVEINSTALLFOLDER} directory was deleted manually, before continuing with the install--the cave box should be checked." > email.txt
		mail -s "Warning CAVE uninstall failed on `uname -n`" ${EMAILLIST} < email.txt
	fi
else
	echo "----No CAVE Installed----"
fi



DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto CAVE Uninstall Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit
