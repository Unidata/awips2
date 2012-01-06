#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Auto CLI Uninstall Started - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

if [ -d ${CLIINSTALLFOLDER} ]
then
	echo "----Running the uninstaller----"
	echo "${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $CLIINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f"
	${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $CLIINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f
	echo "----Waiting for the uninstaller to clean up the previous install.----"
	TIMEOUTCOUNT=0
	while [ -d $CLIINSTALLFOLDER -a $TIMEOUTCOUNT -lt 200 ]
	do
  		sleep 5
  		let TIMEOUTCOUNT=TIMEOUTCOUNT+1 
  		echo "----${CLIINSTALLFOLDER} folder still exists; pausing. $TIMEOUTCOUNT----"
	done

	if [ $TIMEOUTCOUNT = 200 ]
	then
  		echo "****TIMED OUT WAITING FOR UNINSTALLER TO DELETE ${CLIINSTALLFOLDER} FOLDER...****"

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

		echo "----Deleting the ${CLIINSTALLFOLDER} directory----"
		rm -rf ${CLIINSTALLFOLDER}

		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, CLI Auto Stop script failed--Uninstall was killed and the ${CLIINSTALLFOLDER} directory was deleted manually, before continuing with the install--the cli box should be checked." > email.txt
		mail -s "Warning CLI uninstall failed on `uname -n`" ${EMAILLIST} < email.txt
	fi
else
	echo "----No CLI Installed (could have been uninstalled by edex)----"
fi



DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto CLI Uninstall Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit
