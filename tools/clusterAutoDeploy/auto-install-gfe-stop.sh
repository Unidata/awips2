#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Auto GFE Client Uninstall Started - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

if [ -d ${GFEINSTALLFOLDER} ]
then
	echo "----Running the uninstaller----"
	echo "${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $GFEINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f"
	${INSTALLERPATH}/install_files/jre-linux/bin/java -jar $GFEINSTALLFOLDER/Uninstaller/uninstaller.jar -c -f
	echo "----Waiting for the uninstaller to clean up the previous install.----"
	TIMEOUTCOUNT=0
	while [ -d $GFEINSTALLFOLDER -a $TIMEOUTCOUNT -lt 200 ]
	do
  		sleep 5
  		let TIMEOUTCOUNT=TIMEOUTCOUNT+1 
  		echo "----${GFEINSTALLFOLDER} folder still exists; pausing. $TIMEOUTCOUNT----"
	done

	if [ $TIMEOUTCOUNT = 200 ]
	then
  		echo "****TIMED OUT WAITING FOR UNINSTALLER TO DELETE ${GFEINSTALLFOLDER} FOLDER...****"

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

		echo "----Deleting the ${GFEINSTALLFOLDER} directory----"
		rm -rf ${GFEINSTALLFOLDER}

		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, GFE Client Auto Stop script failed--Uninstall was killed and the ${GFEINSTALLFOLDER} directory was deleted manually, before continuing with the install--the GFE Client box should be checked." > email.txt
		mail -s "Warning GFE Client uninstall failed on `uname -n`" ${EMAILLIST} < email.txt
	fi
else
	echo "----No GFE Client Installed----"
fi



DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto GFE Client Uninstall Completed At $DATE"
echo "--------------------------------------------------------------------------------------"
echo ""

exit
