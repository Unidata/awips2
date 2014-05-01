#!/bin/bash
#----------------------------------------------------------------------
# Auto uninstall script for a AWIPS II EDEX server. 
#----------------------------------------------------------------------

LOGS_SAVE_DIR=/awipscm/EdexLogs
CONFIGURATION_DIR=/awipscm/clusterDeployment
INSTALL_FOLDER=/awips2

PAUSE=3

DAEMON_FILE=/etc/init.d/edex_camel
#
#----------------------------------------------------------------------
# basic logic:
#     1) shuts down data flow, if present
#     2) stops instaled EDEX software
#     3) saves logs
#     4) saves selected hydro data
#     5) uninstalls installed software
#
# defaults:
#    default install works on awips-dev1
#
#----------------------------------------------------------------------
# Args:
# $1...$n :: components to uninstall 
#----------------------------------------------------------------------
# limitations:
#   1) this script must be run as root
#
#----------------------------------------------------------------------

# space separated -- may need irt?
if [ $# -eq 0 ]; then
   INSTALLED_COMPONENTS="edex java python hydroapps native"
else
   INSTALLED_COMPONENTS=${1}
   shift 1
   for a in $@; do
      INSTALLED_COMPONENTS="${INSTALLED_COMPONENTS} ${a}"
   done
fi
DATE=`date`

echo "**************************************************************************************"
echo "Dev Standalone Auto Installer software shutdown/uninstall Log For $DATE"
echo "**************************************************************************************"
# source the environment defaults -- allows flexiblity for testing, etc...
if [ -f "${CONFIGURATION_DIR}/defaults" ]; then
   echo "loading properties from 'defaults'"
   . ${CONFIGURATION_DIR}/defaults
else
   echo "using default properties"
fi

echo "cd ${CONFIGURATION_DIR}"
cd ${CONFIGURATION_DIR}

echo "EDEX/Camel installed in '${INSTALL_FOLDER}'"
HOST=`echo $HOSTNAME | cut -d. -f1`
echo "--------------------------------------------------------------------------------------"
echo "Stopping Edex on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"

echo "----Stopping the data flow----"
echo "----Dataflow stopped, stopping the EDEX services----"
echo "----Stopping EDEX/Camel----"
if [ -e ${DAEMON_FILE} ]; then
   echo "execution'${DAEMON_FILE} stop &'"
   ${DAEMON_FILE} stop &
   echo "----Waiting ${PAUSE} seconds for all EDEX processes to stop----"
   sleep ${PAUSE}
else
   echo "unable to find '${DAEMON_FILE}', will continue"
fi

echo "----Checking to see if the EDEX Wrapper has stopped----"
echo "checking '/sbin/pidof wrapper'"
PID=`/sbin/pidof wrapper`
if [ "$PID" != "" ]
then
	echo "----Found EDEX Wrapper PID killing $PID----"
	echo "kill -9 ${PID}"
	kill -9 ${PID}
else
	echo "----No EDEX Wrapper PID found----"
fi

echo "----Checking for any EDEX/Camel processes that did not stop correctly----"
PID=`/sbin/pidof java`
while [ -n "${PID}" ]; do
   kill_count=0
   for a in ${PID}; do
      VAL=`ps ax -o pid,args | grep ${a} | grep "edex.esb.Main"`
      if [ -n "${VAL}" ]; then
         echo "----Found EDEX/Camel PID --- killing ${a} ----"
	     echo "    kill -9 ${a}"
         kill -9 ${a}
         let kill_count=1
         sleep 10
      fi
   done
   if [ ${kill_count} -eq 0 ]; then
      echo ""
      echo "    no EDEX/Camel processes found"
      PID=
   else
      echo -n "*"
      PID=`/sbin/pidof java`
   fi
done

echo ""
echo "----No more EDEX/Camel processes running----"

echo "----Checking for any running HPE process ----"

PID=`ps aux | grep -v grep | grep start_hpe | awk '{print $2}'`
while [ -n "${PID}" ]; do
   kill_count=0
   if [ -n "${PID}" ]; then
         echo "----Found HPE --- killing ${PID} ----"
         kill -9 ${PID}
         let kill_count=1
         sleep 10
   fi
   if [ ${kill_count} -ne 0 ]; then
#      PID=`ps ax -o ppid,args | grep -v grep | grep start_hpe`
     PID=`ps aux | grep -v grep | grep start_hpe | awk '{print $2}'`
   fi
done
echo "----No HPE processes running----"

sleep 10
#remove the link to the topo directory so the uninstall won't
#follow it and delete the topo file and the lock file
TOPO_DIR=${INSTALL_FOLDER}/edex/data/hdf5/topo
echo "checking for topo link '${TOPO_DIR}'"
if [ -L ${TOPO_DIR} ]; then
   echo "deleting topo directory link"
   rm ${TOPO_DIR}
fi
echo "----Services stopped----"

echo "----creating log file archive----"
DATE_DIR=`date +"%m-%d-%y"`
echo "log archive date=${DATE_DIR}"
HOST_STR=`echo ${HOST} | cut -d- -f2`
LOGS_DIR=${LOGS_SAVE_DIR}/${HOST}/${DATE_DIR}
echo "archiving logs to '${LOGS_DIR}'"
if [ ! -d ${LOGS_DIR} ]; then
	echo "creating logs dir '${LOGS_DIR}'"
    mkdir -p ${LOGS_DIR}
fi

echo "----copy the edex logs----"
echo "copying system logs for 'EDEX'"
echo "   cp -R ${INSTALL_FOLDER}/edex/logs/* ${LOGS_DIR}"
cp -R ${INSTALL_FOLDER}/edex/logs/* ${LOGS_DIR}
echo "copying error logs for 'EDEX'"
echo "   cp -R ${INSTALL_FOLDER}/edex/bin/*.log ${LOGS_DIR}"
cp -R ${INSTALL_FOLDER}/edex/bin/*.log ${LOGS_DIR}

echo "----Uninstalling the EDEX/Camel software----"
echo "components: ${INSTALLED_COMPONENTS}"

yum groupremove 'AWIPS II Processing Server' -y

RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: The 'remove_all_awips2.sh' Has Failed On ${HOST}."
   exit 1 
fi

if [ -d ${INSTALL_FOLDER} ]; then
        echo "Cleaning up any left over files"
        rm -rf ${INSTALL_FOLDER}
else
        echo "Did not find ${INSTALL_FOLDER}"
fi

	echo "----Killing any java processes to stop the uninstaller----"
	echo "PID='/sbin/pidof java'"
	PID=`/sbin/pidof java`

	while [ "$PID" != "" ]
	do
        	echo "----Found java PID killing ${PID} ----"
		echo "`ps -p ${PID}`"
		echo "kill -9 ${PID}"
        	kill -9 ${PID}
        	sleep 10
		echo "PID='/sbin/pidof java'"
        	PID=`/sbin/pidof java`
	done
	echo "----No more java processes running----"

	echo "----Deleting the ${INSTALL_FOLDER} directory----"
	rm -rf ${INSTALL_FOLDER}

	echo "----Sending Warning Email----"
	echo "`uname -n`: At `date`, Dev Autodeployment script failed to uninstall Edex/Camel normally--Uninstall was killed and awips directory was deleted manually, before continuing with the install--${HOST} should be checked." > email.txt
	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Beth_C_Lichtenberg@raytheon.com < email.txt

DATE=`date`

echo "-------------------------------------------"
echo "Auto Installer Stop and Uninstall Completed On ${HOST} At ${DATE}"
echo "-------------------------------------------"
echo ""
exit

