#!/bin/bash
#----------------------------------------------------------------------
# auto deploy script for a AWIPS II server. Where possible, machine
# dependent values are obtained at runtime.
#
#----------------------------------------------------------------------
# Default values for these script variables may set using 'defaults'
INSTALL_FOLDER=/awips
DB_MACHINE=awips-devdb
SOURCE_FOLDER=/awipscm/installers
LOGS_SAVE_DIR=/awipscm/EdexLogs
DAEMON_FILE=/etc/init.d/edex_camel
PAUSE=30
RADAR_SERVER=${INSTALL_FOLDER}/rcm
RADAR_SERVER_BAK=/RadarServer-Bak
RUN_FROM_DIR=/installer
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
if [ -f "${RUN_FROM_DIR}/defaults" ]; then
   echo "loading properties from 'defaults'"
   . ${RUN_FROM_DIR}/defaults
else
   echo "using default properties"
fi

echo "cd ${RUN_FROM_DIR}"
cd ${RUN_FROM_DIR}

echo "EDEX/Camel installed in '${INSTALL_FOLDER}'"
echo "DB_MACHINE=${DB_MACHINE}"
HOST=`echo $HOSTNAME | cut -d. -f1`
echo "--------------------------------------------------------------------------------------"
echo "Stopping Edex on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"

echo "----Stopping the data flow----"
# determine if ldm is running on this machine -- if so, stop it
echo "----Stopping the LDM Server----"
RUN_COUNT=`/bin/ps aux | /bin/grep ldm | /bin/grep -v grep | /usr/bin/wc -l`
if [ $RUN_COUNT -ne 0 ]; then
   echo "    stopping ldm..."
   echo "su - ldm -c \"ldmadmin stop\""
   su - ldm -c "ldmadmin stop"
else
   echo "    ldm not running, will continue"
fi

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
#PID=`ps ax -o ppid,args | grep -v grep | grep start_hpe`
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

echo "----Stopping Qpid----"
echo "/etc/init.d/edex_qpid"
/etc/init.d/edex_qpid stop
echo "----Verifying that Qpid stopped correctly----"
PID=`/sbin/pidof qpidd`
while [ -n "${PID}" ]; do
   for a in ${PID}; do
      echo "----Found Qpid PID --- killing ${a} ----"
      echo "kill -9 ${a}"
      kill -9 ${a}
      sleep 10
   done
   PID=`/sbin/pidof qpidd`
done
echo "----No Qpid processes running----"

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
LOGS_DIR=${LOGS_SAVE_DIR}/${HOST_STR}/${DATE_DIR}
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

# start ticket 1575 mods -- mwf
# save off the existing hydro apps data directories
SAVE_FILE=/tmp/hydro-sve.tar
echo "----Saving Hydro Data----"
if [ -f $SAVE_FILE ];then
	/bin/rm -f $SAVE_FILE
fi
/bin/tar -cf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5/hydroapps precip_proc/local/data whfs/local/data
# end ticket 1575 mods -- mwf

# save off the existing aviation directory
SAVE_FILE=/tmp/aviation-sve.tar
echo "----Saving Aviation Data----"
if [ -f $SAVE_FILE ];then
	/bin/rm -f $SAVE_FILE
fi
/bin/tar -cf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5 aviation

echo "----Uninstalling the EDEX/Camel software----"
echo "components: ${INSTALLED_COMPONENTS}"
# this will need to be more selective of the deletes
# note: under the component installer model, we need
#       to uninstall the components individually. Once 
#       that is done, we can uninstall the overall
#       installer scripts.  This will also clean up
#       any leftover directories...
# As a further complication, the IzPak uninstaller is
# written so that it runs completely asynchronistically;
# this means we can't simply wait as the uninstaller runs...

echo "----running uninstallers----"
if [ -f $INSTALL_FOLDER/Uninstaller/uninstaller.jar ]; then
   # use the legacy uninstaller if it exists 
   # -- note this is hard coded to the path
   # --      this branch is provided to allow for an automatic
   # --      switch to the new installers 
   echo "running legacy uninstaller"
   echo "   /installer/install_files/jre/bin/java -jar $INSTALL_FOLDER/Uninstaller/uninstaller.jar -c -f"
   /installer/install_files/jre/bin/java -jar $INSTALL_FOLDER/Uninstaller/uninstaller.jar -c -f
else
   # no legacy uninstaller, delete components
   # note do not use force on the components 
   #  -- server uninstaller will do any additional cleanup
   # then delete the group installer scripts and clean up
   MASTER_UNINSTALLER=$INSTALL_FOLDER/Uninstaller/awips-server-uninstaller.jar
   echo "----executing bundle uninstaller '${MASTER_UNINSTALLER}----"
   if [ -f ${MASTER_UNINSTALLER} ]; then
      echo "   ${SOURCE_FOLDER}/install_files/jre/bin/java -jar ${MASTER_UNINSTALLER} -c -f"
      ${SOURCE_FOLDER}/install_files/jre/bin/java -jar ${MASTER_UNINSTALLER} -c -f
   fi
fi

echo "----Waiting for the uninstaller to clean up the previous install.----"
TIMEOUT_COUNT=0
while [ -d $INSTALL_FOLDER -a $TIMEOUT_COUNT -lt 200 ]; do
  sleep 5
  let TIMEOUT_COUNT=TIMEOUT_COUNT+1 
  echo "----${INSTALL_FOLDER} folder still exists; pausing. $TIMEOUT_COUNT----"
done

if [ $TIMEOUT_COUNT = 200 ]; then
  	echo "****TIMED OUT WAITING FOR UNINSTALLER TO DELETE ${INSTALL_FOLDER} FOLDER...****"

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
	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Kace_Chrisman@raytheon.com < email.txt
	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" mark_w_fegan@raytheon.com < email.txt
	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Bryan_J_Rockwood@raytheon.com < email.txt
	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Scott_Risch@raytheon.com < email.txt
        mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Bryan.Kowal@raytheon.com < email.txt
fi


DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto Installer Stop and Uninstall Completed On ${HOST} At ${DATE}"
echo "--------------------------------------------------------------------------------------"
echo ""

exit

