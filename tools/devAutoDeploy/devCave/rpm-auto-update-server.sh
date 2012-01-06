#!/bin/bash
#----------------------------------------------------------------------
# Auto deploy script for a AWIPS II server. Where possible, machine
# dependent values are obtained at runtime.
#
#----------------------------------------------------------------------
# Default values for these script variables may set using 'defaults'
LOGS_SAVE_DIR=/awipscm/EdexLogs
CONFIGURATION_DIR=/awipscm/clusterDeployment
INSTALL_FOLDER=/awips2

PAUSE=8

DAEMON_FILE=/etc/init.d/edex_camel
#
#----------------------------------------------------------------------
# basic logic:
#     1) shuts down EDEX processes
#     2) saves logs
#     3) saves selected hydro data
#     4) updates installed software
#     5) starts EDEX processes
#
#----------------------------------------------------------------------
# Args:
# $1...$n :: components to uninstall 
#----------------------------------------------------------------------
# limitations:
#   1) this script must be run as root
#
#----------------------------------------------------------------------

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
echo "Auto Update software shutdown/uninstall Log For $DATE"
echo "**************************************************************************************"

echo "cd ${CONFIGURATION_DIR}"
cd ${CONFIGURATION_DIR}

echo "EDEX/Camel installed in '${INSTALL_FOLDER}'"
HOST=`echo $HOSTNAME | cut -d. -f1`
echo "--------------------------------------------------------------------------------------"
echo "Stopping Edex on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"

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

sleep 10

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
yum clean all
export http_proxy=
yum update 'AWIPS II Processing Server' -y

RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: The groupupdate Has Failed On ${HOST}."
   exit 1 
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


# start ticket 1575 mods -- mwf
# restore the previously saved hydro data -- provided it exists

SAVE_FILE=/awipscm/blichtenberg/hydro1/hydro-sve.tar
echo "----Restoring Hydro Data----"
if [ -f $SAVE_FILE ];then
	/bin/tar -xf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5/hydroapps
chmod -R 777 $INSTALL_FOLDER/edex/data/hdf5/hydroapps
chown -R awips:fxalpha $INSTALL_FOLDER/edex/data/hdf5/hydroapps
fi
# end ticket 1575 mods -- mwf

SAVE_FILE=/awipscm/mnash/data/aviation/aviation-sve.tar
echo "----Restoring Aviation Data----"

if [ -f ${SAVE_FILE} ]; then
   /bin/tar -xf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5 aviation
fi
chmod -R 775 /awips2/edex/data/hdf5/aviation
chown -R awips:fxalpha /awips2/edex/data/hdf5/aviation
echo "----Software Install complete----"

cp -Rf /awipscm/clusterDeployment/bin/fxa/setup.env /awips2/fxa/bin/
cp -Rf /awipscm/clusterDeployment/bin/setup.env /awips2/edex/bin/
cp -Rf /awipscm/clusterDeployment/bin/environment.xml /awips2/edex/conf/res/base/
cp -Rf /awipscm/clusterDeployment/bin/RadarServerJMS.properties /awips2/edex/data/utility/edex_static/base/rpgenvdata/
cp -Rf /awipscm/clusterDeployment/bin/Apps_defaults /awips2/edex/data/utility/common_static/base/hydro/
DATE=`date`



DATE=`date`

echo "-------------------------------------------"
echo "Auto Installer Stop/Start and Update Completed On ${HOST} At ${DATE}"
echo "-------------------------------------------"
echo ""
exit

