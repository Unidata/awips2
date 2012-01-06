#!/bin/bash
#----------------------------------------------------------------------
# auto deploy script for a AWIPS II server. Where possible, machine
# dependent values are obtained at runtime.
#
#----------------------------------------------------------------------
# Default values for these script variables may set using 'defaults'
SOURCE_FOLDER=/awipscm/installers
WS_INSTALLER_SCRIPT=awips-server-setup.sh
IZ_INSTALLER_SCRIPT=server-install-script.xml
INSTALL_FOLDER=/awips
EDEX_INSTANCES=("edexRequest=edex,request" "edexIngest=edex,ingest" "edexIngestGrib=edex,ingestGrib")
SERVER_INSTANCES=("edexRequest=esb.Main,request" "edexIngest=esb.Main,ingest" "edexIngestGrib=esb.Main,ingestGrib")

# pattern to check in log for "operational" status
OPERATIONAL_STATUS="* EDEX ESB is now operational"

#RADAR_SERVER=/RadarServer
RADAR_SERVER=${INSTALL_FOLDER}/rcm
RADAR_SERVER_BAK=/RadarServer-Bak
RCM_DAEMON=/etc/init.d/edex_rcm
PAUSE=60
DB_MACHINE=awips-devdb
RUN_FROM_DIR=/installer
#
#----------------------------------------------------------------------
# basic logic:
#   1) installs new AWIPS II software
#   2) 
#   3) starts AWIPS II services
#
# defaults:
#    default install works on awips-dev1
#
#----------------------------------------------------------------------
# args:
#  $1 :: components to start
#
#----------------------------------------------------------------------
# limitations:
#   1) this script must be run as root
#
#----------------------------------------------------------------------
if [ $# -eq 0 ]; then
   INSTALLED_COMPONENTS="camel"
else
   INSTALLED_COMPONENTS=${1}
   shift 1
   for a in $@; do
      INSTALLED_COMPONENTS="${INSTALLED_COMPONENTS} ${a}"
   done
fi

echo "cd ${RUN_FROM_DIR}"
cd ${RUN_FROM_DIR}

HOST=`echo $HOSTNAME | cut -d. -f1`
DATE=`date`
echo "**************************************************************************************"
echo "Server Auto Installer software install/startup Log For $DATE"
echo "**************************************************************************************"
if [ -f "${RUN_FROM_DIR}/defaults" ]; then
   echo "loading properties from 'defaults'"
  . ${RUN_FROM_DIR}/defaults
else
   echo "using default properties"
fi

echo "--------------------------------------------------------------------------------------"
echo "Starting EDEX ESB on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"
echo "----Checking to make sure we can see the installer script----"

INSTALLER_SCRIPT=${SOURCE_FOLDER}/${WS_INSTALLER_SCRIPT}
echo ${INSTALLER_SCRIPT}
ls -l ${INSTALLER_SCRIPT}
if [ -f "${INSTALLER_SCRIPT}" ]; then
   echo "----Installer script ${WS_INSTALLER_SCRIPT} is available----"
else
	echo "----${WS_INSTALLER_SCRIPT} does not exist.  Stopping the automated installation----"
	echo "`uname -n`: At `date`, Dev Autodeployment script failed-nightly installer file does not exist" > email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Kace_Chrisman@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" mark_w_fegan@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Bryan_J_Rockwood@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Scott_Risch@raytheon.com < email.txt
    exit 1   
fi

echo "----Starting the installer script ${WS_INSTALLER_SCRIPT}----"
HDF5_DIR=${INSTALL_FOLDER}/edex/data/hdf5
echo "    Creating target hdf5 directory ${HDF5_DIR}"
mkdir -p ${HDF5_DIR}
echo "    Changing permissions on hdf5 directory ${HDF5_DIR} to 776"
/bin/chmod 776 ${HDF5_DIR}
echo "    Executing ${INSTALLER_SCRIPT}"
echo "    Using auto-deploy script '${IZ_INSTALLER_SCRIPT}'"
${INSTALLER_SCRIPT} ${RUN_FROM_DIR}/${IZ_INSTALLER_SCRIPT}
echo "----Software Install complete----"

# start ticket 1575 mods -- mwf
# restore the previously saved hydro data -- provided it exists
SAVE_FILE=/tmp/hydro-sve.tar
echo "----Restoring Hydro Data----"
if [ -f $SAVE_FILE ];then
	/bin/tar -xf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5/hydroapps
fi
# end ticket 1575 mods -- mwf

echo "    Running 'cleanup_mpe.sh ${INSTALL_FOLDER}'"
$INSTALL_FOLDER/setup/cleanup_mpe.sh ${INSTALL_FOLDER}

SAVE_FILE=/tmp/aviation-sve.tar
echo "----Restoring Aviation Data----"
if [ -f ${SAVE_FILE} ]; then
   /bin/tar -xf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5 aviation
fi

echo "----Software Install complete----"

echo "----Starting Qpid daemon----"
echo "/etc/init.d/edex_qpid start"
/etc/init.d/edex_qpid start
sleep ${PAUSE}
PID=`/sbin/pidof qpidd`
if [ -z "${PID}" ]; then
   echo "    WARNING: Qpid daemon not running"
   let ERROR_COUNT=ERROR_COUNT+1
else
   echo "    Qpid daemon running"
fi

echo "----Starting the EDEX Server services----"
echo "Number of Edex Server services: ${#SERVER_INSTANCES[@]}"
ERROR_COUNT=0
for COMPONENT in ${INSTALLED_COMPONENTS};do
   echo "----Starting ${COMPONENT}----"
   DAEMON="/etc/init.d/edex_${COMPONENT}"
   if [ -f ${DAEMON} ];then
      echo "    executing '${DAEMON} start'"
      ${DAEMON} start
   else
      echo "    WARNING: Unable to find ${DAEMON} - will attempt to continue"
      let ERROR_COUNT=ERROR_COUNT+1
   fi
done
sleep ${PAUSE}

echo "----Verifying Service Startup startup----"
for a in ${SERVER_INSTANCES[@]};do
   NAME=`echo ${a} | cut -d= -f1`
   SERVICE=`echo ${a} | cut -d= -f2 | cut -d, -f1`
   INSTANCE=`echo ${a} | cut -d= -f2 | cut -d, -f2`
   echo "    Checking startup for '${NAME}(${SERVICE}-${INSTANCE})'"
   COUNT=`ps aux | grep ${SERVICE} | grep ${INSTANCE} | wc -l`
   if [ ${COUNT} -eq 0 ];then
      echo "    WARNING: ${NAME}(${SERVICE}-${INSTANCE}) not running"
      let ERROR_COUNT=ERROR_COUNT+1
   else
      echo "    ${NAME}(${SERVICE}-${INSTANCE}) running"
   fi
done
echo "----Pausing ${PAUSE} seconds to allow EDEX ESB instances to start----"
echo "----Waiting for the EDEX ESB to become operational----"
COUNT=0
for a in ${EDEX_INSTANCES[@]}; do
   NAME=`echo ${a} | cut -d= -f1`
   HOME=`echo ${a} | cut -d= -f2 | cut -d, -f1`
   INSTANCE=`echo ${a} | cut -d= -f2 | cut -d, -f2`
   echo "----Checking EDEX ESB instance ${NAME}"
   STARTED=0
   TIMEOUT_COUNT=0
   while [ ${STARTED} -eq 0 -a ${TIMEOUT_COUNT} -lt 200 ]
   do
	  let TIMEOUT_COUNT=TIMEOUT_COUNT+1
	  LOG_FILE_PATTERN="${INSTALL_FOLDER}/${HOME}/logs/edex-${INSTANCE}-*.log"
	  if [ -e ${LOG_FILE_PATTERN} ];then
	     STARTED=`grep -c "${OPERATIONAL_STATUS}" ${LOG_FILE_PATTERN}`
	     if [ ${STARTED} -ne 0 ]; then
	        echo "   EDEX ESB instance ${NAME} is operational"
	        let COUNT=COUNT+1
	     else
	        echo "   EDEX ESB instance ${NAME} not yet operational; pausing. ${TIMEOUT_COUNT}"
	     fi
	  else
	     STARTED=0
	     echo "    EDEX ESB instance ${NAME} log not available; pausing. ${TIMEOUT_COUNT}"
	  fi
	  sleep 5
   done
   echo "----Check of EDEX ESB instance '${NAME}' complete" 
done

if [ ${COUNT} -ne ${#SERVER_INSTANCES[@]} ];then
	echo "----Timed out waiting for EDEX endpoints...exiting----"
	echo "`uname -n`: At `date`, Dev Autodeployment script failed to start Edex" > email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Kace_Chrisman@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" mark_w_fegan@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Bryan_J_Rockwood@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Scott_Risch@raytheon.com < email.txt
	exit 99
fi

echo "----EDEX endpoints are now available.----"

echo "Waiting 60 seconds just in case not all of the sbn dirs have been created"
sleep 60
echo "chmod -R a+rwx $INSTALL_FOLDER/edex/data/sbn"
chmod -R a+rwx $INSTALL_FOLDER/edex/data/sbn

# temporary fix -- may remove later
echo "chmod -R a+w $INSTALL_FOLDER/edex/data/hdf5/hydroapps/precip_proc/local/data/app/mpe"
chmod -R a+w $INSTALL_FOLDER/edex/data/hdf5/hydroapps/precip_proc/local/data/app/mpe

echo "----Starting LDM----"
echo "su - ldm -c \"ldmadmin start\""
su - ldm -c "ldmadmin start"
if [ $? -eq 0 ];then
   echo "    LDM started"
else
   echo "    WARNING: Unable to start LDM, continuing"
   let ERROR_COUNT=ERROR_COUNT+1
fi

DATE=`date`
echo "**************************************************************************************"
echo "Dev Standalone Auto Installer Completed On ${HOST} At ${DATE}"
if [ ${ERROR_COUNT} -ne 0 ]; then
   echo "encountered ${ERROR_COUNT} issues in install - see log for details"
fi
echo "**************************************************************************************"
echo ""

exit 0

