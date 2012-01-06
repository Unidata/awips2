#!/bin/bash
#----------------------------------------------------------------------
# Auto script for a AWIPS II server. 
#----------------------------------------------------------------------
# Default values for these script variables may set using 'defaults'
HOST=`echo $HOSTNAME | cut -d. -f1`
DEV=`expr index "$HOST" awips`

CONFIGURATION_DIR=/awipscm/clusterDeployment
INSTALL_FOLDER=/awips2

# pattern to check in log for "operational" status
OPERATIONAL_STATUS="* EDEX ESB is now operational"


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

HOST=`echo $HOSTNAME | cut -d. -f1`
DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Starting EDEX ESB on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"
echo "----Checking to make sure we can see the installer script----"

echo "----Starting the installer script ${INSTALLER_FOLDER}----"
HDF5_DIR=${INSTALL_FOLDER}/edex/data/hdf5
echo "    Creating target hdf5 directory ${HDF5_DIR}"
mkdir -p ${HDF5_DIR}
echo "    Changing permissions on hdf5 directory ${HDF5_DIR} to 776"
/bin/chmod 776 ${HDF5_DIR}

export http_proxy=
# this should not be necessary; but, for some reason the yum cache on
# awips-dev1 has problems.
yum clean all
yum groupinstall 'AWIPS II Processing Server' -y
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: The YUM Group Installation Of 'AWIPS II Processing Server' Has Failed."
   exit 1
fi

chown -R awips:fxalpha /awips2/edex
chmod -Rf 775 /awips2/edex/data

# start ticket 1575 mods -- mwf
# restore the previously saved hydro data -- provided it exists
SAVE_FILE=/awipscm/blichtenberg/hydro1/hydro-sve.tar
echo "----Restoring Hydro Data----"
if [ -f $SAVE_FILE ];then
	/bin/tar -xf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5/hydroapps
chmod -R 777 $INSTALL_FOLDER/edex/data/hdf5/hydroapps
chown -R awips:awips $INSTALL_FOLDER/edex/data/hdf5/hydroapps
fi
# end ticket 1575 mods -- mwf

SAVE_FILE=/awipscm/mnash/aviation/aviation-sve.tar
echo "----Restoring Aviation Data----"

if [ -f ${SAVE_FILE} ]; then
   /bin/tar -xf $SAVE_FILE -C $INSTALL_FOLDER/edex/data/hdf5 aviation
fi
chmod -R 775 /awips2/edex/data/hdf5/aviation
chown -R awips:awips /awips2/edex/data/hdf5/aviation
echo "----Software Install complete----"

# temporary fix -- may remove later
echo "chmod -R a+w $INSTALL_FOLDER/edex/data/hdf5/hydroapps/precip_proc/local/data/app/mpe"
chmod -R a+w $INSTALL_FOLDER/edex/data/hdf5/hydroapps/precip_proc/local/data/app/mpe
DATE=`date`
echo "**************************************************************************************"
echo "Auto Installer Completed On ${HOST} At ${DATE}"
echo "**************************************************************************************"
echo ""

exit 0


