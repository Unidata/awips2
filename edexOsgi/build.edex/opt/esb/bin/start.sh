#!/bin/sh
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#-------------------------------------------------------------------------
# This script will start mule in the mode specified on the command line
# Usage:
#   start.sh [options] mode
#
#   mode is one of client, distribute, server, standalone
#   options:
#     -d, --dev   sets DEV_MODE to on (in environment)
#     -b, --debug starts Mule using debug config file.
#
# file history
# Date       PTR    Engineer    Comment
# -------    ---    --------    -----------------
# 09May07    TO6    MW Fegan    Initial creation.
# 06Jun07    279    MW Fegan    Added optional devmode flag.
# 10Apr08    1040   MW Fegan    Added debug command line flag. (removes
#                                the devmode flag.)
# 01Aug08    1363   MW Fegan    Turn off GfePurgeSrv on cluster clients.
# 28Oct2008  1524   MW Fegan    Added export of MULE_HOME
#-------------------------------------------------------------------------

#change to the directory this file runs in
dir=${0%/*}
if [ "$dir" = "$0" ]; then
  dir="."
fi
cd "$dir"

#-------------------------------------------------------------------------
#read and interpret the command line arguments
#-------------------------------------------------------------------------
DEV_FLAG=off
DEBUG_FLAG=off
RUN_MODE=
for arg in $@
do
  case $arg in
    -d|--dev) DEV_FLAG=on;;
    -b|--debug) DEBUG_FLAG=on;;
    -db|-bd) DEV_FLAG=on;  DEBUG_FLAG=on;;
    *) RUN_MODE=$arg;;
  esac
done

#-------------------------------------------------------------------------
#get computer name
#export the name of the mule application (defines name of mule log file)
#-------------------------------------------------------------------------
export NODE_ID=`uname -n`
export MULE_APP=$NODE_ID-${RUN_MODE}



#-------------------------------------------------------------------------
# setup basic internal variables for the script
# r -- points to the esb conf directory
# p -- points to install directory of EDEX data decoder plugins
# g -- contains the relative path to the global xml file
# s -- contains comma separated list of files to skip when
#      building Mule startup configuration string
#-------------------------------------------------------------------------
r=../conf
p=../../../mule/lib/user/plugins
g=$r/global.xml
s=global.xml
files="file:$g"

#-------------------------------------------------------------------------
# set clustered flag and skip files based on the execution type
# note: modify s as needed to exclude end points from running
#       on specific installations
#-------------------------------------------------------------------------
case ${RUN_MODE} in
   client) clustered=true;s=$s,fileToJMS.xml,purge.xml,smartInit.xml,gfePurge.xml;;
   distribute) clustered=true;s=$s,ingest.xml;;
   server) clustered=true;;
   standalone) clustered=false;;
   *) echo "unknown mode ${RUN_MODE}, exiting...";exit 1;;
esac

echo "starting Mule in ${RUN_MODE} mode..."

#-------------------------------------------------------------------------
# set up the runtime environment
#-------------------------------------------------------------------------
export MULE_HOME=$PWD/../../../mule
MULE_CONF=$MULE_HOME/conf
export MULE_LIB=$MULE_HOME/lib

export HIBERNATE_CONF=$PWD/../conf/db
echo "HIBERNATE_CONF set to ${HIBERNATE_CONF}"

#-------------------------------------------------------------------------
#set and export DEV_MODE to the environment
#-------------------------------------------------------------------------
if [ $DEV_FLAG = 'off' ]
then
  export DEV_MODE=off
else
  export DEV_MODE=on
fi
echo "DEV_MODE = ${DEV_MODE}"

#-------------------------------------------------------------------------
#set the wrapper startup config file based on the DEBUG_FLAG
#-------------------------------------------------------------------------
if [ -e $MULE_CONF/wrapper.conf ]
then
  rm $MULE_CONF/wrapper.conf
fi
if [ $DEBUG_FLAG = 'off' ]
then
  cp $MULE_CONF/wrapper.conf.normal $MULE_CONF/wrapper.conf
else
  cp $MULE_CONF/wrapper.conf.debug $MULE_CONF/wrapper.conf
fi
echo Remote Debugging $DEBUG_FLAG

#-------------------------------------------------------------------------
#replace clustered tag
#-------------------------------------------------------------------------
sed 's/<clustered>[a-z][a-z]*<\/clustered>/<clustered>'$clustered'<\/clustered>/g' ../conf/res/site/environment.xml > ../conf/res/site/environment.xml.tmp
mv ../conf/res/site/environment.xml.tmp ../conf/res/site/environment.xml


#-------------------------------------------------------------------------
#run rea to extract end-point descriptors from jars
#-------------------------------------------------------------------------
java -jar rea.jar $MULE_HOME/lib/user/plugins ../conf
java -jar rea.jar $MULE_HOME/lib/user/services ../conf

#-------------------------------------------------------------------------
# identify the "core" config files to include
#-------------------------------------------------------------------------
for x in `ls $r/*.xml`
do
   if [ -f $x ]
   then
      f=`basename $x`
      if [ 0 -eq `expr $s : .*$f` ]
      then
         files=$files,file:$x
      fi
   fi
done

#-------------------------------------------------------------------------
# start Mule
#-------------------------------------------------------------------------
${MULE_HOME}/bin/mule console \
-config ${files} \
-builder org.mule.extras.spring.config.SpringConfigurationBuilder


