#!/bin/bash

function buildRPM()
{
   # Arguments:
   #   ${1} == the name of the rpm.
   lookupRPM "${1}"
   if [ $? -ne 0 ]; then
      echo "ERROR: '${1}' is not a recognized AWIPS II RPM."
      exit 1
   fi

   /usr/bin/rpmbuild -bb \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_uframe_eclipse %(echo ${UFRAME_ECLIPSE})' \
      --define '_static_files %(echo ${AWIPSII_STATIC_FILES})' \
      --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
      --define '_build_site %(echo ${AWIPSII_BUILD_SITE})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_component_build_date %(echo ${COMPONENT_BUILD_DATE})' \
      --define '_component_build_time %(echo ${COMPONENT_BUILD_TIME})' \
      --define '_component_build_system %(echo ${COMPONENT_BUILD_SYSTEM})' \
      --buildroot ${AWIPSII_BUILD_ROOT} \
      ${RPM_SPECIFICATION}/component.spec
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build RPM ${1}."
      exit 1
   fi

   return 0
}

# This script will build all of the 32-bit rpms.
# Ensure that we are on a machine with the correct architecture.

architecture=`uname -i`
if [ ! "${architecture}" = "i386" ]; then
   echo "ERROR: This build can only be performed on a 32-bit Operating System."
   exit 1
fi

# Determine which directory we are running from.
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

common_dir=`cd ${dir}/../common; pwd;`
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to find the common functions directory."
   exit 1
fi
# source the common functions.
source ${common_dir}/lookupRPM.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to source the common functions."
   exit 1
fi
source ${common_dir}/systemInfo.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to retrieve the system information."
   exit 1
fi

# prepare the build environment.
source ${dir}/buildEnvironment.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to prepare the build environment."
   exit 1
fi

if [ "${1}" = "-full" ]; then
   buildRPM "awips2-qpid-lib"
   buildRPM "awips2-qpid-java"
   buildRPM "awips2-qpid-java-broker"
   buildRPM "awips2-java"
   buildRPM "awips2-notification"

   exit 0
fi

if [ "${1}" = "-qpid" ]; then
   buildRPM "awips2-qpid-lib"
   buildRPM "awips2-qpid-java"
   buildRPM "awips2-qpid-java-broker"

   exit 0
fi

if [ "${1}" = "-java" ]; then
   buildRPM "awips2-java"

   exit 0
fi

if [ "${1}" = "-notification" ]; then
   buildRPM "awips2-notification"

   exit 0
fi

echo "Usage: $0 OPTION"
echo "   -full          perform a full build of all the rpms."
echo "   -qpid          build only the QPID rpms."
echo "   -java          build only the Java rpm."
echo "   -notification  build only the notification rpm."
echo "   --help         display this message and exit."

exit 0
