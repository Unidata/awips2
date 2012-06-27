#!/bin/bash

# Arguments:
#   ${1} == Build Version
#   ${2} == Build Release
#   -nobinlightning optional flag 

if [ "${RPM_TOP_DIR}" = "" ]; then
   echo "ERROR: You Must Set The RPM_TOP_DIR Environment Variable."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# !! NOTE !! - We Assume That We Are In A Workspace With The Installer Projects,
#              The Edex Projects, The Cave Projects, The Native Projects, And Etc.
export WORKSPACE_DIR=`cd ../../../; pwd;`

source ${WORKSPACE_DIR}/Installer.rpm/awips2.core/deploy.builder/env.sh

# If there is an actual version, it will be given to us as an argument.
if [ "${1}" = "" ] ||
   [ "${1}" = "-nobinlightning" ]; then
   # Check version.txt
   VERSION_TXT="${WORKSPACE_DIR}/Installer.rpm/version.txt"
   if [ ! -f ${VERSION_TXT} ]; then
      echo "ERROR: Unable to find the version file - ${VERSION_TXT}."
      exit 1
   fi
   export BUILD_VERSION=`cat ${VERSION_TXT}`
   # Ensure that a version has been specified.
   if [ "${BUILD_VERSION}" = "" ]; then
      echo "ERROR: A build version has not been specified."
      exit 1
   fi
else
   export BUILD_VERSION="${1}"
fi
# If there is an actual release, it will be given to us as an argument.
if [ "${2}" = "" ] ||
   [ "${2}" = "-nobinlightning" ]; then
   # Use the date.
   export BUILD_RELEASE=`date +"%Y%m%d"`
else
   export BUILD_RELEASE="${2}"
fi

#See if there is a -nobinlightning flag
LIGHTNING="YES"
for x in "$@"
do
  if [ ${x} = "-nobinlightning" ]; then
    LIGHTNING="NO"
  fi
done

function buildRPM()
{
   BUILDROOT_DIR=/tmp/awips-component

   COMPONENT_DIR=${1}
   COMPONENT_SPECS=${COMPONENT_DIR}/component.spec

   # We Need To Delete The 'BuildRoot' Directory After Each RPM Is
   # Built Whether The Build Is Successful Or Not.
   rm -rf ${BUILDROOT_DIR}

   # We Build The List Of Files That Need To Be Installed On-Demand Now.
   # If One Exists From A Previous Build, Delete It.
   if [ -f ${RPM_TOP_DIR}/BUILD/component-files.txt ]; then
      rm -f ${RPM_TOP_DIR}/BUILD/component-files.txt
   fi

   # Build The RPM.
   rpmbuild -ba --target=i386 \
      --define '_topdir %(echo ${RPM_TOP_DIR})' \
      --define '_component_version %(echo ${BUILD_VERSION})' \
      --define '_component_release %(echo ${BUILD_RELEASE})' \
      --buildroot ${BUILDROOT_DIR} ${COMPONENT_SPECS}
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo ""
      echo "ERROR: Unable To Build The RPM Defined In: ${COMPONENT_DIR}."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi
}

# Get A List Of The RPM Directories (Excluding This One)
# Note: Presently, We Are In ../../Installer.rpm/deploy.builder

# Adjust Our Execution Position.
cd ../

buildRPM "Installer.edex-base"
buildRPM "Installer.edex-configuration"
buildRPM "Installer.edex-gfe"
buildRPM "Installer.edex-bufr"
buildRPM "Installer.edex-common-core"
buildRPM "Installer.edex-core"
buildRPM "Installer.edex-cots"
buildRPM "Installer.edex-dat"
buildRPM "Installer.edex-dataplugins"
if [ $LIGHTNING = "YES" ]; then
  buildRPM "Installer.edex-binlightning"
fi
buildRPM "Installer.edex-grib"
buildRPM "Installer.edex-hydro"
buildRPM "Installer.edex-radar"
buildRPM "Installer.edex-ncep"
buildRPM "Installer.edex-satellite"
buildRPM "Installer.edex-text"
buildRPM "Installer.edex-native"
buildRPM "Installer.edex-shapefiles"
#buildRPM "Installer.edex-npp"
