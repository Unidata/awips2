#!/bin/bash -vf

function buildRPM()
{
   # Arguments:
   #   ${1} == the name of the rpm.
   lookupRPM "${1}"
   if [ $? -ne 0 ]; then
      echo "ERROR: '${1}' is not a recognized AWIPS II RPM."
      exit 1
   fi

        buildRPMExec "${RPM_SPECIFICATION}"

   return 0
}

# Arguments
#   ${1} == The Directory With The Specs File And Possibly Other Custom
#               Scripts That May Need To Be Merged Into A Component.
#   ${2} == The name of the CAVE RPM that will be built.
function buildRPMExec()
{
   local COMPONENT_DIR=${1}
   local COMPONENT_SPECS=${COMPONENT_DIR}/component.spec
   export RPM_NAME=${2}

   /usr/bin/rpmbuild -bb \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_uframe_eclipse %(echo ${UFRAME_ECLIPSE})' \
      --define '_static_files %(echo ${AWIPSII_STATIC_FILES})' \
      --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
      --define '_build_site %(echo ${AWIPSII_BUILD_SITE})' \
      --define '_component_name %(echo ${RPM_NAME})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_component_build_date %(echo ${COMPONENT_BUILD_DATE})' \
      --define '_component_build_time %(echo ${COMPONENT_BUILD_TIME})' \
      --define '_component_build_system %(echo ${COMPONENT_BUILD_SYSTEM})' \
      --buildroot ${AWIPSII_BUILD_ROOT} \
      ${COMPONENT_SPECS}
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build RPM ${1}."
      exit 1
   fi
}

function buildEDEX()
{
   cd ${WORKSPACE}/rpms/awips2.edex/deploy.builder
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the edex rpms."
      exit 1
   fi

   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the edex rpms."
      exit 1
   fi

   return 0
}

function buildCAVE()
{
   cd ${WORKSPACE}/rpms/awips2.cave/deploy.builder
   echo "in ${WORKSPACE}/rpms/awips2.cave/deploy.builder"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the cave rpms."
      exit 1
   fi

   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the cave rpms."
      exit 1
   fi

   return 0
}
function buildLocalizationRPMs()
{
   awips2_core_directory=${WORKSPACE}/rpms/awips2.core
   installer_localization_directory="${awips2_core_directory}/Installer.localization"
   localization_SPECIFICATION="${installer_localization_directory}/component.spec"

   export LOCALIZATION_DIRECTORY="localization"
   export COMPONENT_NAME="awips2-localization"

   echo "Building localization rpm"

   rpmbuild -ba \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_component_name %(echo ${COMPONENT_NAME})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_localization_directory %(echo ${LOCALIZATION_DIRECTORY})' \
      --define '_build_site %(echo ${AWIPSII_BUILD_SITE})' \
      --buildroot ${AWIPSII_BUILD_ROOT} \
      ${localization_SPECIFICATION}
   RC=$?
   unset LOCALIZATION_DIRECTORY
   unset COMPONENT_NAME
   if [ ${RC} -ne 0 ]; then
      return 1
   fi

   return 0
}

function buildShapefiles()
{
   cd ${WORKSPACE}/rpms/awips2.shapefiles/deploy.builder
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the edex shapefile rpm."
      return 1
   fi

   # Determine the build architecture.
   export EDEX_BUILD_ARCH=`uname -i`
   if [ "${EDEX_BUILD_ARCH}" = "i386" ]; then
      export EDEX_BUILD_ARCH="x86"
   fi

   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to determine the architecture."
      return 1
   fi
   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the edex shapefile rpm."
      return 1
   fi

   return 0
}

