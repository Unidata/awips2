#!/bin/bash
# This script will build the AWIPS II Viz RPMs.

# Build Variables:
# -----------------------------------------------------------------------------
VAR_AWIPSII_TOP_DIR="/home/bkowal/rpmbuild"
VAR_WORKSPACE="/common/bkowal/git/thunder/baseline"
VAR_AWIPSII_BUILD_ROOT="/tmp/awips-component"
VAR_AWIPSII_VERSION=""
VAR_AWIPSII_RELEASE=""
VAR_UFRAME_ECLIPSE="/opt/uframe-eclipse"
VAR_AWIPSCM_SHARE="/awipscm"
# -----------------------------------------------------------------------------

if [ "${AWIPSII_TOP_DIR}" = "" ] &&
   [ "${VAR_AWIPSII_TOP_DIR}" = "" ]; then
   echo "ERROR: You Must Set the AWIPSII_TOP_DIR Environment Variable."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

function prepareBuildEnvironment()
{
   if [ "${AWIPSII_TOP_DIR}" = "" ]; then
      export AWIPSII_TOP_DIR="${VAR_AWIPSII_TOP_DIR}"
   fi

   if [ "${WORKSPACE}" = "" ]; then
      export WORKSPACE="${VAR_WORKSPACE}"
   fi

   if [ "${AWIPSII_BUILD_ROOT}" = "" ]; then
      export AWIPSII_BUILD_ROOT="${VAR_AWIPSII_BUILD_ROOT}"
   fi

   if [ "${AWIPSII_VERSION}" = "" ]; then
      # Determine if we need to use the default version.
      if [ "${VAR_AWIPSII_VERSION}" = "" ]; then
         VAR_AWIPSII_VERSION=`cat ${WORKSPACE}/rpms/version.txt`
      fi
      export AWIPSII_VERSION="${VAR_AWIPSII_VERSION}"
   fi

   if [ "${AWIPSII_RELEASE}" = "" ]; then
      # Determine if we need to use the default release.
      if [ "${VAR_AWIPSII_RELEASE}" = "" ]; then
         VAR_AWIPSII_RELEASE=`date +"%Y%m%d"`
      fi
      export AWIPSII_RELEASE="${VAR_AWIPSII_RELEASE}"
   fi

   if [ "${UFRAME_ECLIPSE}" = "" ]; then
      export UFRAME_ECLIPSE="${VAR_UFRAME_ECLIPSE}"
   fi

   if [ "${AWIPSCM_SHARE}" = "" ]; then
      export AWIPSCM_SHARE="${VAR_AWIPSCM_SHARE}"
   fi
}

function setTargetArchitecture()
{
   # Set the target build architecture for the rpms based on the CAVE build
   # architecture.
   export TARGET_BUILD_ARCH="${CAVE_BUILD_ARCH}"
   export CAVE_BUILD_BITS="64"
   if [ "${CAVE_BUILD_ARCH}" = "x86" ]; then
      export TARGET_BUILD_ARCH="i386"
      export CAVE_BUILD_BITS=""
   fi
}

export TARGET_BUILD_ARCH=
# If the architecture has not been specified, default to 32-bit.
if [ "${CAVE_BUILD_ARCH}" = "" ]; then
   export CAVE_BUILD_ARCH="x86"
   echo "The Build Architecture was not specified ... defaulting to x86."
else
   echo "Building for architecture ... ${CAVE_BUILD_ARCH}."
fi

# Prepare
prepareBuildEnvironment
setTargetArchitecture

if [ ! -d ${WORKSPACE}/rpms/awips2.cave/setup/dist ]; then
   mkdir -p ${WORKSPACE}/rpms/awips2.cave/setup/dist
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
if [ ! -d ${WORKSPACE}/rpms/awips2.cave/Installer.cave-feature/feature.setup ]; then
   mkdir -p ${WORKSPACE}/rpms/awips2.cave/Installer.cave-feature/feature.setup
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

if [ ! -f ${WORKSPACE}/rpms/awips2.cave/setup/scripts/prepare_dist.sh ]; then
   echo "ERROR: Unable to find the setup script."
   exit 1
fi
/bin/bash ${WORKSPACE}/rpms/awips2.cave/setup/scripts/prepare_dist.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: setup failed."
   exit 1
fi

# Arguments
#	${1} == The Directory With The Specs File And Possibly Other Custom
#               Scripts That May Need To Be Merged Into A Component.
function buildRPM()
{
   local COMPONENT_DIR=${1}
   # if we are building a 64-bit version of CAVE, create a temporary
   # component.spec.64 file for the build.
   if [ ! "${TARGET_BUILD_ARCH}" = "x86_64" ]; then
      local COMPONENT_SPECS=${COMPONENT_DIR}/component.spec
   else
      # Create an alternate component.spec file.
      cp -v ${COMPONENT_DIR}/component.spec ${COMPONENT_DIR}/component.spec.64
      if [ $? -ne 0 ]; then
         exit 1
      fi
      # libraries to substitute: { libMrm.so.4, libXp.so.6, libg2c.so.0 }
      perl -p -i -e "s/requires: libMrm.so.4/requires: libMrm.so.4()(64bit)/g" \
         ${COMPONENT_DIR}/component.spec.64
      if [ $? -ne 0 ]; then
         exit 1
      fi
      perl -p -i -e "s/requires: libXp.so.6/requires: libXp.so.6()(64bit)/g" \
         ${COMPONENT_DIR}/component.spec.64
      if [ $? -ne 0 ]; then
         exit 1
      fi
      perl -p -i -e "s/requires: libg2c.so.0/requires: libg2c.so.0()(64bit)/g" \
         ${COMPONENT_DIR}/component.spec.64
      if [ $? -ne 0 ]; then
         exit 1
      fi
      local COMPONENT_SPECS=${COMPONENT_DIR}/component.spec.64
   fi

   if [ -d ${BUILDROOT_DIR} ]; then
      rm -rf ${BUILDROOT_DIR}
   fi

   # Build The RPM.
   rpmbuild -ba --target=${TARGET_BUILD_ARCH} \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_build_arch %(echo ${CAVE_BUILD_ARCH})' \
      --define '_build_bits %(echo ${CAVE_BUILD_BITS})' \
      --buildroot ${AWIPSII_BUILD_ROOT} ${COMPONENT_SPECS}
   # If We Are Unable To Build An RPM, Fail The Build:
   RC="$?"

   if [ -f ${COMPONENT_DIR}/component.spec.64 ]; then
      rm -fv ${COMPONENT_DIR}/component.spec.64
   fi
   if [ ! "${RC}" = "0" ]; then
      exit 1
   fi
}

function buildFeatureRPMs()
{
   local CONST_COMPONENT_SPECS="Installer.cave-feature/component.spec"
   local CONST_SETUP_DIR="Installer.cave-feature/feature.setup"
   local CONST_SETUP_DIR_FULL="${WORKSPACE}/rpms/awips2.cave/${CONST_SETUP_DIR}"
   local CONST_FEATURE_DIR="${WORKSPACE}/build/cave/p2/features"
   local CONST_FEATURES_TXT="${WORKSPACE}/build/cave/p2/dist/features.txt"
   
   if [ ! -f ${CONST_FEATURES_TXT} ]; then
      echo "ERROR: Unable to find the list of features - ${CONST_FEATURES_TXT}."
      exit 1
   fi

   local PROCESS_FEATURE_JAR="${WORKSPACE}/build/tools/ProcessFeature.jar"

   for feature in `cat ${CONST_FEATURES_TXT}`;
   do
      java -jar ${PROCESS_FEATURE_JAR} \
         -p \
         ${CONST_FEATURE_DIR}/${feature} \
         ${CONST_SETUP_DIR_FULL}
      RC=$?
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: ${PROCESS_FEATURE_JAR} Failed."
         exit 1
      fi

      if [ ! -f ${CONST_SETUP_DIR}/feature.setup ]; then
         echo "ERROR: ${CONST_SETUP_DIR}/feature.setup Does Not Exist."
         exit 1
      fi
      source ${CONST_SETUP_DIR}/feature.setup

      echo "Building Feature ... ${feature}"
      rpmbuild -ba --target=${TARGET_BUILD_ARCH} \
         --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
         --define '_component_feature %(echo ${COMPONENT_FEATURE})' \
         --define '_component_desc %(echo ${COMPONENT_DESC})' \
         --define '_downstream_requires %(echo ${DOWNSTREAM_REQUIRES})' \
         --define '_component_version %(echo ${AWIPSII_VERSION})' \
         --define '_component_release %(echo ${AWIPSII_RELEASE})' \
         --define '_baseline_workspace %(echo ${WORKSPACE})' \
         --define '_build_arch %(echo ${CAVE_BUILD_ARCH})' \
         --buildroot ${AWIPSII_BUILD_ROOT} ${CONST_COMPONENT_SPECS}
      RC=$?
      if [ ${RC} -ne 0 ]; then
         exit 1
      fi     
   done
}

# Adjust Our Execution Position.
cd ../

# Only Build The RPMs That May Have Changed - AWIPS II-Specific Components.
buildRPM "Installer.cave"
buildRPM "Installer.cave-etc"
buildFeatureRPMs
