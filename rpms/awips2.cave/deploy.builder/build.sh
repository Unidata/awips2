#!/bin/bash
# We Have Been Created To Automate The Building Of The AWIPS II RPMs.

# Arguments:
#   ${1} == Build Version
#   ${2} == Build Release

if [ "${RPM_TOP_DIR}" = "" ]; then
   echo "ERROR: You Must Set The RPM_TOP_DIR Environment Variable."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

export TARGET_BUILD_ARCH=
# If the architecture has not been specified, default to 32-bit.
if [ "${CAVE_BUILD_ARCH}" = "" ]; then
   export CAVE_BUILD_ARCH="x86"
   echo "The Build Architecture was not specified ... defaulting to x86."
else
   echo "Building for architecture ... ${CAVE_BUILD_ARCH}."
fi
# Set the target build architecture for the rpms based on the CAVE build architecture.
export TARGET_BUILD_ARCH="${CAVE_BUILD_ARCH}"
if [ "${CAVE_BUILD_ARCH}" = "x86" ]; then
   export TARGET_BUILD_ARCH="i386"
   export CAVE_BUILD_BITS=""
else
   export CAVE_BUILD_BITS="64"
fi

export WORKSPACE_DIR=`cd ../../../; pwd;`

# If there is an actual version, it will be given to us as an argument.
if [ "${1}" = "" ]; then
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
if [ "${2}" = "" ]; then
   # Use the date.
   export BUILD_RELEASE=`date +"%Y%m%d"`
else
   export BUILD_RELEASE="${2}"
fi


# Prepare
if [ ! -f ${WORKSPACE_DIR}/Installer.rpm/awips2.cave/setup/scripts/prepare_dist.sh ]; then
   echo "ERROR: Unable to find the setup script."
   exit 1
fi
/bin/bash ${WORKSPACE_DIR}/Installer.rpm/awips2.cave/setup/scripts/prepare_dist.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: setup failed."
   exit 1
fi

# The RPM Build Directory Structure Consists Of:
#	ROOT = ${RPM_BUILD_ROOT}
#       * BUILD   
#	* RPMS    - Our Output RPMs
#	* SOURCES
#	* SPECS
#	* SRPMS

# Arguments
#	${1} == The Directory With The Specs File And Possibly Other Custom
#               Scripts That May Need To Be Merged Into A Component.
function buildRPM()
{
   local BUILDROOT_DIR=/tmp/awips-component

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
      --define '_topdir %(echo ${RPM_TOP_DIR})' \
      --define '_component_version %(echo ${BUILD_VERSION})' \
      --define '_component_release %(echo ${BUILD_RELEASE})' \
      --define '_build_arch %(echo ${CAVE_BUILD_ARCH})' \
      --define '_build_bits %(echo ${CAVE_BUILD_BITS})' \
      --buildroot ${BUILDROOT_DIR} ${COMPONENT_SPECS}
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
   local CONST_BUILDROOT_DIR=/tmp/awips-component
   local CONST_COMPONENT_SPECS="Installer.cave-feature/component.spec"
   local CONST_SETUP_DIR="Installer.cave-feature/feature.setup"
   local CONST_SETUP_DIR_FULL="${WORKSPACE_DIR}/Installer.rpm/awips2.cave/${CONST_SETUP_DIR}"
   local CONST_FEATURE_DIR="${WORKSPACE_DIR}/build/cave/p2/features"

   local PROCESS_FEATURE_JAR="${WORKSPACE_DIR}/build/tools/ProcessFeature.jar"
   if [ "${AWIPSII_JAVA_HOME}" = "" ]; then
      echo "INFO: AWIPSII_JAVA_HOME has not been set. Defaulting to: /awips2/java."
      export AWIPSII_JAVA_HOME="/awips2/java"
   fi

   if [ -d ${CONST_FEATURE_DIR}/com.raytheon.viz.feature.awips ]; then
      rm -rf ${CONST_FEATURE_DIR}/com.raytheon.viz.feature.awips
   fi
   if [ -d ${CONST_FEATURE_DIR}/com.raytheon.uf.viz.eclipse.feature ]; then
      rm -rf ${CONST_FEATURE_DIR}/com.raytheon.uf.viz.eclipse.feature
   fi
   if [ -d ${CONST_FEATURE_DIR}/com.raytheon.uf.viz.feature.alertviz ]; then
      rm -rf ${CONST_FEATURE_DIR}/com.raytheon.uf.viz.feature.alertviz
   fi
   if [ -d ${CONST_FEATURE_DIR}/com.raytheon.viz.feature.awips.developer ]; then
      rm -rf ${CONST_FEATURE_DIR}/com.raytheon.viz.feature.awips.developer
   fi

   # These are variables that will be placed into the environment by sourcing
   # a shell script.
   for feature_dir in `ls -1 ${CONST_FEATURE_DIR}`;
   do
      java -jar ${PROCESS_FEATURE_JAR} \
         -p \
         ${CONST_FEATURE_DIR}/${feature_dir} \
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

      rpmbuild -ba --target=${TARGET_BUILD_ARCH} \
         --define '_topdir %(echo ${RPM_TOP_DIR})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
         --define '_component_feature %(echo ${COMPONENT_FEATURE})' \
         --define '_component_desc %(echo ${COMPONENT_DESC})' \
         --define '_downstream_requires %(echo ${DOWNSTREAM_REQUIRES})' \
         --define '_component_version %(echo ${BUILD_VERSION})' \
         --define '_component_release %(echo ${BUILD_RELEASE})' \
         --define '_build_arch %(echo ${CAVE_BUILD_ARCH})' \
         --define '_awips2_java_home %(echo ${AWIPSII_JAVA_HOME})' \
         --buildroot ${CONST_BUILDROOT_DIR} ${CONST_COMPONENT_SPECS}
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

unset USE_JROCKIT
unset AWIPSII_JAVA_HOME
