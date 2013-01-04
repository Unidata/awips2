#!/bin/bash
# This script will build the AWIPS II Edex RPMs.

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
   # Set the target build architecture for the rpms based on the EDEX build
   # architecture.
   export TARGET_BUILD_ARCH="${EDEX_BUILD_ARCH}"
   export EDEX_BUILD_BITS="64"
   if [ "${EDEX_BUILD_ARCH}" = "x86" ]; then
      export TARGET_BUILD_ARCH="i386"
      export EDEX_BUILD_BITS="32"
   fi
}


export TARGET_BUILD_ARCH=
# If the architecture has not been specified, default to 32-bit.
if [ "${EDEX_BUILD_ARCH}" = "" ]; then
   export EDEX_BUILD_ARCH="x86"
   echo "The Build Architecture was not specified ... defaulting to x86."
else
   echo "Building for architecture ... ${EDEX_BUILD_ARCH}."
fi

function patchDDSpecification()
{
   # copy the standard rpm feature specification into the
   # data delivery rpm project directory
   cp -v Installer.edex-component/component.spec \
      Installer.edex-datadelivery/component.spec
   if [ $? -ne 0 ]; then
      exit 1
   fi   

   # apply the specification patch
   pushd . > /dev/null 2>&1
   cd Installer.edex-datadelivery
   patch -p1 -i datadelivery.patch0
   if [ $? -ne 0 ]; then
      exit 1
   fi
   popd > /dev/null 2>&1
}

function buildRPM()
{
   # Arguments:
   #   ${1} == specs file

   if [ ! "${COMPONENT_NAME}" = "edex-binlightning" ] ||
      [ ${LIGHTNING} = true ]; then
      rpmbuild -ba --target=${TARGET_BUILD_ARCH} \
         --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
         --define '_baseline_workspace %(echo ${WORKSPACE})' \
         --define '_uframe_eclipse %(echo ${UFRAME_ECLIPSE})' \
         --define '_awipscm_share %(echo ${AWIPSCM_SHARE})' \
         --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
         --define '_component_version %(echo ${AWIPSII_VERSION})' \
         --define '_component_release %(echo ${AWIPSII_RELEASE})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
         --define '_build_arch %(echo ${EDEX_BUILD_ARCH})' \
         --define '_build_bits %(echo ${EDEX_BUILD_BITS})' \
         --buildroot ${AWIPSII_BUILD_ROOT} \
         ${1}/component.spec
      RC=$?
      if [ ${RC} -ne 0 ]; then
         echo "FATAL: rpmbuild failed."
         exit 1
      fi
   fi
}

prepareBuildEnvironment

pushd .
cd ${WORKSPACE}/build.edex
if [ $? -ne 0 ]; then
   exit 1
fi
if [ ${LIGHTNING} = true ]; then
   /awips2/ant/bin/ant -f build.xml -Dlightning=true
   RC=$?
else
   /awips2/ant/bin/ant -f build.xml
   RC=$?
fi
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd
setTargetArchitecture

# Adjust Our Execution Position.
cd ../

buildRPM "Installer.edex-base"
buildRPM "Installer.edex-configuration"
buildRPM "Installer.edex-shapefiles"
# only build edex-datadelivery if it is present
# (this logic will be removed once edex-datadelivery has been integrated)
if [ -f ${WORKSPACE}/build.edex/edex/dist/edex-datadelivery.zip ]; then
   # build the edex-datadelivery rpm
   export COMPONENT_NAME="edex-datadelivery"
   patchDDSpecification
   buildRPM "Installer.edex-datadelivery"
   unset COMPONENT_NAME
fi
# For, now edex-native is always a 32-bit rpm.
export TARGET_BUILD_ARCH="i386"
buildRPM "Installer.edex-native"
# Reset the target architecture for the remaining rpms.
setTargetArchitecture

DIST="${WORKSPACE}/build.edex/edex/dist"
for edex_zip in `cd ${DIST}; ls -1;`;
do
   edex_component=`python -c "zipFile='${edex_zip}'; componentName=zipFile.replace('.zip',''); print componentName;"`
   # do not build edex-datadelivery since it is now built differently from the other edex feature rpms
   # since this is currently the only case, the exclusion will be hard-coded
   
   if [ ! "${edex_component}" = "edex-datadelivery" ]; then
      export COMPONENT_NAME="${edex_component}"
      buildRPM "Installer.edex-component"
   fi
done
