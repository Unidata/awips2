#!/bin/bash
# This script will build the AWIPS II Edex RPMs.

# Build Variables:
# -----------------------------------------------------------------------------
VAR_AWIPSII_BUILD_ROOT="/tmp/awips-component"
VAR_AWIPSII_VERSION=""
VAR_AWIPSII_RELEASE=""
VAR_UFRAME_ECLIPSE="/awips2/eclipse"
# -----------------------------------------------------------------------------

if [ "${AWIPSII_TOP_DIR}" = "" ]; then
   echo "ERROR: You Must Set the AWIPSII_TOP_DIR Environment Variable."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

if [ "${WORKSPACE}" = "" ]; then
   echo "ERROR: You Must Set the WORKSPACE Environment Variable."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

function prepareBuildEnvironment()
{
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
}

# Build EDEX as 64 bit only.
architecture=`uname -i`
if [ ! "${architecture}" = "x86_64" ]; then
   echo "ERROR: This build can only be performed on a 64-bit Operating System."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

function patchSpecification()
{
   # copy the standard rpm feature specification into the
   # component's project directory
   cp -v Installer.edex-component/component.spec \
      Installer.${COMPONENT_NAME}/component.spec
   if [ $? -ne 0 ]; then
      exit 1
   fi   

   # apply the specification patch
   pushd . > /dev/null 2>&1
   cd Installer.${COMPONENT_NAME}
   patch -p1 -i *.patch0
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
      /usr/bin/rpmbuild -bb \
         --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
         --define '_baseline_workspace %(echo ${WORKSPACE})' \
         --define '_uframe_eclipse %(echo ${UFRAME_ECLIPSE})' \
         --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
         --define '_build_site %(echo ${AWIPSII_BUILD_SITE})' \
         --define '_component_version %(echo ${AWIPSII_VERSION})' \
         --define '_component_release %(echo ${AWIPSII_RELEASE})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
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
   /awips2/ant/bin/ant -f build.xml -Dlightning=true \
      -Duframe.eclipse=${UFRAME_ECLIPSE}
   RC=$?
else
   /awips2/ant/bin/ant -f build.xml \
      -Duframe.eclipse=${UFRAME_ECLIPSE}
   RC=$?
fi
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd

# Adjust Our Execution Position.
cd ../

buildRPM "Installer.edex"
buildRPM "Installer.edex-configuration"

# build the edex-hazards component
export COMPONENT_NAME="edex-hazards"
# Workaround until #5466 is resolved.
if [ -f ${DIST}/${COMPONENT_NAME}.zip ]; then
   patchSpecification
   buildRPM "Installer.edex-hazards"
fi
unset COMPONENT_NAME

DIST="${WORKSPACE}/build.edex/edex/dist"
for edex_zip in `cd ${DIST}; ls -1;`;
do
   edex_component=`python -c "zipFile='${edex_zip}'; componentName=zipFile.replace('.zip',''); print componentName;"`
  
   #Data Delivery and Hazard Services components are built separately
   if [ ! "${edex_component}" = "edex-datadelivery" ] &&
      [ ! "${edex_component}" = "common-base" ] &&
      [ ! "${edex_component}" = "edex-hazards" ]; then
      export COMPONENT_NAME="${edex_component}"
      buildRPM "Installer.edex-component"
      unset COMPONENT_NAME
   fi
done

# build the edex-datadelivery rpm
export COMPONENT_NAME="edex-datadelivery"
# Workaround until #5466 is resolved.
if [ -f ${DIST}/${COMPONENT_NAME}.zip ]; then
   patchSpecification
   buildRPM "Installer.edex-datadelivery"
fi
unset COMPONENT_NAME

