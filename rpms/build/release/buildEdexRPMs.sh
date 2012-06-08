#!/bin/bash

DB_FILE="${WORKSPACE}/Installer.rpm/awips2-rpm.db"

# Now, it is time to build the AWIPS II Edex RPMs.
echo "INFO: Begin - Building AWIPS II Edex RPMs."

export RPM_TOP_DIR="${WORKSPACE}/rpmbuild"
export WORKSPACE_DIR="${WORKSPACE}"
BUILDROOT_DIR=/tmp/awips2-component

COMPONENT=""
COMPONENT_DIR=""

#echo "DB_FILE=${DB_FILE}"
#exit 1
export AWIPSCM_SHARE=${SHARE_DIR}

function updateEdexRepository()
{
   mv ${RPM_TOP_DIR}/RPMS/i386/* ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/edex
}

function updateSpecsFile()
{
   perl -p -i -e "s/Version: 1.0.0/Version: ${AWIPSII_VERSION}/g" ${COMPONENT_SPECS}
   perl -p -i -e "s/Release: 1/Release: ${AWIPSII_RELEASE}/g" ${COMPONENT_SPECS}
}

function buildRPM()
{
   if [ -f ${RPM_TOP_DIR}/BUILD/component-files.txt ]; then
      rm -f ${RPM_TOP_DIR}/BUILD/component-files.txt
   fi

   rm -rf ${BUILDROOT_DIR}

   updateSpecsFile
   time rpmbuild -ba --target=i386 \
      --define '_topdir %(echo ${RPM_TOP_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --buildroot ${BUILDROOT_DIR} ${COMPONENT_SPECS}
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "ERROR: The build of '${COMPONENT}' has FAILED."
      exit 1
   fi
}

function loopThroughAllEdexRPMs()
{
   SELECT_ALL_SQL="SELECT component FROM awips2_edex_rpms ORDER BY component;"

   # Select All RPMs From The Edex DB
   for component in `echo ${SELECT_ALL_SQL} | sqlite3 ${DB_FILE}`; do
      COMPONENT="${component}"
      # Scan DB for Edex RPMs

      SQL="SELECT buildDirectory FROM awips2_edex_rpms WHERE component = '${COMPONENT}';"

      COMPONENT_DIR=`echo ${SQL} | sqlite3 ${DB_FILE}`
      # We will be building the rpms directly, instead of using the "batch" build scripts that are included in the rpm
      # build projects.
      COMPONENT_SPECS="${WORKSPACE_DIR}/Installer.rpm/${COMPONENT_DIR}/component.spec"
      buildRPM
   done

   # Build edex-npp
#   COMPONENT_SPECS="${WORKSPACE_DIR}/Installer.rpm/awips2.edex/Installer.edex-npp/component.spec"
#   buildRPM
}

function loopThroughSpecifiedRPMs()
{
   # Scan DB for Python Site-Package RPMs
   for component in ${RPMS_TO_BUILD[*]}; do
      COMPONENT="${component}"
      SQL="SELECT buildDirectory FROM awips2_edex_rpms WHERE component = '${COMPONENT}';"

      COMPONENT_DIR=`echo ${SQL} | sqlite3 ${DB_FILE}`
      # We will be building the rpms directly, instead of using the "batch" build scripts that are included in the rpm
      # build projects.
      if [ ! "${COMPONENT_DIR}" = "" ]; then
         COMPONENT_SPECS="${WORKSPACE_DIR}/Installer.rpm/${COMPONENT_DIR}/component.spec"
         buildRPM
      fi
   done
}

if [ "${BUILD_ALL_RPMS}" = "false" ]; then
   loopThroughSpecifiedRPMs
else
   loopThroughAllEdexRPMs
fi

updateEdexRepository

echo "INFO: Finish - Building AWIPS II Edex RPMs."
