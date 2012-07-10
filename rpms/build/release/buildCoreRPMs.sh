#!/bin/bash

set -x

DB_FILE="${WORKSPACE}/Installer.rpm/awips2-rpm.db"

# Now, it is time to build the AWIPS II Core rpms.
echo "INFO: Begin - Building AWIPS II Core RPMs."
export RPM_TOP_DIR="${WORKSPACE}/rpmbuild"
export WORKSPACE_DIR="${WORKSPACE}"
export BUILDROOT_DIR=/tmp/awips2-component

COMPONENT=""
COMPONENT_DIR=""

export AWIPSCM_SHARE=${SHARE_DIR}
export CFLAGS="-m32 -O2"
export LDFLAGS="-m32"
export BASECFLAGS="-m32 -O2"
export LD="ld -melf_i386"

# Build the qpid-cluster rpms.

function updateCoreRepository()
{
   mv ${RPM_TOP_DIR}/RPMS/i386/* ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/core
   mv ${RPM_TOP_DIR}/RPMS/noarch/* ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/core
}

function updateSpecsFileIfNecessary()
{
   SQL="SELECT dynamicVersion FROM awips2_core_rpms WHERE component = '${COMPONENT}';"

   UPDATE_FLAG=`echo ${SQL} | sqlite3 ${DB_FILE}`
   if [ "${UPDATE_FLAG}" = "Y" ]; then
      perl -p -i -e "s/Version: 1.0.0/Version: ${AWIPSII_VERSION}/g" ${COMPONENT_SPECS}
      perl -p -i -e "s/Release: 1/Release: ${AWIPSII_RELEASE}/g" ${COMPONENT_SPECS}
   fi
}

function buildRPM()
{
   if [ -f ${RPM_TOP_DIR}/BUILD/component-files.txt ]; then
      rm -f ${RPM_TOP_DIR}/BUILD/component-files.txt
   fi

   rm -rf ${BUILDROOT_DIR}

   updateSpecsFileIfNecessary
   time rpmbuild -ba --target=i386 \
      --define '_topdir %(echo ${RPM_TOP_DIR})' \
      --define '_build_root %(echo ${BUILDROOT_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --buildroot ${BUILDROOT_DIR} ${COMPONENT_SPECS}
   RC="$?"

   if [ ! "${RC}" = "0" ]; then
      echo "ERROR: The build of '${COMPONENT}' has FAILED."
      exit 1
   fi
}

function buildLocalizationRPMs()
{  
   # Find all of the localization.${site} directories, if there are any.
   ls ${WORKSPACE}/localization.* > /dev/null 2>&1
   RC=$?
   if [ ${RC} -ne 0 ]; then
      # There are not any localization projects.
      echo "INFO: There are not any localization projects."
      return 0
   fi

   for dir in `cd ${WORKSPACE}; ls -1d localization.*`; do
      site=`perl ${WORKSPACE}/Installer.rpm/awips2.core/deploy.builder/extractSite.pl ${dir}`
      RC=$?
      if [ ${RC} -ne 0 ]; then
         exit 1
      fi
      export LOCALIZATION_DIRECTORY="${dir}"
      export COMPONENT_NAME="awips2-localization-${site}"
      echo "site: ${site}"
      export site="${site}"

      rm -rf ${BUILDROOT_DIR}

echo "AAAAAA the version in localization is ${AWIPSII_VERSION}"
echo "AAAAAA the release in localization is ${AWIPSII_RELEASE}"

      rpmbuild -ba \
         --define '_topdir %(echo ${RPM_TOP_DIR})' \
         --define '_component_version %(echo ${AWIPSII_VERSION})' \
         --define '_component_release %(echo ${AWIPSII_RELEASE})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
         --define '_baseline_workspace %(echo ${WORKSPACE})' \
         --define '_localization_site %(echo ${site})' \
         --define '_localization_directory %(echo ${LOCALIZATION_DIRECTORY})' \
         --buildroot ${BUILDROOT_DIR} \
         ${WORKSPACE}/Installer.rpm/awips2.core/Installer.localization/component.spec
      RC=$?
      unset LOCALIZATION_DIRECTORY
      unset COMPONENT_NAME
echo "BBBBBB the version in localization is ${AWIPSII_VERSION}"
echo "BBBBBB the release in localization is ${AWIPSII_RELEASE}"
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: Failed to build ${COMPONENT_NAME}."
         exit 1
      fi
   done
}

function buildVersionRPM()
{
   rm -rf ${BUILDROOT_DIR}

   export AWIPSII_BUILD_DATE=`date +"%m-%d-%Y"`
   export AWIPSII_BUILD_TIME=`date +"%T %Z"`
   export AWIPSII_BUILD_SYSTEM=`uname -n`
   export AWIPSII_BUILD_TAG=`perl ${WORKSPACE}/Installer.rpm/awips2.core/deploy.builder/extractTag.pl ${SVN}`

   rpmbuild -ba --target=i386 \
      --define '_topdir %(echo ${RPM_TOP_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_component_build_date %(echo ${AWIPSII_BUILD_DATE})' \
      --define '_component_build_time %(echo ${AWIPSII_BUILD_TIME})' \
      --define '_component_build_system %(echo ${AWIPSII_BUILD_SYSTEM})' \
      --define '_svn_tag %(echo ${AWIPSII_BUILD_TAG})' \
      --buildroot ${BUILDROOT_DIR} \
      ${COMPONENT_SPECS}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: The build of '${COMPONENT}' has FAILED."
      exit 1
   fi
}

function buildQpidRPMs()
{
   cd ${WORKSPACE_DIR}/Installer.rpm/awips2.qpid/deploy.builder

   chmod a+x build.sh
   time ./build.sh
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: The build of the QPID RPMs has FAILED."
      exit 1
   fi

   # If we reach this point, we have successfully built the
   # qpid rpms, so it is time to copy them to the workspace
   # "repository".

   # There are only three qpid rpms that we will want to copy.
   local QPID_CLIENT_RPM="awips2-qpid-client-0.7.946106-*.i386.rpm"
   local QPID_SERVER_RPM="awips2-qpid-server-0.7.946106-*.i386.rpm"
   local QPID_STORE_RPM="awips2-qpid-server-store-0.7.946106-*.i386.rpm"

   local QPID_RPM_DIR="${WORKSPACE_DIR}/Installer.rpm/awips2.qpid/RPMS/i386"
   cp -v ${QPID_RPM_DIR}/${QPID_CLIENT_RPM} \
      ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/core
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed To Copy ... ${QPID_CLIENT_RPM}."
      exit 1
   fi
   cp -v ${QPID_RPM_DIR}/${QPID_SERVER_RPM} \
      ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/core
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed To Copy ... ${QPID_SERVER_RPM}."
      exit 1
   fi
   cp -v ${QPID_RPM_DIR}/${QPID_STORE_RPM} \
      ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/core
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed To Copy ... ${QPID_STORE_RPM}."
      exit 1
   fi
}

function loopThroughAllCoreRPMs()
{
   SELECT_ALL_SQL="SELECT component FROM awips2_core_rpms ORDER BY component;"

echo "CCCCCC the version in core is ${AWIPSII_VERSION}"
echo "CCCCCC the release in core is ${AWIPSII_RELEASE}"
   # Select All RPMs From The Core DB
   for component in `echo ${SELECT_ALL_SQL} | sqlite3 ${DB_FILE}`; do
      COMPONENT="${component}"
      #We build the qpid-cluster RPMs as their own group because their
      #build has a different structure. 
      if [ ! "${COMPONENT}" = "awips2-httpd-pypies" ] &&
         [ ! "${COMPONENT}" = "awips2-eclipse" ]; then
         # Scan DB for Core RPMs
         SQL="SELECT buildDirectory FROM awips2_core_rpms WHERE component = '${COMPONENT}';"

         COMPONENT_DIR=`echo ${SQL} | sqlite3 ${DB_FILE}`
         # We will be building the rpms directly, instead of using the "batch" build scripts that are included in the rpm
         # build projects.
         COMPONENT_SPECS="${WORKSPACE_DIR}/Installer.rpm/${COMPONENT_DIR}/component.spec"

         if [ "${COMPONENT}" != "awips2" ]; then
            buildRPM
         else
            buildVersionRPM
         fi
      fi
echo "DDDDDD the version in core is ${AWIPSII_VERSION}"
echo "DDDDDD the release in core is ${AWIPSII_RELEASE}"
   done

   # Build The Localization RPMs
   buildLocalizationRPMs

   # Build The QPID RPMs
   buildQpidRPMs
}

function loopThroughSpecifiedRPMs()
{
   # Scan DB for Core RPMs
   for component in ${RPMS_TO_BUILD[*]}; do
      COMPONENT="${component}"
      SQL="SELECT buildDirectory FROM awips2_core_rpms WHERE component = '${COMPONENT}';"

      COMPONENT_DIR=`echo ${SQL} | sqlite3 ${DB_FILE}`
      # We will be building the rpms directly, instead of using the "batch" build scripts that are included in the rpm
      # build projects.
      if [ ! "${COMPONENT_DIR}" = "" ]; then
         COMPONENT_SPECS="${WORKSPACE_DIR}/Installer.rpm/${COMPONENT_DIR}/component.spec"
         buildRPM
      fi
   done
}


# For the PostgreSQL Build ~ by hudson user = tomcat
if [ -f /etc/profile.d/awipsPSQL.sh ]; then
   source /etc/profile.d/awipsPSQL.sh
fi

if [ "${BUILD_ALL_RPMS}" = "false" ]; then
   loopThroughSpecifiedRPMs
else
   loopThroughAllCoreRPMs
fi

updateCoreRepository
echo "INFO: Finished - Building AWIPS II Core RPMs."
