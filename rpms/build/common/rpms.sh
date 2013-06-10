#!/bin/bash

function buildJava()
{
   pushd . > /dev/null 2>&1

   cd ${WORKSPACE}/rpms/awips2.core/Installer.java
   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      return 1
   fi

   popd > /dev/null 2>&1
}

function buildQPID()
{
   # Arguments:
   #   ${1} == optionally -ade

   pushd . > /dev/null 2>&1

   cd ${WORKSPACE}/rpms/awips2.qpid
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the qpid rpms."
      return 1
   fi

   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the qpid rpms."
      return 1
   fi

   # ensure that the destination rpm directories exist
   if [ ! -d ${AWIPSII_TOP_DIR}/RPMS/noarch ]; then
      mkdir -p ${AWIPSII_TOP_DIR}/RPMS/noarch
      if [ $? -ne 0 ]; then
         exit 1
      fi
   fi
   if [ ! -d ${AWIPSII_TOP_DIR}/RPMS/i386 ]; then
      mkdir -p ${AWIPSII_TOP_DIR}/RPMS/i386
      if [ $? -ne 0 ]; then
         exit 1
      fi
   fi

   pushd . > /dev/null 2>&1
   # Copy the 0.18 qpid rpms
   cd 0.18/RPMS/noarch
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build Qpid v0.18."
      return 1
   fi
   /bin/cp -v *.rpm ${AWIPSII_TOP_DIR}/RPMS/noarch
   if [ $? -ne 0 ]; then
      return 1
   fi
   popd > /dev/null 2>&1

   pushd . > /dev/null 2>&1
   # Copy the 0.7 qpid rpms
   cd 0.7/RPMS/i386
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build Qpid v0.7."
      return 1
   fi

   # Copy the qpid rpms from the local build directory to the rpm
   # "staging" directory.
   /bin/cp -v awips2-qpid-client-0.7.946106-*.i386.rpm \
      ${AWIPSII_TOP_DIR}/RPMS/i386/
   if [ $? -ne 0 ]; then
      return 1
   fi
   /bin/cp -v awips2-qpid-server-0.7.946106-*.i386.rpm \
      ${AWIPSII_TOP_DIR}/RPMS/i386/
   if [ $? -ne 0 ]; then
      return 1
   fi
   /bin/cp -v awips2-qpid-server-store-0.7.946106-*.i386.rpm \
      ${AWIPSII_TOP_DIR}/RPMS/i386/
   if [ $? -ne 0 ]; then
      return 1
   fi

   # if the -ade argument has been specified. Also copy the qpid
   # devel rpms.
   if [ "${1}" = "-ade" ]; then
      /bin/cp -v awips2-qpid-client-devel-0.7.946106-*.i386.rpm \
         ${AWIPSII_TOP_DIR}/RPMS/i386/
      if [ $? -ne 0 ]; then
         return 1
      fi

      /bin/cp -v awips2-qpid-client-devel-docs-0.7.946106-*.i386.rpm \
         ${AWIPSII_TOP_DIR}/RPMS/i386/
      if [ $? -ne 0 ]; then
         return 1
      fi

      /bin/cp -v awips2-qpid-server-devel-0.7.946106-*.i386.rpm \
         ${AWIPSII_TOP_DIR}/RPMS/i386/
      if [ $? -ne 0 ]; then
         return 1
      fi

      /bin/cp qpid-cpp-mrg-debuginfo-0.7.946106-*.i386.rpm \
         ${AWIPSII_TOP_DIR}/RPMS/i386/
      if [ $? -ne 0 ]; then
         return 1
      fi
   fi

   popd > /dev/null 2>&1
   popd > /dev/null 2>&1

   return 0
}

function buildEDEX()
{
   cd ${WORKSPACE}/rpms/awips2.edex/deploy.builder
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the edex rpms."
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
      echo "ERROR: Failed to build the edex rpms."
      return 1
   fi

   return 0
}

function buildCAVE()
{
   cd ${WORKSPACE}/rpms/awips2.cave/deploy.builder
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the cave rpms."
      return 1
   fi

   # Determine the build architecture.
   export CAVE_BUILD_ARCH=`uname -i`
   if [ "${CAVE_BUILD_ARCH}" = "i386" ]; then
      export CAVE_BUILD_ARCH="x86"
   fi

   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to determine the architecture."
      return 1
   fi
   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the cave rpms."
      return 1
   fi

   return 0
}

function buildLocalizationRPMs()
{
   awips2_core_directory=${WORKSPACE}/rpms/awips2.core
   extract_site_pl="${awips2_core_directory}/deploy.builder/extractSite.pl"
   installer_localization_directory="${awips2_core_directory}/Installer.localization"
   localization_SPECIFICATION="${installer_localization_directory}/component.spec"

   # Find all of the localization.${site} directories, if there are any.
   ls ${WORKSPACE}/localization.* > /dev/null 2>&1
   RC=$?
   if [ ${RC} -ne 0 ]; then
      # There are not any localization projects.
      echo "INFO: There are not any localization projects."
      return 0
   fi

   for localization in `cd ${WORKSPACE}; ls -1d localization.*;`; do
      site=`/usr/bin/perl ${extract_site_pl} ${localization}`
      if [ $? -ne 0 ]; then
         return 1
      fi
      export LOCALIZATION_DIRECTORY="${localization}"
      export COMPONENT_NAME="awips2-localization-${site}"
      export site=${site}

      echo "Building localization rpm for site: ${site}."

      rpmbuild -ba \
         --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
         --define '_component_version %(echo ${AWIPSII_VERSION})' \
         --define '_component_release %(echo ${AWIPSII_RELEASE})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
         --define '_baseline_workspace %(echo ${WORKSPACE})' \
         --define '_localization_directory %(echo ${LOCALIZATION_DIRECTORY})' \
         --define '_localization_site %(echo ${site})' \
         --buildroot ${AWIPSII_BUILD_ROOT} \
         ${localization_SPECIFICATION}
      RC=$?
      unset LOCALIZATION_DIRECTORY
      unset COMPONENT_NAME
      if [ ${RC} -ne 0 ]; then
         return 1
      fi
   done

   return 0
}

function unpackHttpdPypies()
{
   # This function will unpack the httpd-pypies SOURCES
   # into the: ${AWIPSII_TOP_DIR}/SOURCES directory.
   awips2_core_directory=${WORKSPACE}/rpms/awips2.core
   httpd_pypies_directory=${awips2_core_directory}/Installer.httpd-pypies
   httpd_SOURCES=${httpd_pypies_directory}/src/httpd-2.2.3-SOURCES.tar

   /bin/tar -xvf ${httpd_SOURCES} -C ${AWIPSII_TOP_DIR}/SOURCES
   if [ $? -ne 0 ]; then
      return 1
   fi

   return 0
}
