#!/bin/bash

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

      rpmbuild -bb \
         --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
         --define '_component_version %(echo ${AWIPSII_VERSION})' \
         --define '_component_release %(echo ${AWIPSII_RELEASE})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
         --define '_baseline_workspace %(echo ${WORKSPACE})' \
         --define '_localization_directory %(echo ${LOCALIZATION_DIRECTORY})' \
         --define '_localization_site %(echo ${site})' \
         --define '_build_site %(echo ${AWIPSII_BUILD_SITE})' \
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
   httpd_SOURCES=${httpd_pypies_directory}/src/httpd-2.2.15-SOURCES.tar

   /bin/tar -xvf ${httpd_SOURCES} -C ${AWIPSII_TOP_DIR}/SOURCES
   if [ $? -ne 0 ]; then
      return 1
   fi
   cp -vf ${httpd_pypies_directory}/SOURCES/* ${AWIPSII_TOP_DIR}/SOURCES
   if [ $? -ne 0 ]; then
      return 1
   fi

   return 0
}
