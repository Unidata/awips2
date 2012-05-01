#!/bin/bash
# We Have Been Created To Automate The Building Of The AWIPS II RPMs.
set -x
# We Need To Setup Our Environment.
source env.sh

echo "The AWIPSII Version is $AWIPSII_VERSION "
echo "The AWIPSII Release is $AWIPSII_RELEASE "



if [ "${RPM_TOP_DIR}" = "" ]; then
   echo "ERROR: You Must Set The RPM_TOP_DIR Environment Variable."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# !! NOTE !! - We Assume That We Are In A Workspace With The Installer Projects,
#              The Edex Projects, The Cave Projects, The Native Projects, And Etc.
export WORKSPACE_DIR=`cd ../../../; pwd;`

# The RPM Build Directory Structure Consists Of:
#	ROOT = /usr/src/redhat
#       * BUILD   - 
#	* RPMS    - Our Output RPMs
#	* SOURCES - Not Important In Phase I
#	* SPECS
#	* SRPMS

# Arguments
#	${1} == The Directory With The Specs File And Possibly Other Custom
#               Scripts That May Need To Be Merged Into A Component.

export AWIPSII_VERSION=`cat ${WORKSPACE_DIR}/Installer.rpm/version.txt`
export AWIPSII_RELEASE=`date +"%Y%m%d"`


echo "The AWIPSII Version is $AWIPSII_VERSION outside the buildRPM function"
echo "The AWIPSII Release is $AWIPSII_RELEASE outside the buildRPM function"


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

   if [ "${COMPONENT_DIR}" = "Installer.version" ]; then
      # Get the build information.
      export AWIPSII_BUILD_DATE=`date +"%m-%d-%Y"`
      export AWIPSII_BUILD_TIME=`date +"%T %Z"`
      export AWIPSII_BUILD_SYSTEM=`uname -n`

      echo "The AWIPSII Version is $AWIPSII_VERSION in the buildRPM function"
      echo "The AWIPSII Release is $AWIPSII_RELEASE in the buildRPM function"

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
         echo "Unable To Build The RPM Defined In: ${COMPONENT_DIR}."
         echo "Unable To Continue ... Terminating."
         exit 1
      fi

      unset AWIPSII_BUILD_DATE
      unset AWIPSII_BUILD_TIME
      unset AWIPSII_BUILD_SYSTEM

      return
   fi

   export BUILDROOT_DIR="${BUILDROOT_DIR}"
   # Build The RPM.
   rpmbuild -ba --target=i386 \
      --define '_topdir %(echo ${RPM_TOP_DIR})' \
      --define '_build_root %(echo ${BUILDROOT_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_baseline_workspace %(echo ${WORKSPACE_DIR})' \
      --buildroot ${BUILDROOT_DIR} \
      ${COMPONENT_SPECS}
   # If We Are Unable To Build An RPM, Fail The Build:
   RC="$?"
   unset BUILDROOT_DIR
   if [ ! "${RC}" = "0" ]; then
      if [ ! "${COMPONENT_DIR}" = "Installer.ant" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.httpd-pypies" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.irt" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.java" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.ldm" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.postgresql" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.psql" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.python" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.server" ] &&
         [ ! "${COMPONENT_DIR}" = "Installer.tools" ]; then
         echo "ERROR: Unable To Build The RPM Defined In: ${COMPONENT_DIR}."
         echo "Unable To Continue ... Terminating."
         exit 1
      fi
   fi
}

function buildLocalizationRPMs()
{
   BUILDROOT_DIR=/tmp/awips-component
   # Find all of the localization.${site} directories, if there are any.
   ls ${WORKSPACE_DIR}/localization.* > /dev/null 2>&1
   RC=$?
   if [ ${RC} -ne 0 ]; then
      # There are not any localization projects.
      echo "INFO: There are not any localization projects."
      return 0
   fi

   for dir in `cd ${WORKSPACE_DIR}; ls -1d localization.*`; do
      site=`perl extractSite.pl ${dir}`
      export LOCALIZATION_DIRECTORY="${dir}"
      export COMPONENT_NAME="awips2-localization-${site}"
      echo ${site}

      rm -rf ${BUILDROOT_DIR}

      rpmbuild -ba \
         --define '_topdir %(echo ${RPM_TOP_DIR})' \
         --define '_component_version %(echo ${AWIPSII_VERSION})' \
         --define '_component_release %(echo ${AWIPSII_RELEASE})' \
         --define '_component_name %(echo ${COMPONENT_NAME})' \
         --define '_baseline_workspace %(echo ${WORKSPACE_DIR})' \
         --define '_localization_directory %(echo ${LOCALIZATION_DIRECTORY})' \
         --buildroot ${BUILDROOT_DIR} \
         ../Installer.localization/component.spec
      RC=$?
      unset LOCALIZATION_DIRECTORY
      unset COMPONENT_NAME
      if [ ${RC} -ne 0 ]; then
         exit 1
      fi
   done
}

export CFLAGS="-m32 -O2"
export LDFLAGS="-m32"
export BASECFLAGS="-m32 -O2"
export LD="ld -melf_i386"
# Get A List Of The RPM Directories (Excluding This One)
# Note: Presently, We Are In ../../Installer.rpm/deploy.builder

buildLocalizationRPMs

# Adjust Our Execution Position.
cd ../

# Only Build The RPMs That May Have Changed - AWIPS II-Specific Components.
buildRPM "Installer.version"
buildRPM "Installer.gfesuite-client"
buildRPM "Installer.gfesuite-server"
buildRPM "Installer.database-standalone-configuration"
buildRPM "Installer.database-server-configuration"
buildRPM "Installer.adapt-native"
buildRPM "Installer.alertviz"
buildRPM "Installer.aviation"
buildRPM "Installer.cli"
buildRPM "Installer.database"
buildRPM "Installer.maps-database"
buildRPM "Installer.gfe.climo"
buildRPM "Installer.topo"
buildRPM "Installer.hydroapps"
buildRPM "Installer.notification"
buildRPM "Installer.pypies"
buildRPM "Installer.rcm"
buildRPM "Installer.localapps-environment"

unset AWIPSII_VERSION
unset AWIPSII_RELEASE
