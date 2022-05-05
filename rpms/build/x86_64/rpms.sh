#!/bin/bash

function buildRPM()
{
   local name=$(echo "${1}" | awk -F/ '{print $1}')
   local arch=$(echo "${1}" | awk -F/ '{print $2}')
   # Arguments:
   #   ${1} == the name of the rpm.
   findSpecFile "${name}"
   if [ $? -ne 0 ]; then
      echo "ERROR: '${name}' is not a recognized AWIPS II RPM."
      exit 1
   fi

        buildRPMExec "${RPM_SPECIFICATION}" "" "${arch}"

   return 0
}

# Arguments
#   ${1} == The component name to find a spec file for.
# Note: This function will scan the baseline lookup script first
#  then if not found continue looking in the external repos until
#  the first match is found. 
#
# Returns 0 if found and sets ${RPM_SPECIFICATION}
# Returns 1 if not found.
function findSpecFile()
{
    local _component=${1}

    lookupRPM "${_component}"
    if [ $? -eq 0 ]; then
       return 0
    fi

    # Not found in baseline scan the external repos
    for rpm_contribution in `ls -1d ${WORKSPACE}/rpms-*`; do

        local _build_dir="${rpm_contribution}/build"
        local _lookup_script="${_build_dir}/lookupWA_RPM.sh"

        source "${_lookup_script}"
        if [ $? -ne 0 ]; then
            echo "ERROR: unable to access the expected WA lookup script - ${_lookup_script}"
            exit 1
        fi

        lookupWA_RPM "${_component}" "${rpm_contribution}"
        if [ $? -eq 0 ]; then
           return 0
        fi
    done
    return 1
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
   local arch=${3}

   arch_flags=()
   if [ -n "$arch" ]; then
       arch_flags+=(setarch "${arch}")
   fi

   "${arch_flags[@]}" /usr/bin/rpmbuild -bb \
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

