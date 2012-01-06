#!/bin/bash

# This script will determine if any delta scripts need to be included in the
# rpm that is being built based on the build.

# This file contains the current build information.
CONST_VERSION_TXT="Installer.rpm/version.txt"
CONST_SETUP_CORE_DIR="Installer.rpm/delta/setup"

function copySetupCore()
{
   # Arguments:
   #   ${1} == the current build root.
   #   ${2} == the default rpm prefix.

   local BUILD_ROOT="${1}"
   local RPM_DEFAULT_PREFIX="${2}"

   updateScripts=( 'createUpdateRegistry.sh' 'updateManager.sh' )

   if [ ! -d ${BUILD_ROOT}/${RPM_DEFAULT_PREFIX}/delta ]; then
      mkdir -p ${BUILD_ROOT}/${RPM_DEFAULT_PREFIX}/delta
   fi

   # Copy the update scripts.
   for script in ${updateScripts[*]};
   do
      cp ${WORKSPACE_DIR}/${CONST_SETUP_CORE_DIR}/${script} \
         ${BUILD_ROOT}/${RPM_DEFAULT_PREFIX}/delta
   done
}

function copyApplicableDeltas()
{
   # Arguments:
   #   ${1} == the current build root.
   #   ${2} == the component name.
   #   ${3} == the component project directory.
   #   ${4} == the default rpm prefix.

   local BUILD_ROOT="${1}"
   local COMPONENT_NAME="${2}"
   local COMPONENT_PROJECT_DIR="${3}"
   local RPM_DEFAULT_PREFIX="${4}"

   local CURRENT_BUILD=`cat ${WORKSPACE_DIR}/${CONST_VERSION_TXT}`

   # Determine if a delta directory exists for the component for the current build.
   EXPECTED_DIR="${WORKSPACE_DIR}/Installer.rpm/${COMPONENT_PROJECT_DIR}"
   EXPECTED_DIR="${EXPECTED_DIR}/delta/B${CURRENT_BUILD}"

   # Create The Delta Script Directory.
   if [ ! -d ${BUILD_ROOT}/${RPM_DEFAULT_PREFIX}/delta/${COMPONENT_NAME} ]; then
      mkdir -p ${BUILD_ROOT}/${RPM_DEFAULT_PREFIX}/delta/${COMPONENT_NAME}
   fi

   if [ ! -d ${EXPECTED_DIR} ]; then
      return
   fi

   # Determine How Many Delta Scripts There Are.
   COUNT=`ls -1 ${EXPECTED_DIR} | wc -l`
   
   if [ ${COUNT} -le 0 ]; then
      return
   fi

   cp ${EXPECTED_DIR}/* \
      ${BUILD_ROOT}/${RPM_DEFAULT_PREFIX}/delta/${COMPONENT_NAME}
} 
