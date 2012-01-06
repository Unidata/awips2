#!/bin/bash

# 1) Ensure that the Update Registry exists.
# 2) Determine how many updates, if any, need to be applied for this build.
# 3) Step through the list of update scripts ...
#      3.1) source the script.
#      3.2) examine the build and the id; check the registry to determine if
#           the update has been made.
#      3.3) skip the script if the update has already been made.
#      3.4) run the script if the update was not completed.
#      3.5) check the return code after the execution to determine
#           if the update was successful or not.
#      3.6) if the update was successful, update the registry.

# Version: 1.0

# Variables
export CONST_REGISTRY_DIR="/usr/share/doc/awips2/delta"
export CONST_REGISTRY_DB_FILE="deltaRegistry.db"
CONST_REGISTRY_CREATION_SCRIPT="createUpdateRegistry.sh"
CONST_FUNCTION_TYPE="function"

UPDATE_SCRIPT=
VALID_UPDATE_IND=
UPDATE_REQUIRED_IND=
UPDATE_SUCCESS_IND=

# Arguments:
#   ${1} == the component that we may be updating.

COMPONENT="${1}"

# Ensure That The Update Registry Exists.
if [ ! -f ${CONST_REGISTRY_DIR}/${CONST_REGISTRY_DB_FILE} ]; then
   # Create The Update Registry Before Proceeding.
   /bin/bash ${CONST_REGISTRY_CREATION_SCRIPT}
fi

function validateUpdate()
{
   # Verify A Delta Build Has Been Specified.
   if [ "${DELTA_BUILD}" = "" ]; then
      VALID_UPDATE_IND="N"
      return
   fi

   # Verify A Delta ID Has Been Specified.
   if [ "${DELTA_ID}" = "" ]; then
      VALID_UPDATE_IND="N"
      return
   fi

   # Verify A Delta Description Has Been Specified.
   if [ "${DELTA_DESC}" = "" ]; then
      VALID_UPDATE_IND="N"
      return
   fi

   # Verify A 'runUpdate' Function Exists.
   local EVAL_TYPE=`type -t runUpdate`
   if [ ! "${EVAL_TYPE}" = "${CONST_FUNCTION_TYPE}" ]; then
      VALID_UPDATE_IND="N"
      return
   fi
}

function checkIfUpdateIsNecessary()
{
   local SQL="SELECT COUNT(*) FROM awips2_delta_registry WHERE "
   SQL="${SQL} deltaBuild=\"${DELTA_BUILD}\" AND "
   SQL="${SQL} deltaID=\"${DELTA_ID}\";"

   local COUNT=`echo ${SQL} | sqlite3 ${CONST_REGISTRY_DIR}/${CONST_REGISTRY_DB_FILE}`
   if [ ! "${COUNT}" = "0" ]; then
      UPDATE_REQUIRED_IND="N"
   fi
}

function applyUpdate()
{
   if [ ! "${DELTA_RUN_USER}" = "" ]; then
      su ${DELTA_RUN_USER} -c "source ${UPDATE_SCRIPT}; runUpdate" 
   else
      runUpdate
   fi

   local RC="$?"
   if [ ! "${RC}" = "0" ]; then
      UPDATE_SUCCESS_IND="N"
   fi
}

function logUpdate()
{
   DATE_APPLIED=`date +"%Y%m%d"`
   TIME_APPLIED=`date +"%H%M%S"`

   local SQL="INSERT INTO awips2_delta_registry VALUES("
   SQL="${SQL}\"${DELTA_BUILD}\", "
   SQL="${SQL}\"${DELTA_ID}\", "
   SQL="${SQL}\"${DELTA_DESC}\", ${DATE_APPLIED}, ${TIME_APPLIED});"

   echo ${SQL} | sqlite3 ${CONST_REGISTRY_DIR}/${CONST_REGISTRY_DB_FILE}
}

function update()
{
   local RES_COL=70
   local MOVE_TO_COL="echo -en \\033[${RES_COL}G"
   local SETCOLOR_SUCCESS="echo -en \\033[1;32m"
   local SETCOLOR_FAILURE="echo -en \\033[1;31m"
   local SETCOLOR_NORMAL="echo -en \\033[0;39m"

   source ${UPDATE_SCRIPT}

   VALID_UPDATE_IND="Y"
   validateUpdate
   
   if [ ! "${VALID_UPDATE_IND}" = "Y" ]; then
      return
   fi

   UPDATE_REQUIRED_IND="Y"
   checkIfUpdateIsNecessary

   if [ ! "${UPDATE_REQUIRED_IND}" = "Y" ]; then
      return
   fi

   UPDATE_SUCCESS_IND="Y"

   echo -n "Applying Update ... ${DELTA_ID}"

   applyUpdate
   sleep 10

   ${MOVE_TO_COL}
   echo -n "["
   if [ ! "${UPDATE_SUCCESS_IND}" = "Y" ]; then
      ${SETCOLOR_FAILURE}
      echo -n "FAILURE"
      ${SETCOLOR_NORMAL}
      echo -n "]"
      echo ""
      return
   fi

   ${SETCOLOR_SUCCESS}
   echo -n "SUCCESS"
   ${SETCOLOR_NORMAL}
   echo -n "]"
   echo ""
   logUpdate 
}

# Get a list of update scripts.
for script in ${COMPONENT}/*;
do
   UPDATE_SCRIPT="${script}"
   
   # Start The Update.
   update
done
