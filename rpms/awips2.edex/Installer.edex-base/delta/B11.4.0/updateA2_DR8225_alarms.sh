#!/bin/bash

export DELTA_BUILD="11.4"
export DELTA_ID="A2_DR8225_alarms"
export DELTA_DESC="Remove User and Site Alarm Localization Files."

function runUpdate()
{
   local COMMON_STATIC_DIR="${COMPONENT_INSTALL}/edex/data/utility/common_static"
   local USER_LOCALIZATION="${COMMON_STATIC_DIR}/user"
   local SITE_LOCALIZATION="${COMMON_STATIC_DIR}/site"

   local USER_DIRECTORIES_EXIST="N"
   # Count The Number Of User Localization Directories.
   pushd . > /dev/null 2>&1
   cd ${USER_LOCALIZATION}
   local COUNT=`ls -1 | wc -l`
   if [ ${COUNT} -gt 0 ]; then
      USER_DIRECTORIES_EXIST="Y"
   fi
   popd > /dev/null 2>&1

   local SITE_DIRECTORIES_EXIST="N"
   # Count The Number Of Site Localization Directories.
   pushd . > /dev/null 2>&1
   cd ${SITE_LOCALIZATION}
   local COUNT=`ls -1 | wc -l`
   if [ "${COUNT}" -gt 0 ]; then
      SITE_DIRECTORIES_EXIST="Y"
   fi
   popd > /dev/null 2>&1

   # Loop Through All Of The User-Specific Directories.
   if [ "${USER_DIRECTORIES_EXIST}" = "Y" ]; then
      for dir in `ls -1 ${USER_LOCALIZATION}`;
      do
         local USER_DIR="${USER_LOCALIZATION}/${dir}"
         # Is There An 'alarms' Directory?
         if [ -d ${USER_DIR}/alarms ]; then
            # Remove All Files That End With .txt.
            rm -f ${USER_DIR}/alarms/*.txt
         fi
      done
   fi

   # Loop Through All Of The Site-Specific Directories.
   if [ "${SITE_DIRECTORIES_EXIST}" = "Y" ]; then
      for dir in `ls -1 ${SITE_LOCALIZATION}`;
      do
         local SITE_DIR="${SITE_LOCALIZATION}/${dir}"
         # Is There An 'alarms' Directory?
         if [ -d ${SITE_DIR}/alarms ]; then
            # Remove All Files That End With .txt.
            rm -f ${SITE_DIR}/alarms/*.txt
         fi
      done
   fi

   return 0
}
