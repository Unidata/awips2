#!/bin/bash

# Is Ant Installed?
rpm -q awips2-ant > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   return
fi

# Determine Where Ant Has Been Installed.
ANT_INSTALL=/awips2/ant

# Update The Environment.
export ANT_HOME="${ANT_INSTALL}"
# Determine If Ant Is Already Part Of The Path.
CHECK_PATH=`echo ${PATH} | grep ${ANT_INSTALL}`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi
# Ant Is Not In The Path; Add It To The Path.
export PATH="${ANT_INSTALL}/bin:${PATH}"
