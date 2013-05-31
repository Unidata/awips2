#!/bin/bash

# Is Java Installed?
rpm -q awips2-java > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   return
fi

JAVA_INSTALL="/awips2/java"
# Update The Environment
export JAVA_HOME=${JAVA_INSTALL}
# Determine If Java Is Already Part Of The Path.
CHECK_PATH=`echo ${PATH} | grep ${JAVA_INSTALL}`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi
# Java Is Not In The Path; Add It To The Path.
export PATH=${JAVA_INSTALL}/bin:${PATH}
