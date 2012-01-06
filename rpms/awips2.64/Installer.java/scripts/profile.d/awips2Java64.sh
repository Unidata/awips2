#!/bin/bash

JAVA_INSTALL="/awips2/java"

if [ ! -d ${JAVA_INSTALL} ]; then
   return
fi

# Update the environment.
export JAVA_HOME=${JAVA_INSTALL}
# Determine if Java is already part of the path.
CHECK_PATH=`echo "${PATH}" | grep "${JAVA_INSTALL}"`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi
# Java is not in the path; add it to the path.
export PATH=${JAVA_INSTALL}/bin:${PATH}
