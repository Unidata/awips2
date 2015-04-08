#!/bin/bash

if [ -d /awips2/java ]; then
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
fi
