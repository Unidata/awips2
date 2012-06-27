#!/bin/bash

export LOCALAPPS_HOME=/localapps
export LOCALAPPS_RUN=${LOCALAPPS_HOME}/runtime
export LOCALAPPS_DEV=${LOCALAPPS_HOME}/dev
export LOCALAPPS_LIB_java=${LOCALAPPS_HOME}/lib/java
export LOCALAPPS_LIB_javascript=${LOCALAPPS_HOME}/lib/javascript
export LOCALAPPS_LIB_perl=${LOCALAPPS_HOME}/lib/perl
export LOCALAPPS_LIB_python=${LOCALAPPS_HOME}/lib/python
export LOCALAPPS_LIB_shell=${LOCALAPPS_HOME}/lib/shell
export LOCALAPPS_LIB_tcl=${LOCALAPPS_HOME}/lib/tcl
export LOCALAPPS_LOGS=${LOCALAPPS_HOME}/logs

CHECK_PATH=`echo ${PYTHONPATH} | grep "/awips2/fxa/bin/src"`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi
# Update PYTHONPATH
if [ "${PYTHONPATH}" = "" ]; then
   export PYTHONPATH=/awips2/fxa/bin/src
else  
   export PYTHONPATH=/awips2/fxa/bin/src:${PYTHONPATH}
fi  
