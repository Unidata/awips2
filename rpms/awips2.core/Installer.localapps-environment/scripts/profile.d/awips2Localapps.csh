#!/bin/csh

setenv LOCALAPPS_HOME "/localapps"
setenv LOCALAPPS_RUN "${LOCALAPPS_HOME}/runtime"
setenv LOCALAPPS_DEV "${LOCALAPPS_HOME}/dev"
setenv LOCALAPPS_LIB_java "${LOCALAPPS_HOME}/lib/java"
setenv LOCALAPPS_LIB_javascript "${LOCALAPPS_HOME}/lib/javascript"
setenv LOCALAPPS_LIB_perl "${LOCALAPPS_HOME}/lib/perl"
setenv LOCALAPPS_LIB_python "${LOCALAPPS_HOME}/lib/python"
setenv LOCALAPPS_LIB_shell "${LOCALAPPS_HOME}/lib/shell"
setenv LOCALAPPS_LIB_tcl "${LOCALAPPS_HOME}/lib/tcl"
setenv LOCALAPPS_LOGS "${LOCALAPPS_HOME}/logs"

if $?PYTHONPATH then
   setenv PYTHONPATH /awips2/fxa/bin/src:$PYTHONPATH
else
   setenv PYTHONPATH /awips2/fxa/bin/src
endif 
