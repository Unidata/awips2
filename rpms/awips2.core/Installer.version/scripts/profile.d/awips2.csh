#!/bin/csh
alias cave /awips2/cave/run.sh -alertviz -component thinclient
set JAVA_INSTALL="/awips2/java"
set PYTHON_INSTALL="/awips2/python"
set NOTIFICATION_INSTALL="/awips2/notification"
set QPID_LIB_DIR="/awips2/qpid/lib"
set GFESUITE_PATH="/awips2/GFESuite/bin"
set HDF5_TOOLS_INSTALL="/awips2/tools"
set POSTGRES_INSTALL="/awips2/postgresql"
set PSQL_INSTALL="/awips2/psql"
set CLI_INSTALL="/awips2/fxa"
setenv LDMHOME "/awips2/ldm"
setenv JAVA_HOME "${JAVA_INSTALL}"
setenv PATH ${LDMHOME}/bin:${LDMHOME}/decoders:${LDMHOME}/util:${CLI_INSTALL}/bin:${PSQL_INSTALL}/bin:${POSTGRES_INSTALL}/bin:${HDF5_TOOLS_INSTALL}/bin:${GFESUITE_PATH}:${NOTIFICATION_INSTALL}/bin:${PYTHON_INSTALL}/bin:${JAVA_INSTALL}/bin:$PATH
set CAVE_INSTALL="/awips2/cave"
setenv TMCP_HOME "${CAVE_INSTALL}/caveEnvironment"
setenv FXA_HOME "${CAVE_INSTALL}/caveEnvironment"
if $?LD_LIBRARY_PATH then
  setenv LD_LIBRARY_PATH /awips2/ldm/lib:${PSQL_INSTALL}/lib:${POSTGRES_INSTALL}/lib:${HDF5_TOOLS_INSTALL}/lib:${NOTIFICATION_INSTALL}/lib:${QPID_LIB_DIR}:${PYTHON_INSTALL}/lib:$LD_LIBRARY_PATH
else
  setenv LD_LIBRARY_PATH /awips2/ldm/lib:${PSQL_INSTALL}/lib:${POSTGRES_INSTALL}/lib:${HDF5_TOOLS_INSTALL}/lib:${NOTIFICATION_INSTALL}/lib:${QPID_LIB_DIR}:${PYTHON_INSTALL}/lib
endif
