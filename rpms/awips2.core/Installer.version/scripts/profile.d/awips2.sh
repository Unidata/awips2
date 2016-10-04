#!/bin/bash
PATH=/awips2/tools/bin:$PATH
if [[ ${USER} = "root" ]]; then
  alias ldmadmin='service edex_ldm'
  return
fi
alias cave='/awips2/cave/run.sh -alertviz -component thinclient'
PYTHON_INSTALL="/awips2/python"
CLI_INSTALL="/awips2/fxa"
GFESUITE_PATH="/awips2/GFESuite/bin"
HDF5_TOOLS_INSTALL="/awips2/tools"
NOTIFICATION_INSTALL="/awips2/notification"
QPID_LIB_DIR="/awips2/qpid/lib"
POSTGRESQL_INSTALL="/awips2/postgresql"
PSQL_INSTALL="/awips2/psql"
JAVA_INSTALL="/awips2/java"
LDM_INSTALL="/awips2/ldm"
AWIPS_PYTHON=${PYTHON_INSTALL}
export LDMHOME=${LDM_INSTALL}
export JAVA_HOME=${JAVA_INSTALL}
export YAJSW_HOME="/awips2/yajsw"
export GROOVY_HOME=/awips2/groovy
export PATH=${GROOVY_HOME}/bin:${PATH}
CAVE_INSTALL="/awips2/cave"
export TMCP_HOME=${CAVE_INSTALL}/caveEnvironment
export FXA_HOME=${CAVE_INSTALL}/caveEnvironment

if [ -d ${PYTHON_INSTALL} ]; then
   # Determine If Python Is Already Part Of The Path
   CHECK_PATH=`echo ${PATH} | grep ${PYTHON_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export PATH=${PYTHON_INSTALL}/bin:${PATH}
   fi
   # Determine If Python Is On LD_LIBRARY_PATH
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${PYTHON_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export LD_LIBRARY_PATH=${PYTHON_INSTALL}/lib:${LD_LIBRARY_PATH}
   fi
fi
if [ -d ${CLI_INSTALL}  ]; then
   export PATH=${CLI_INSTALL}/bin:${PATH}
fi
# Ensure that it is not already in the path
CHECK_PATH=`echo ${PATH} | grep ${GFESUITE_PATH}`
if [ "${CHECK_PATH}" = "" ]; then
   export PATH=${GFESUITE_PATH}:${PATH}
fi
if [ -d ${HDF5_TOOLS_INSTALL} ]; then
   # Determine If awips2-tools Is Already Part Of The Path.
   CHECK_PATH=`echo ${PATH} | grep ${HDF5_TOOLS_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export PATH=${HDF5_TOOLS_INSTALL}/bin:${PATH}
   fi
   # Determine If awips2-tools Is On LD_LIBRARY_PATH.
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${HDF5_TOOLS_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export LD_LIBRARY_PATH=${HDF5_TOOLS_INSTALL}/lib:${LD_LIBRARY_PATH}
   fi
fi
if [ -d ${JAVA_INSTALL} ]; then
   # Determine If Java Is Already Part Of The Path.
   CHECK_PATH=`echo ${PATH} | grep ${JAVA_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export PATH=${JAVA_INSTALL}/bin:${PATH}
   fi
fi
if [ -d ${NOTIFICATION_INSTALL} ]; then
   # Determine if awips2-notification is Already On LD_LIBRARY_PATH
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${NOTIFICATION_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      # awips2-notification Is Not On LD_LIBRARY_PATH; Add It.
      _lib_dir=${NOTIFICATION_INSTALL}/lib
      if [ -d ${NOTIFICATION_INSTALL}/lib64 ]; then
         _lib_dir=${NOTIFICATION_INSTALL}/lib64
      fi
      export LD_LIBRARY_PATH=${_lib_dir}:${LD_LIBRARY_PATH}
   fi
   # Determine if the qpid lib directory is already on LD_LIBRARY_PATH
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${QPID_LIB_DIR}`
   if [ "${CHECK_PATH}" = "" ]; then
      export LD_LIBRARY_PATH=${QPID_LIB_DIR}:$LD_LIBRARY_PATH
   fi
   # Determine if awips2-notification Is Already Part Of The Path.
   CHECK_PATH=`echo ${PATH} | grep ${NOTIFICATION_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export PATH=${NOTIFICATION_INSTALL}/bin:${PATH}
   fi
fi
if [ -d ${POSTGRESQL_INSTALL} ]; then
   # Determine if awips2-postgresql is Already On LD_LIBRARY_PATH
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${POSTGRESQL_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export LD_LIBRARY_PATH=${POSTGRESQL_INSTALL}/lib:${LD_LIBRARY_PATH}
   fi
   # Determine if awips2-postgresql Is Already Part Of The Path.
   CHECK_PATH=`echo ${PATH} | grep ${POSTGRESQL_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export PATH=${POSTGRESQL_INSTALL}/bin:${PATH}
   fi
fi
if [ -d ${PSQL_INSTALL} ]; then
   # Determine if awips2-psql is Already On LD_LIBRARY_PATH
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${PSQL_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export LD_LIBRARY_PATH=${PSQL_INSTALL}/lib:${LD_LIBRARY_PATH}
   fi
   # Determine If awips2-psql Is Already Part Of The Path.
   CHECK_PATH=`echo ${PATH} | grep ${PSQL_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      export PATH=${PSQL_INSTALL}/bin:${PATH}
   fi
fi
if [ -d ${LDM_INSTALL} ]; then
  CHECK_PATH=`echo ${PATH} | grep ldm`
  if [ "${CHECK_PATH}" = "" ]; then
    export PATH=${LDM_INSTALL}/bin:${LDM_INSTALL}/decoders:${LDM_INSTALL}/util:${PATH}
  fi
  CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${LDM_INSTALL}`
  if [ "${CHECK_PATH}" = "" ]; then
    export LD_LIBRARY_PATH=${LDM_INSTALL}/lib:${LD_LIBRARY_PATH}
  fi
fi

if [ -d /awips2/yajsw/lib/core/jna/com/sun/jna ]; then
  export LD_LIBRARY_PATH=/awips2/yajsw/lib/core/jna/com/sun/jna/linux-amd64:$LD_LIBRARY_PATH
fi
