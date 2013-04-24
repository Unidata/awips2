#!/bin/bash

# Is awips2-psql Installed?
rpm -q awips2-psql > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   return
fi

# Determine Where awips2-psql Has Been Installed.
PSQL_INSTALL="/awips2/psql"

# Update The Environment.
# Determine if awips2-psql is Already On LD_LIBRARY_PATH
CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${PSQL_INSTALL}`
if [ "${CHECK_PATH}" = "" ]; then
   # awips2-psql Is Not On LD_LIBRARY_PATH; Add It.
   export LD_LIBRARY_PATH=${PSQL_INSTALL}/lib:${LD_LIBRARY_PATH}
fi

# Determine If awips2-psql Is Already Part Of The Path.
CHECK_PATH=`echo ${PATH} | grep ${PSQL_INSTALL}`
if [ "${CHECK_PATH}" = "" ]; then
   # awips2-psql Is Not In The Path; Add It To The Path.
   export PATH=${PSQL_INSTALL}/bin:${PATH}
fi
