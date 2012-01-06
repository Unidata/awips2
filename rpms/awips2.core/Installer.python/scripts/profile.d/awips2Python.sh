#!/bin/bash

# Is Python Installed?
rpm -q awips2-python > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   return
fi

PYTHON_INSTALL="/awips2/python"
# Update The Environment.
AWIPS_PYTHON=${PYTHON_INSTALL}
# Determine If Python Is Already Part Of The Path
CHECK_PATH=`echo ${PATH} | grep ${PYTHON_INSTALL}`
if [ "${CHECK_PATH}" = "" ]; then
   # Python Is Not In The Path; Add It.
   export PATH=${PYTHON_INSTALL}/bin:${PATH}
fi

# Determine If Python Is On LD_LIBRARY_PATH
CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${PYTHON_INSTALL}`
if [ "${CHECK_PATH}" = "" ]; then
   # Python Is Not On LD_LIBRARY_PATH, Add It.
   export LD_LIBRARY_PATH=${PYTHON_INSTALL}/lib:${LD_LIBRARY_PATH}
fi
