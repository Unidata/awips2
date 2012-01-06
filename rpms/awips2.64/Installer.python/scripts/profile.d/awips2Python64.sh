#!/bin/bash

PYTHON_INSTALL="/awips2/python"

if [ ! -d ${PYTHON_INSTALL} ]; then
   return
fi

# Update the environment.
CHECK_PATH=`echo "${PATH}" | grep "${PYTHON_INSTALL}"`
if [ "${CHECK_PATH}" = "" ]; then
   export PATH=${PYTHON_INSTALL}/bin:${PATH}
fi

CHECK_PATH=`echo "${LD_LIBRARY_PATH}" | grep "${PYTHON_INSTALL}"`
if [ "${CHECK_PATH}" = "" ]; then
   export LD_LIBRARY_PATH=${PYTHON_INSTALL}/lib:${LD_LIBRARY_PATH}
fi
