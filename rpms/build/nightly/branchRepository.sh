#!/bin/bash

if [ "${REPO_SHARE_ROOT}" = "" ]; then
   echo "ERROR: the 'REPO_SHARE_ROOT' environment variable is UNDEFINED."
   exit 1
fi
if [ "${BUILD_DATE}" = "" ]; then
   echo "ERROR: the 'BUILD_DATE' environment variable is UNDEFINED."
   exit 1
fi


# Create a dated directory in the repo directory - archive any existing directories.
if [ -d ${REPO_SHARE_ROOT}/${BUILD_DATE} ]; then
   rm -rf ${REPO_SHARE_ROOT}/${BUILD_DATE}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
fi

# Create the nightly repo directory.
mkdir -p ${REPO_SHARE_ROOT}/${BUILD_DATE}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
# Create the categorized repo directories.
mkdir -p ${REPO_SHARE_ROOT}/${BUILD_DATE}/core
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${REPO_SHARE_ROOT}/${BUILD_DATE}/edex
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${REPO_SHARE_ROOT}/${BUILD_DATE}/python.site-packages
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${REPO_SHARE_ROOT}/${BUILD_DATE}/cave
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
