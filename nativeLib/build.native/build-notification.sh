#!/bin/bash

# Constants:
__ECLIPSE_="eclipse"
__NO_SPLASH_="-nosplash"
__APPLICATION_="-application"
__CDT_HEADLESS_="org.eclipse.cdt.managedbuilder.core.headlessbuild"
__IMPORT_="-import"
__DATA_="-data"
__BUILD_="-build"
__BUILD_CONFIG_32_="Build x86"
__BUILD_CONFIG_64_="Build x86_64"

BUILD_CONFIGURATION=
FOSS_LIB_DIR="lib"
# determine which configuration to build
_arch=`uname -i`
if [ "${_arch}" = "x86_64" ]; then
   BUILD_CONFIGURATION=${__BUILD_CONFIG_64_}
   FOSS_LIB_DIR="lib64"
else
   BUILD_CONFIGURATION=${__BUILD_CONFIG_32_}
fi

# Arguments:
#  1) the workspace
#  2) the uframe-eclipse
#  3) the build root (will become the location of the temporary workspace)

# ensure that there are at least three arguments
if [ $# -ne 3 ]; then
   echo "Usage: $0 WORKSPACE UFRAME_ECLIPSE BUILD_ROOT"
   exit 0
fi

WORKSPACE=${1}
UFRAME_ECLIPSE=${2}
BUILD_ROOT=${3}

# ensure that the workspace directory exists
if [ ! -d ${WORKSPACE} ]; then
   echo "ERROR: the specified workspace does not exist - ${WORKSPACE}"
   exit 1
fi
# ensure that Eclipse is present
if [ ! -f ${UFRAME_ECLIPSE}/${__ECLIPSE_} ]; then
   echo "ERROR: the Eclipse executable does not exist at the specified location - ${UFRAME_ECLIPSE}"
   exit 1
fi
# ensure that the build root exists
if [ ! -d ${BUILD_ROOT} ]; then
   echo "ERROR: the specified build root does not exist - ${BUILD_ROOT}"
   exit 1
fi
if [ -d ${BUILD_ROOT}/workspace_ ]; then
   rm -rf ${BUILD_ROOT}/workspace_
   if [ $? -ne 0 ]; then
      echo "ERROR: unable to remove the existing temporary workspace in the specified build root - ${BUILD_ROOT}"
      exit 1
   fi
fi
mkdir ${BUILD_ROOT}/workspace_
if [ $? -ne 0 ]; then
   echo "ERROR: unable to create a temporary workspace in the specified build root - ${BUILD_ROOT}"
   exit 1
fi

PROJECTS_TO_IMPORT=( "org.apache.thrift" "org.apache.qpid" )
PROJECTS_TO_BUILD=( "edex_com" "edex_notify" )

for project in ${PROJECTS_TO_IMPORT[*]};
do
   if [ ! -d ${WORKSPACE}/${project} ]; then
      echo "ERROR: required project ${project} is not available in the specified workspace."
      exit 1
   fi

   # copy the project to the workspace; we do not want to disturb the original
   cp -r ${WORKSPACE}/${project} ${BUILD_ROOT}/workspace_
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to copy project ${project} to the temporary workspace."
      exit 1
   fi

   # import the project into the workspace
   ${UFRAME_ECLIPSE}/${__ECLIPSE_} ${__NO_SPLASH_} ${__APPLICATION_} \
      ${__CDT_HEADLESS_} ${__IMPORT_} ${BUILD_ROOT}/workspace_/${project} \
      ${__DATA_} ${BUILD_ROOT}/workspace_
   if [ $? -ne 0 ]; then
      echo "ERROR: failed to import ${project} into the temporary workspace."
      exit 1
   fi
done

for project in ${PROJECTS_TO_BUILD[*]};
do
   if [ ! -d ${WORKSPACE}/${project} ]; then
      echo "ERROR: required project ${project} is not available in the specified workspace."
      exit 1
   fi

   # copy the project to the workspace; we do not want to disturb the original
   cp -r ${WORKSPACE}/${project} ${BUILD_ROOT}/workspace_
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to copy project ${project} to the temporary workspace."
      exit 1
   fi

   # import the project into the workspace and build the project
   ${UFRAME_ECLIPSE}/${__ECLIPSE_} ${__NO_SPLASH_} ${__APPLICATION_} \
      ${__CDT_HEADLESS_} ${__IMPORT_} ${WORKSPACE}/${project} \
      ${__DATA_} ${BUILD_ROOT}/workspace_ ${__BUILD_} "${project}/${BUILD_CONFIGURATION}"
   if [ $? -ne 0 ]; then
      echo "ERROR: failed to build project ${project}"
      exit 1
   fi
done

# create the notification sub-directories
mkdir ${BUILD_ROOT}/awips2/notification/bin
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to create directory - ${BUILD_ROOT}/workspace_/notification/bin."
   exit 1
fi
mkdir ${BUILD_ROOT}/awips2/notification/lib
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to create directory - ${BUILD_ROOT}/workspace_/notification/lib."
   exit 1
fi
mkdir ${BUILD_ROOT}/awips2/notification/src
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to create directory - ${BUILD_ROOT}/workspace_/notification/src."
   exit 1
fi
mkdir ${BUILD_ROOT}/awips2/notification/include
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to create directory - ${BUILD_ROOT}/workspace_/notification/include."
   exit 1
fi

# libedex_com.so -> notification/lib
cp -v "${BUILD_ROOT}/workspace_/edex_com/${BUILD_CONFIGURATION}/libedex_com.so" \
   ${BUILD_ROOT}/awips2/notification/lib
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to copy libedex_com.so to its destination."
   exit 1
fi
# edex_com src -> notification/src
cp -vf ${BUILD_ROOT}/workspace_/edex_com/src/*.cpp \
   ${BUILD_ROOT}/awips2/notification/src
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to copy the edex_com src to its destination."
   exit 1
fi
# edex_com headers -> notification/include
cp -vf ${BUILD_ROOT}/workspace_/edex_com/src/*.h ${BUILD_ROOT}/awips2/notification/include
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to copy the edex_com header to its destination."
   exit 1
fi
# edex_notify -> notification/bin
cp -vf "${BUILD_ROOT}/workspace_/edex_notify/${BUILD_CONFIGURATION}/edex_notify" \
   ${BUILD_ROOT}/awips2/notification/bin
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to copy edex_notify to its destination."
   exit 1
fi
# edex_notify src -> notification/src
cp -vf ${BUILD_ROOT}/workspace_/edex_notify/src/*.c \
   ${BUILD_ROOT}/awips2/notification/src
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to copy the edex_notify src to its destination."
   exit 1
fi
# org.apache.thrift lib -> notification/lib
cp -vPf ${BUILD_ROOT}/workspace_/org.apache.thrift/${FOSS_LIB_DIR}/* \
   ${BUILD_ROOT}/awips2/notification/lib
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to copy the org.apache.thrift lib to its destination."
   exit 1
fi
# org.apache.qpid lib -> notification/lib
cp -vPf ${BUILD_ROOT}/workspace_/org.apache.qpid/${FOSS_LIB_DIR}/* \
   ${BUILD_ROOT}/awips2/notification/lib
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to copy the org.apache.qpid lib to its destination."
   exit 1
fi

exit 0
