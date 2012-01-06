#!/bin/bash

SCRIPT_DIR=`dirname $_`
source awips/Installer.rpm/common/functions/rpmBuild.sh
prepareEnvironment
function updateFilesystemRepository()
{
   # ${1} = RPM Class
   RPM_CLASS="${1}"

   if [ "${AWIPSII_RPM_REPOSITORY}" = "" ]; then
      return 0
   fi

   if [ ! -d ${AWIPSII_RPM_REPOSITORY} ]; then
      mkdir -p ${AWIPSII_RPM_REPOSITORY}
      chmod -R 777 ${AWIPSII_RPM_REPOSITORY}
      RC=$?
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: Unable to create directory '${AWIPSII_RPM_REPOSITORY}'."
         exit 1
      fi
   fi

   local REPO_DIR="${AWIPSII_RPM_REPOSITORY}/${AWIPSII_VERSION}-${AWIPSII_RELEASE}"
   if [ ! -d ${REPO_DIR} ]; then
      mkdir -p ${REPO_DIR}
      RC=$?
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: Unable to create directory '${REPO_DIR}'."
         exit 1
      fi
      chmod -R 777 ${REPO_DIR}
   fi

   if [ ! -d ${REPO_DIR}/${RPM_CLASS} ]; then
      mkdir -p ${REPO_DIR}/${RPM_CLASS}
      RC=$?
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: Unable to create directory '${REPO_DIR}/${RPM_CLASS}'."
         exit 1
      fi
      chmod -R 777 ${REPO_DIR}/${RPM_CLASS}
   fi

   cp -v awips/Installer.rpm/rpmbuild/RPMS/i386/* ${REPO_DIR}/${RPM_CLASS}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Unable to copy the rpms to the repository directory - ${REPO_DIR}/${RPM_CLASS}."
      exit 1
   fi
   chmod 644 ${REPO_DIR}/${RPM_CLASS}/*

   rm -fv awips/Installer.rpm/rpmbuild/RPMS/i386/*
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Unable to remove the rpms from the build directory."
      exit 1
   fi
}

if [ "${BUILD_BASE}" = "true" ]; then
   pushd . > /dev/null
   cd awips/Installer.rpm/awips2.base/deploy.builder
   chmod a+x *.sh
   ./build2.0.sh
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed to build the BASE AWIPS II RPMs."
      exit 1
   fi
   popd > /dev/null

   updateFilesystemRepository "base"
fi

if [ "${BUILD_CAVE}" = "true" ]; then
   pushd . > /dev/null
   cd awips/Installer.rpm/awips2.cave/deploy.builder
   chmod a+x *.sh
   ./build2.0.sh
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed to build the CAVE AWIPS II RPMs."
      exit 1
   fi
   popd > /dev/null

   updateFilesystemRepository "cave"
fi

if [ "${BUILD_CORE}" = "true" ]; then
   pushd . > /dev/null
   cd awips/Installer.rpm/awips2.core/deploy.builder
   chmod a+x *.sh
   ./build2.0.sh
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed to build the CORE AWIPS II RPMs."
      exit 1
   fi
   popd > /dev/null

   updateFilesystemRepository "core"
fi

if [ "${BUILD_EDEX}" = "true" ]; then
   pushd . > /dev/null
   cd awips/Installer.rpm/awips2.edex/deploy.builder
   chmod a+x *.sh
   ./build2.0.sh
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed to build the EDEX AWIPS II RPMs."
      exit 1
   fi
   popd > /dev/null

   updateFilesystemRepository "edex"
fi

if [ "${BUILD_PYTHON}" = "true" ]; then
   pushd . > /dev/null
   cd awips/Installer.rpm/awips2.python/deploy.builder
   chmod a+x *.sh
   ./build2.0.sh
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed to build the Python AWIPS II RPMs."
      exit 1
   fi
   popd > /dev/null

   updateFilesystemRepository "python"
fi
