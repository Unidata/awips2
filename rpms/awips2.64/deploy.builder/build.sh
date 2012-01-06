#!/bin/bash

# Build Variables:
# -----------------------------------------------------------------------------
VAR_AWIPSII_TOP_DIR="/home/bkowal/rpmbuild"
VAR_WORKSPACE="/common/bkowal/workspace"
VAR_AWIPSII_BUILD_ROOT="/tmp/awips-component"
VAR_AWIPSII_VERSION=""
VAR_AWIPSII_RELEASE=""
# -----------------------------------------------------------------------------

# DO NOT EDIT ANYTHING BELOW THIS LINE!
function prepareBuildEnvironment()
{
   if [ "${AWIPSII_TOP_DIR}" = "" ]; then
      export AWIPSII_TOP_DIR="${VAR_AWIPSII_TOP_DIR}"
   fi

   if [ "${WORKSPACE}" = "" ]; then
      export WORKSPACE="${VAR_WORKSPACE}"
   fi

   if [ "${AWIPSII_BUILD_ROOT}" = "" ]; then
      export AWIPSII_BUILD_ROOT="${VAR_AWIPSII_BUILD_ROOT}"
   fi

   if [ "${AWIPSII_VERSION}" = "" ]; then
      # Determine if we need to use the default version.
      if [ "${VAR_AWIPSII_VERSION}" = "" ]; then
         VAR_AWIPSII_VERSION=`cat ${WORKSPACE}/Installer.rpm/version.txt`
      fi
      export AWIPSII_VERSION="${VAR_AWIPSII_VERSION}"
   fi

   if [ "${AWIPSII_RELEASE}" = "" ]; then
      # Determine if we need to use the default release.
      if [ "${VAR_AWIPSII_RELEASE}" = "" ]; then
         VAR_AWIPSII_RELEASE=`date +"%Y%m%d"`
      fi
      export AWIPSII_RELEASE="${VAR_AWIPSII_RELEASE}"
   fi
}

RPM_SPECS_DIR=""
function lookupRPM()
{
   # This function is used to lookup the location of a specs file
   # based on the name of an rpm.

   # $1 == the name of the rpm we would like to build.
   if [ "${1}" = "" ]; then
      return 1
   fi
   local RPM_64BIT_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.64"

   if [ "${1}" = "awips2-python-dynamicserialize" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.dynamicserialize"
      return 0
   fi

   if [ "${1}" = "awips2-python-h5py" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.h5py"
      return 0
   fi

   if [ "${1}" = "awips2-java" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.java"
      return 0
   fi

   if [ "${1}" = "awips2-python-jimporter" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.jimporter"
      return 0
   fi

   if [ "${1}" = "awips2-python-matplotlib" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.matplotlib"
      return 0
   fi

   if [ "${1}" = "awips2-python-nose" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.nose"
      return 0
   fi

   if [ "${1}" = "awips2-python-numpy" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.numpy"
      return 0
   fi

   if [ "${1}" = "awips2-python-pil" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.pil"
      return 0
   fi

   if [ "${1}" = "awips2-python-pmw" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.pmw"
      return 0
   fi

   if [ "${1}" = "awips2-python-pupynere" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.pupynere"
      return 0
   fi

   if [ "${1}" = "awips2-python" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.python"
      return 0
   fi

   if [ "${1}" = "awips2-python-qpid" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.qpid"
      return 0
   fi

   if [ "${1}" = "awips2-python-scientific" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.scientific"
      return 0
   fi

   if [ "${1}" = "awips2-python-scipy" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.scipy"
      return 0
   fi

   if [ "${1}" = "awips2-python-tables" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.tables"
      return 0
   fi

   if [ "${1}" = "awips2-python-thrift" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.thrift"
      return 0
   fi

   if [ "${1}" = "awips2-python-tpg" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.tpg"
      return 0
   fi

   if [ "${1}" = "awips2-python-ufpy" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.ufpy"
      return 0
   fi

   if [ "${1}" = "awips2-python-werkzeug" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.werkzeug"
      return 0
   fi

   if [ "${1}" = "awips2-python-pygtk" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/../python.site-packages/Installer.pygtk"
      return 0
   fi

   if [ "${1}" = "awips2-python-pycairo" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/../python.site-packages/Installer.pycairo"
      return 0
   fi

   if [ "${1}" = "awips2-cave" ]; then
      RPM_SPECS_DIR="_BUILD_CAVE_"
      return 0
   fi

   if [ "${1}" = "awips2-alertviz" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.alertviz"
      return 0
   fi

   RPM_SPECS_DIR="<Unknown>"
   return 1
}

function buildRPM()
{
   # $1 == the component file for the rpm that we would like to build.
   if [ -f ${AWIPSII_TOP_DIR}/BUILD/component-files.txt ]; then
      rm -fv ${AWIPSII_TOP_DIR}/BUILD/component-files.txt
      RC=$?
      if [ ${RC} -ne 0 ]; then
         exit 1
      fi
   fi

   if [ -d ${AWIPSII_BUILD_ROOT} ]; then
      rm -rf ${AWIPSII_BUILD_ROOT}
      RC=$?
      if [ ${RC} -ne 0 ]; then
         exit 1
      fi
   fi

   rpmbuild -ba --target=x86_64 \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --buildroot ${AWIPSII_BUILD_ROOT} \
      ${1}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      return 1
   fi
}

# Special Cases:
function buildCAVE()
{
   export CAVE_BUILD_ARCH="x86_64"
   export RPM_TOP_DIR="${AWIPSII_TOP_DIR}"
   cd ${WORKSPACE}/Installer.rpm/awips2.cave/deploy.builder
   time ./build.sh "${AWIPSII_VERSION}" "${AWIPSII_RELEASE}"
   RC=$?
   if [ ${RC} -ne 0 ]; then
      return 1
   fi
   return 0
}

# Initialize our environment.
prepareBuildEnvironment

# Determine if any arguments have been passed to us.
if [ $# -eq 0 ]; then
   # We will be building all 64-bit RPMs.
   for dir in `ls -1d ${WORKSPACE}/Installer.rpm/awips2.64/Installer.*`;
   do
      buildRPM ${dir}/component.spec
      RC=$?
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: Failed to build the 64-Bit RPMs."
         exit 1
      fi
   done
   buildCAVE
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Failed to build the 64-Bit RPMs."
      exit 1
   fi
   lookupRPM "awips2-python-pygtk"
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM ${RPM_SPECS_DIR}/component.spec
   if [ $? -ne 0 ]; then
      exit 1
   fi
   lookupRPM "awips2-python-pycairo"
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM ${RPM_SPECS_DIR}/component.spec
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

if [ $# -gt 0 ]; then
   # We will attempt to build the rpms the user specified.
   for arg in $@; do
      lookupRPM ${arg}
      RC=$?
      if [ "${RC}" -ne 0 ]; then
         echo "ERROR: Invalid RPM Specifier: ${arg}."
         exit 1
      fi

      if [ ! "${RPM_SPECS_DIR}" = "_BUILD_CAVE_" ]; then
         buildRPM ${RPM_SPECS_DIR}/component.spec
         RC=$?
      else
         buildCAVE
         RC=$?
      fi
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: Failed to build - ${arg}."
         exit 1
      fi
   done
fi
