#!/bin/bash

# Build Variables:
# -----------------------------------------------------------------------------
VAR_AWIPSII_TOP_DIR="/home/bkowal/rpmbuild"
VAR_WORKSPACE="/common/bkowal/git/thunder/baseline"
VAR_AWIPSII_BUILD_ROOT="/tmp/awips-component"
VAR_AWIPSII_VERSION=""
VAR_AWIPSII_RELEASE=""
VAR_UFRAME_ECLIPSE="/opt/uframe-eclipse"
VAR_AWIPSCM_SHARE="/awipscm"
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

   if [ "${UFRAME_ECLIPSE}" = "" ]; then
      export UFRAME_ECLIPSE="${VAR_UFRAME_ECLIPSE}"
   fi

   if [ "${AWIPSCM_SHARE}" = "" ]; then
      export AWIPSCM_SHARE="${VAR_AWIPSCM_SHARE}"
   fi
}

RPM_SPECS_DIR=""
function lookupRPM()
{
   export TARGET_ARCH="x86_64"
   # This function is used to lookup the location of a specs file
   # based on the name of an rpm.

   # $1 == the name of the rpm we would like to build.
   if [ "${1}" = "" ]; then
      return 1
   fi
   local RPM_64BIT_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.64"
   local RPM_CAVE_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.cave"
   local RPM_CORE_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.core"
   local RPM_EDEX_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.edex"

   if [ "${1}" = "awips2-python-dynamicserialize" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.dynamicserialize"
      return 0
   fi

   if [ "${1}" = "awips2-python-h5py" ]; then
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/Installer.h5py"
      return 0
   fi

   if [ "${1}" = "awips2-java" ]; then
      RPM_SPECS_DIR="${RPM_CORE_PROJECT_DIR}/Installer.java"
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
      RPM_SPECS_DIR="${RPM_64BIT_PROJECT_DIR}/../awips2.core/Installer.python"
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

   if [ "${1}" = "CAVE" ]; then
      RPM_SPECS_DIR="_BUILD_CAVE_"
      return 0
   fi

   if [ "${1}" = "EDEX" ]; then
      RPM_SPECS_DIR="_BUILD_EDEX_"
      return 0
   fi

   if [ "${1}" = "awips2-alertviz" ]; then
      RPM_SPECS_DIR="${RPM_CAVE_PROJECT_DIR}/Installer.alertviz"
      return 0
   fi

   if [ "${1}" = "awips2-edex-base" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-base"
      return 0
   fi

   if [ "${1}" = "awips2-edex-configuration" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-configuration"
      return 0
   fi

   if [ "${1}" = "awips2-edex-shapefiles" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-shapefiles"
      return 0
   fi

   if [ "${1}" = "awips2-edex-native" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-native"
      export TARGET_ARCH="i386"
      return 0
   fi

   if [ "${1}" = "awips2-edex-bufr" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-bufr"
      return 0
   fi

   if [ "${1}" = "awips2-edex-common-core" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-common-core"
      return 0
   fi

   if [ "${1}" = "awips2-edex-core" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-core"
      return 0
   fi

   if [ "${1}" = "awips2-edex-cots" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-cots"
      return 0
   fi

   if [ "${1}" = "awips2-edex-dat" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-dat"
      return 0
   fi

   if [ "${1}" = "awips2-edex-dataplugins" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-dataplugins"
      return 0
   fi

   if [ "${1}" = "awips2-edex-gfe" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-gfe"
      return 0
   fi

   if [ "${1}" = "awips2-edex-grib" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-grib"
      return 0
   fi

   if [ "${1}" = "awips2-edex-hydro" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-hydro"
      return 0
   fi

   if [ "${1}" = "awips2-edex-ncep" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-ncep"
      return 0
   fi

   if [ "${1}" = "awips2-edex-ncep-nsharp" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-ncep-nsharp"
      return 0
   fi

   if [ "${1}" = "awips2-edex-npp" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-npp"
      return 0
   fi

   if [ "${1}" = "awips2-edex-radar" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-radar"
      return 0
   fi

   if [ "${1}" = "awips2-edex-satellite" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-satellite"
      return 0
   fi

   if [ "${1}" = "awips2-edex-shapefiles" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-shapefiles"
      return 0
   fi

   if [ "${1}" = "awips2-edex-text" ]; then
      RPM_SPECS_DIR="${RPM_EDEX_PROJECT_DIR}/Installer.edex-component"
      export COMPONENT_NAME="edex-text"
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

   rpmbuild -ba --target=${TARGET_ARCH} \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_uframe_eclipse %(echo ${UFRAME_ECLIPSE})' \
      --define '_awipscm_share %(echo ${AWIPSCM_SHARE})' \
      --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_component_name %(echo ${COMPONENT_NAME})' \
      --buildroot ${AWIPSII_BUILD_ROOT} \
      ${1}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      return 1
   fi
}

# Special Cases:
# Builds all of the CAVE RPMs.
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

# Builds all of the EDEX RPMs.
function buildEDEX()
{
   export EDEX_BUILD_ARCH="x86_64"
   export RPM_TOP_DIR="${AWIPSII_TOP_DIR}"
   cd ${WORKSPACE}/Installer.rpm/awips2.edex/deploy.builder
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
   buildEDEX
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build the 64-bit RPMs."
      exit 1
   fi
   lookupRPM "awips2-python"
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM ${RPM_SPECS_DIR}/component.spec
   if [ $? -ne 0 ]; then
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

      if [ ! "${RPM_SPECS_DIR}" = "_BUILD_CAVE_" ] &&
         [ ! "${RPM_SPECS_DIR}" = "_BUILD_EDEX_" ]; then
         buildRPM ${RPM_SPECS_DIR}/component.spec
         RC=$?
      else
         if [ "${RPM_SPECS_DIR}" = "_BUILD_CAVE_" ]; then
            buildCAVE
            RC=$?
         fi

         if [ "${RPM_SPECS_DIR}" = "_BUILD_EDEX_" ]; then
            buildEDEX
            RC=$?
         fi
      fi
      if [ ${RC} -ne 0 ]; then
         echo "ERROR: Failed to build - ${arg}."
         exit 1
      fi
   done
fi
