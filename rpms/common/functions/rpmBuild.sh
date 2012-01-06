#!/bin/bash

# Environment
# public
function prepareEnvironment()
{
   if [ "${WORKSPACE}" = "" ]; then
      determineWorkspaceLocation
   fi

   if [ "${AWIPSII_VERSION}" = "" ]; then
      setVersion
   fi

   if [ "${AWIPSII_RELEASE}" = "" ]; then
      setRelease
   fi

   if [ "${AWIPSII_BASELINE_STATIC}" = "" ]; then
      setBaselineStatic
   fi

   if [ "${AWIPSII_PYTHON_EXECUTABLE}" = "" ]; then
      setPythonExecutable
   fi

   if [ "${AWIPSII_ANT_EXECUTABLE}" = "" ]; then
      setAntExecutable
   fi

   if [ "${AWIPSII_UFRAME_ECLIPSE}" = "" ]; then
      setUframeEclipse
   fi

   setBuildRoot
   setTopDir
}

# private
function setVersion()
{
   export AWIPSII_VERSION=`cat ${WORKSPACE}/Installer.rpm/version.txt`
}

# private
function setRelease()
{
   # The Current Date.
   export AWIPSII_RELEASE=`date +"%Y%m%d"`
}

# private
function setBaselineStatic()
{
   export AWIPSII_BASELINE_STATIC="/share1"
}

# private
function setBuildRoot()
{
   export AWIPSII_BUILD_ROOT="${WORKSPACE}/Installer.rpm/rpmbuild/awips2-component"
}

# private
function setTopDir()
{
   export AWIPSII_TOP_DIR="${WORKSPACE}/Installer.rpm/rpmbuild"
}

# private
function setPythonExecutable()
{
   if [ ! -f /awips2/python/bin/python ]; then
      echo "WARNING: The default AWIPS II Python Executable was not found."
      export AWIPSII_PYTHON_EXECUTABLE="<Unknown>"
   fi
   export AWIPSII_PYTHON_EXECUTABLE="/awips2/python/bin/python"
}

# private
function setAntExecutable()
{
   if [ ! -f /awips2/ant/bin/ant ]; then
      echo "WARNING: The default AWIPS II Ant Executable was not found."
      export AWIPSII_ANT_EXECUTABLE="<Unknown>"
   fi
   export AWIPSII_ANT_EXECUTABLE="/awips2/ant/bin/ant"
}

# private
function setUframeEclipse()
{
   if [ ! -f /opt/uframe-eclipse/eclipse ]; then
      echo "WARNING: The default Uframe-Eclipse was not found."
      export AWIPSII_UFRAME_ECLIPSE="<Unknown>"
   fi
   export AWIPSII_UFRAME_ECLIPSE="/opt/uframe-eclipse"
}

# private
function determineWorkspaceLocation()
{
   local SCRIPT_DIR=`dirname $_`
   export WORKSPACE=`cd ${SCRIPT_DIR}/../../../; pwd;`

   return 0
}

# Baseline Project Build
# private
function pdeBuild()
{
   time ./build.sh -eclipse=${AWIPSII_UFRAME_ECLIPSE}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      return 1
   fi
}

# private
function buildCAVEAndAlertViz()
{
   pushd . > /dev/null
   cd ${WORKSPACE}/build
   pdeBuild
   RC=$?
   popd > /dev/null
   if [ ${RC} -ne 0 ]; then
      return 1
   fi
}

function buildCAVEP2()
{
   pushd . > /dev/null
   cd ${WORKSPACE}/build
   time ant -f p2-build.xml \
      -Dbuild.version=${AWIPSII_VERSION} \
      -Declipse.dir=${AWIPSII_UFRAME_ECLIPSE}
   RC=$?
   popd > /dev/null
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
}

# private
function buildEDEX()
{
   pushd . > /dev/null
   cd ${WORKSPACE}/build.edex
   pdeBuild
   RC=$?
   popd > /dev/null
   if [ ${RC} -ne 0 ]; then
      return 1
   fi
}

# private
function buildRCM()
{
   pushd . > /dev/null
   cd ${WORKSPACE}/build.rcm
   pdeBuild
   RC=$?
   popd > /dev/null
   if [ ${RC} -ne 0 ]; then
      return 1
   fi
}

# public
function buildBaselineProjects()
{
   if [ "${BUILD_CAVE}" = "true" ]; then
      buildCAVEAndAlertViz
      RC=$?
      if [ ${RC} -ne 0 ]; then
         return 1
      fi
      buildCAVEP2
      RC=$?
      if [ ${RC} -ne 0 ]; then
         return 1
      fi
   fi

   if [ "${BUILD_EDEX}" = "true" ]; then
      buildEDEX
      RC=$?
      if [ ${RC} -ne 0 ]; then
         return 1
      fi
   fi

   if [ "${BUILD_CORE}" = "true" ]; then
      buildRCM
      RC=$?
      if [ ${RC} -ne 0 ]; then
         return 1
      fi
   fi

   if [ "${BUILD_CORE}" = "true" ] &&
      [ "${BUILD_CAVE}" = "false" ]; then
      buildCAVEAndAlertViz
      RC=$?
      if [ ${RC} -ne 0 ]; then
         return 1
      fi
   fi   
}

# RPM Build
# public
function execute()
{
   pushd .
   cd ${SCRIPT_DIR}/../
   preBuild
   RC=$?
   if [ ${RC} -ne 0 ]; then
      return 1
   fi

   for directory in `ls -1d Installer.*`;
   do
      buildRPM "${directory}/component.spec"
      RC=$?
      if [ ${RC} -ne 0 ]; then
         return 1
      fi
   done

   postBuild
   popd

   return 0
}

# public
function buildRPM()
{
   # ${1} = the specs file
   export SPECS="${1}"

   preRPMBuild
   RC=$?
   if [ ${RC} -ne 0 ]; then
      return 0
   fi

   if [ ! -f ${SPECS} ]; then
      file ${SPECS}
      return 1
   fi

   if [ -f ${AWIPSII_TOP_DIR}/BUILD/component-files.txt ]; then
      rm -f ${AWIPSII_TOP_DIR}/BUILD/component-files.txt
   fi
   rpmbuild -ba --target=i386 \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_baseline_static %(echo ${AWIPSII_BASELINE_STATIC})' \
      --define '_python_exe %(echo ${AWIPSII_PYTHON_EXECUTABLE})' \
      --define '_ant_exe %(echo ${AWIPSII_ANT_EXECUTABLE})' \
      --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
      --define '_uframe_eclipse %(echo ${AWIPSII_UFRAME_ECLIPSE})' \
      ${SPECS}
   RC=$?

   postRPMBuild ${SPECS}
   if [ ${RC} -ne 0 ]; then
      return 1
   fi

   return 0
}
