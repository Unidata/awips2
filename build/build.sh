#!/bin/bash -v
set -xe

# Determine where we are
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)
source ${dir}/buildEnvironment.sh

export _script_dir=${dir}
echo "Running build.sh from ${_script_dir}"
echo "   JENKINS_WORKSPACE = ${JENKINS_WORKSPACE}"
cd ${dir}
logdir=${dir}/logs

if [ ! -d $logdir ]; then
  mkdir -p $logdir
fi

START_TIME=`date "+%s"`
timestamp=`date +%Y_%m_%d_%H:%M:%S`

# Cleanup before building CAVE rpms
if  [[ ${2} = "buildCAVE" ]]; then
  rm -rf ${JENKINS_HOME}/buildspace/workspace/AWIPS2-UPC_build/baseline/
  rm -rf ${JENKINS_HOME}/buildspace/workspace/tmp/${USER}/
fi

echo "BUILD_DIR = $BUILD_DIR"
echo "BUILD_WORKSPACE = $BUILD_WORKSPACE"
echo "BASELINE = $BASELINE"
echo "WORKSPACE = $WORKSPACE"
echo "AWIPSII_VERSION = $AWIPSII_VERSION"
echo "AWIPSII_RELEASE = $AWIPSII_RELEASE"
echo "AWIPSII_TOP_DIR = $AWIPSII_TOP_DIR"
echo "UFRAME_ECLIPSE = $UFRAME_ECLIPSE"
echo "AWIPSII_STATIC_FILES = $AWIPSII_STATIC_FILES"
echo "AWIPSII_BUILD_ROOT = $AWIPSII_BUILD_ROOT"

# Prepare the rpm build directory structure
mkdir -p ${AWIPSII_TOP_DIR}/{BUILD,RPMS,SOURCES,SPECS,SRPMS}

pushd . > /dev/null 2>&1
cd ${BASELINE}
mkdir -p ${WORKSPACE}
RSYNC_DIRS=`cat $dir/rsync.dirs`
rsync -ruql --delete --exclude-from=${dir}/excludes ${RSYNC_DIRS} ${WORKSPACE}

popd > /dev/null 2>&1

# execute the build for the appropriate architecture
_rpms_build_directory=${WORKSPACE}/rpms/build
_architecture=`uname -i`
_build_sh_directory=${_rpms_build_directory}/${_architecture}
pushd . > /dev/null 2>&1

cd ${_build_sh_directory}
cp -v ${dir}/buildEnvironment.sh .

# check rpms/build/x86_64/build.sh for build groups

build_log=${logdir}/build${1}-${timestamp}.log
if [ "${1}" = "-b" -a -n "${2}" ]; then
   build_log=${logdir}/build-${2}-${timestamp}.log
fi

/bin/bash ${_build_sh_directory}/build.sh ${1} ${2} > ${build_log}

popd > /dev/null 2>&1

export rpm_end_dir="${AWIPSII_VERSION}-${AWIPSII_RELEASE}"

if [ "$(ls -A ${AWIPSII_TOP_DIR}/RPMS/x86_64/)" ]; then
   mv ${AWIPSII_TOP_DIR}/RPMS/x86_64/* ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/
fi
if [ "$(ls -A ${AWIPSII_TOP_DIR}/RPMS/noarch/)" ]; then
   mv ${AWIPSII_TOP_DIR}/RPMS/noarch/* ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/noarch/
fi

END_TIME=`date "+%s"`
TIME_SPENT=$((END_TIME - START_TIME))
TTI_HOURS=$((TIME_SPENT/3600))
TTI_SECS=$((TIME_SPENT %3600))      #Remaining seconds
TTI_MINS=$((TTI_SECS/60))
TTI_SECS=$((TTI_SECS%60))
echo "Total-time-Spent-In-The-Build-For $0 = $TTI_HOURS hours, $TTI_MINS minutes, $TTI_SECS seconds"

exit
