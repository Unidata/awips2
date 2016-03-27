#!/bin/bash -v
set -xe

export JENKINS_WORKSPACE=/awips2/repo/awips2-builds

# determine where we are ...
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

export _script_dir=${dir}
echo "Running build.sh from ${_script_dir}"
echo "   JENKINS_WORKSPACE = ${JENKINS_WORKSPACE}"
#/bin/bash ${_script_dir}/build2.sh
#echo "Entering the build2.sh script"


START_TIME=`date "+%s"`
timestamp=`date +%Y_%m_%d_%H:%M:%S`

# set all awips2 components into the path.
#export PATH=/awips2/java/bin:/awips2/python/bin:/awips2/ant/bin:/usr/local/heroku/bin:/home/mjames/util:/awips2/eclipse:/usr/lib64/qt-3.3/bin:/bin:/usr/local/bin:/bin:/usr/bin:/usr/ccs/bin
#export LD_LIBRARY_PATH=/awips2/java/lib:/awips2/python/lib:${LD_LIBRARY_PATH}
export JAVA_HOME=/awips2/java
export ANT_HOME=/awips2/ant

# determine where we are ...
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)
cd ${dir}
logdir=${dir}/../logs

# prepare the build environment (since we are manually executed)
source ${dir}/buildEnvironment.sh

# prepare the rpm build directory structure
mkdir -p ${AWIPSII_TOP_DIR}/BUILD
mkdir -p ${AWIPSII_TOP_DIR}/RPMS
mkdir -p ${AWIPSII_TOP_DIR}/SOURCES
mkdir -p ${AWIPSII_TOP_DIR}/SPECS
mkdir -p ${AWIPSII_TOP_DIR}/SRPMS

echo BASELINE is ${BASELINE}
echo WORKSPACE is ${WORKSPACE}

# prepare the baseline
pushd . > /dev/null 2>&1
cd ${BASELINE}
RSYNC_DIRS=`cat $dir/rsync.dirs`

rsync -ruql --delete --exclude-from=${dir}/excludes ${RSYNC_DIRS} ${WORKSPACE}

#cp ${dir}/p2-build.xml.local ${WORKSPACE}/build/p2-build.xml
#cp ${dir}/edex_build.xml ${WORKSPACE}/build.edex/build.xml

#mkdir -p ${WORKSPACE}/build.edex/esb/logs
#mkdir -p ${WORKSPACE}/build.edex/esb/data/manual
#mkdir -p ${WORKSPACE}/build.edex/esb/data/uEngine
popd > /dev/null 2>&1


# execute the build for the appropriate architecture
_rpms_build_directory=${WORKSPACE}/rpms/build
_architecture=`uname -i`
_build_sh_directory=${_rpms_build_directory}/${_architecture}
pushd . > /dev/null 2>&1

cd ${_build_sh_directory}
cp -v ${dir}/buildEnvironment.sh .


#Usage: build.sh OPTION [-nobinlightning]
#   -delta    perform a build of only the rpms that are likely to change.
#   -full     perform a full build of all the rpms.
#   -ade      build all rpms that are packaged in the ade.
#   -viz      only build the Viz rpms (CAVE & AlertViz).
#   -edex     only build the EDEX rpms.
#   -qpid     build only the QPID rpms.
#   -ldm      build the awips2-ldm rpm; requires root privileges.
#   -package  create a yum repository tar file with the rpms that were just built.
#   --help    display this message and exit.
#
# check rpms/build/x86_64/build.sh for these groups
#
/bin/bash build.sh ${1} > ${dir}/build${1}-${timestamp}.log
#/bin/bash build.sh -edex > ${dir}/build-EDEX-${timestamp}.log
#/bin/bash build.sh -local > ${dir}/build-LOCAL-${timestamp}.log
#/bin/bash build.sh -viz > ${dir}/build-VIZ-${timestamp}.log
#/bin/bash build.sh -ldm > ${dir}/build-LDM-${timestamp}.log
#/bin/bash build.sh -httpd > ${dir}/build-LDM-${timestamp}.log
#/bin/bash build.sh -postgres > ${dir}/build-${timestamp}.log
#/bin/bash build.sh -qpid > ${dir}/build-QPID-${timestamp}.log
#/bin/bash build.sh -python-qpid > ${dir}/build-${timestamp}.log
#/bin/bash build.sh -python > ${dir}/build-${timestamp}.log
#/bin/bash build.sh -psql > ${dir}/build-${timestamp}.log
#/bin/bash build.sh -java > ${dir}/build-${timestamp}.log
# remain untried / redundant
#/bin/bash build.sh -notification > ${dir}/build-notification-${timestamp}.log
#/bin/bash build.sh -ade > ${dir}/build-ade-${timestamp}.log
#/bin/bash build.sh -64bit > ${dir}/build-${timestamp}.log
#/bin/bash build.sh -ncep-database > ${dir}/build-${timestamp}.log
#/bin/bash build.sh -delta > ${dir}/build-${timestamp}.log
#/bin/bash build.sh -rh6 > ${dir}/build-rh6-${timestamp}.log
#/bin/bash build.sh -custom >> ${dir}/build-CUSTOM-${timestamp}.log
popd > /dev/null 2>&1

export rpm_end_dir="${AWIPSII_VERSION}-${AWIPSII_RELEASE}"

#mkdir -p /awips2/jenkins/build/rpms/${rpm_end_dir}


if [ "$(ls -A ${AWIPSII_TOP_DIR}/RPMS/x86_64/)" ]; then
   mv ${AWIPSII_TOP_DIR}/RPMS/x86_64/* /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/
fi
if [ "$(ls -A ${AWIPSII_TOP_DIR}/RPMS/noarch/)" ]; then
   mv ${AWIPSII_TOP_DIR}/RPMS/noarch/* /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/noarch/
fi

END_TIME=`date "+%s"`
TIME_SPENT=$((END_TIME - START_TIME))
TTI_HOURS=$((TIME_SPENT/3600))
TTI_SECS=$((TIME_SPENT %3600))      #Remaining seconds
TTI_MINS=$((TTI_SECS/60))
TTI_SECS=$((TTI_SECS%60))
echo "Total-time-Spent-In-The-Build-For $0 = $TTI_HOURS hours, $TTI_MINS minutes, $TTI_SECS seconds"

exit
