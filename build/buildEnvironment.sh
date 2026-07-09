#!/bin/bash
# Version
export AWIPSII_VERSION="23.4.3"
export AWIPSII_RELEASE="1"
export AWIPSII_BUILD_DATE=`date`
export AWIPSII_BUILD_SYS=`cat /etc/system-release`
# Author
export AWIPSII_BUILD_VENDOR="UCAR"
export AWIPSII_BUILD_SITE="Unidata"
export AWIPSII_AUTHOR="Tiffany Meyer <tiffanym@ucar.edu>"
# Directories
export UFRAME_ECLIPSE=/awips2/eclipse
export JAVA_HOME=/awips2/java
export ANT_HOME=/awips2/ant
export GRADLE_HOME=/awips2/gradle
export REPO=/awips2/repo
export JENKINS_HOME=/awips2/jenkins
export JENKINS_WORKSPACE=${REPO}/awips2
export BUILD_DIR=${JENKINS_HOME}/buildspace
export AWIPSII_STATIC_FILES=${REPO}/awips2-static
# More env vars
export BUILD_WORKSPACE=${BUILD_DIR}/workspace
export BASELINE=${JENKINS_WORKSPACE}
export AWIPSII_TOP_DIR=${BUILD_WORKSPACE}/tmp/rpms_built_dir
export WORKSPACE=${BUILD_WORKSPACE}/AWIPS2-UPC_build/baseline
export UFRAME_TARGET=${WORKSPACE}/target
#export AWIPSII_BUILD_ROOT=${BUILD_WORKSPACE}/tmp/${USER}/awips-component
export AWIPSII_BUILD_ROOT=${BUILD_WORKSPACE}/tmp/awips-component
#export REPO_DEST=${BUILD_WORKSPACE}/tmp/${USER}/repo
export REPO_DEST=${BUILD_WORKSPACE}/tmp/repo
export PROJ_DIR=/awips2/python
export PROJ_LIBDIR=/awips2/python/share
export PROJ_INCDIR=/awips2/python/include
export LD_LIBRARY_PATH=/awips2/python/lib
export LIBRARY_PATH=/awips2/python/lib
export PATH=$PATH:/awips2/gradle/bin
