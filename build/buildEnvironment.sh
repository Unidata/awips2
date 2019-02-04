#!/bin/bash
# Version
export AWIPSII_VERSION="18.1.1"
export AWIPSII_RELEASE="5"
# Author
export AWIPSII_BUILD_VENDOR="UCAR"
export AWIPSII_BUILD_SITE="Unidata"
export AWIPSII_AUTHOR="Michael James <mjames@ucar.edu>"
# Directories
export UFRAME_ECLIPSE=/awips2/eclipse
export JAVA_HOME=/awips2/java
export ANT_HOME=/awips2/ant
export REPO=/awips2/repo
export JENKINS_HOME=/awips2/jenkins
export JENKINS_WORKSPACE=${REPO}/awips2-builds
export BUILD_DIR=${JENKINS_HOME}/buildspace/
export AWIPSII_STATIC_FILES=${REPO}/awips2-static
# More env vars
export BUILD_WORKSPACE=${BUILD_DIR}/workspace
export BASELINE=${JENKINS_WORKSPACE}
export AWIPSII_TOP_DIR=${BUILD_WORKSPACE}/tmp/rpms_built_dir
export WORKSPACE=${BUILD_WORKSPACE}/AWIPS2-UPC_build/baseline
export AWIPSII_BUILD_ROOT=${BUILD_WORKSPACE}/tmp/${USER}/awips-component
export REPO_DEST=${BUILD_WORKSPACE}/tmp/${USER}/repo
