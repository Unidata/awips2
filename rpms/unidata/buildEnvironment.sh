#!/bin/bash
export AWIPSII_VERSION="17.1.1"
export AWIPSII_RELEASE="2"

. /etc/profile.d/awips2.sh

# User-configurable environment parameters that are used during the build.

# The baseline parameter is specific to non-Jenkins / non-Hudson builds
export AWIPSII_BUILD_VENDOR="UCAR"
export AWIPSII_BUILD_SITE="Unidata"
export BUILD_DIR=/awips2/jenkins/buildspace/
# Not sure why, but ld isn't finding /awips2/python/lib/libpython2.7.so
# Need to create sym link: 
#    /usr/lib64/libpython2.7.so -> /awips2/python/lib/libpython2.7.so
#export LD_LIBRARY_PATH=/awips2/python/lib:/usr/lib64
export BUILD_WORKSPACE=${BUILD_DIR}/workspace

# baseline is...
export BASELINE="${JENKINS_WORKSPACE}"
export AWIPSII_TOP_DIR="${BUILD_WORKSPACE}/tmp/rpms_built_dir"

# WORKSPACE is to be removed and created, this should be a new directory
export WORKSPACE="${BUILD_WORKSPACE}/AWIPS2-UPC_build/baseline"

export UFRAME_ECLIPSE="/awips2/eclipse"

export AWIPSII_STATIC_FILES="/awips2/repo"
export AWIPSII_STATIC_FILES="/awips2/repo/awips2-static"
export AWIPSII_BUILD_ROOT="${BUILD_WORKSPACE}/tmp/${USER}/awips-component"
export REPO_DEST="${BUILD_WORKSPACE}/tmp/${USER}/repo"

