#!/bin/bash

# User-configurable environment parameters that are used during the build.

# The baseline parameter is specific to non-Jenkins / non-Hudson builds
export BUILD_DIR=/awips2/jenkins/buildspace/
# Not sure why, but ld isn't finding /awips2/python/lib/libpython2.7.so
# Need to create sym link: 
#    /usr/lib64/libpython2.7.so -> /awips2/python/lib/libpython2.7.so
export LD_LIBRARY_PATH=/awips2/python/lib:/usr/lib64
export BUILD_WORKSPACE=${BUILD_DIR}/workspace

# baseline is...
export BASELINE="${JENKINS_WORKSPACE}"
export AWIPSII_VERSION="14.4.1"
export BUILD_NUMBER="9"
export AWIPSII_RELEASE="10n${BUILD_NUMBER}"

export AWIPSII_TOP_DIR="${BUILD_WORKSPACE}/tmp/rpms_built_dir"

# WORKSPACE is to be removed and created, this should be a new directory
export WORKSPACE="${BUILD_WORKSPACE}/AWIPS2-UPC_build/baseline"

export UFRAME_ECLIPSE="/awips2/eclipse"

export AWIPSCM_SHARE="${BUILD_WORKSPACE}/awipscm"
export AWIPSII_BUILD_ROOT="${BUILD_WORKSPACE}/tmp/${USER}/awips-component"

echo "BUILD_DIR = $BUILD_DIR"
echo "BUILD_WORKSPACE = $BUILD_WORKSPACE"
echo "BASELINE = $BASELINE"
echo "AWIPSII_VERSION = $AWIPSII_VERSION"
echo "AWIPSII_RELEASE = $AWIPSII_RELEASE"
echo "AWIPSII_TOP_DIR = $AWIPSII_TOP_DIR"
echo "WORKSPACE = $WORKSPACE"
echo "UFRAME_ECLIPSE = $UFRAME_ECLIPSE"
echo "AWIPSCM_SHARE = $AWIPSCM_SHARE"
echo "AWIPSII_BUILD_ROOT = $AWIPSII_BUILD_ROOT"

