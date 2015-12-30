#!/bin/bash

# User-configurable environment parameters that are used during the build.

# The baseline parameter is specific to non-Jenkins / non-Hudson builds
export BUILD_DIR=/awips2/jenkins/buildspace
export BUILD_WORKSPACE=${BUILD_DIR}/workspace

export AWIPSII_VERSION="14.2.1"
export AWIPSII_RELEASE="16n${BUILD_NUMBER}"

export AWIPSII_TOP_DIR="${BUILD_WORKSPACE}/tmp/rpms_built_dir"
#export WORKSPACE="${BUILD_WORKSPACE}/AWIPS2-NCEP_build/baseline"
export WORKSPACE="/awips2/repo/awips2-builds"
export BASELINE="/awips2/repo/awips2-builds"

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
