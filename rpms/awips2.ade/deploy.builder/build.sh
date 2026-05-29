#!/bin/bash

set -x

# This script executes all of the scripts that are responsible
# for building and packaging the ade components.

export WORKSPACE_DIR=`cd ../../../; pwd;`
#export BUILDROOT_DIR="/tmp/awips-component"
export BUILDROOT_DIR="${WORKSPACE_DIR}/../tmp/awips-component"
export RPM_BUILD_ROOT="${BUILDROOT_DIR}"
export UFRAME_ECLIPSE_DIR="/opt/uframe-eclipse"
export AWIPSCM_SHARE="/awipscm"

if [ -d /tmp/awips-component ]; then
   rm -rf /tmp/awips-component
fi
if [ -d /tmp/ade-component ]; then
   rm -rf /tmp/ade-component
fi
if [ -f /usr/src/redhat/BUILD/component-files.txt ]; then
   rm -f /usr/src/redhat/BUILD/component-files.txt
fi

echo "WORKSPACE_DIR is $WORKSPACE_DIR"
echo "BUILDROOT_DIR is $BUILDROOT_DIR"
echo "RPM_BUILD_ROOT is $RPM_BUILD_ROOT"

# Build the baseline jar file
cd ${WORKSPACE_DIR}/rpms/awips2.ade/tar.baseline
./build.sh "${WORKSPACE_DIR}"

# Create the ade tar file
#cd ${WORKSPACE_DIR}/rpms/awips2.ade/tar.ade
#./build.sh "${AWIPSII_VERSION}" "${AWIPSII_RELEASE}"
