#!/bin/bash

# This script executes all of the scripts that are responsible
# for building and packaging the ade components.

export BUILDROOT_DIR="/tmp/awips-component"
export RPM_BUILD_ROOT="${BUILDROOT_DIR}"
export UFRAME_ECLIPSE_DIR="/awips2/eclipse"
export WORKSPACE_DIR=`cd ../../../; pwd;`
export AWIPSCM_SHARE="/awips2/jenkins/buildspace/workspace/awipscm"

if [ -d /tmp/awips-component ]; then
   rm -rf /tmp/awips-component
fi
if [ -d /tmp/ade-component ]; then
   rm -rf /tmp/ade-component
fi
if [ -f /usr/src/redhat/BUILD/component-files.txt ]; then
   rm -f /usr/src/redhat/BUILD/component-files.txt
fi
# Build the eclipse rpm
cd ${WORKSPACE_DIR}/Installer.rpm/awips2.ade/Installer.eclipse
rpmbuild -ba --target=i386 --buildroot ${BUILDROOT_DIR} component.spec

# Build the baseline jar file
cd ${WORKSPACE_DIR}/Installer.rpm/awips2.ade/tar.baseline
./build.sh

# Create the ade tar file
cd ${WORKSPACE_DIR}/Installer.rpm/awips2.ade/tar.ade
./build.sh "${AWIPSII_VERSION}" "${AWIPSII_RELEASE}"
