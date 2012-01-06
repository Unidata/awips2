#!/bin/bash

# TODO - Check Number Of Arguments.
WORKSPACE=${1}
SHARE_DIR=${2}
# This is where we will copy the rpms to once they are
# successfully built.
REPO_DEST=${3}

# Determine if we are 32-bit?
CHECK_ARCH=`uname -i`

if [ ! "${CHECK_ARCH}" = "i386" ]; then
   echo "INFO: The 32-Bit Build Will Not Be Executed."
   exit 0
fi

# Verify that we are being ran as root.
if [ ! "${USER}" = "root" ]; then
   echo "ERROR: This Script Must Be Ran As 'root'."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# Since we need to build as root.
RPM_DEST_DIR="/usr/src/redhat/RPMS/i386"

echo "INFO: Starting The 32-Bit Build."
# Set script variables
export BUILDROOT_DIR=/tmp/awips-component

# Build awips2-httpd-pypies
PYPIES_PROJECT_DIR="Installer.rpm/awips2.core/Installer.httpd-pypies"
HTTPD_PYPIES_RPM="awips2-httpd-pypies-2.2.3-*.i386.rpm"
# We Need The Source RPM.
PYPIES_SRC_RPM="${SHARE_DIR}/packages/httpd-pypies/src/awips2-httpd-pypies-2.2.3-22.src.rpm"

# Ensure That The Source RPM Exists.
if [ ! -f ${PYPIES_SRC_RPM} ]; then
   echo "ERROR: Unable to locate the httpd-pypies source rpm."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
# If the source rpm does exist install it.
rpm -ivh ${PYPIES_SRC_RPM}
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Installation of the httpd-pypies src rpm Failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# Prepare to build the rpm.
export PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.core/Installer.httpd-pypies"
# Build awips2-httpd-pypies
rpmbuild -ba --target=i386 --buildroot ${BUILDROOT_DIR} ${PROJECT_DIR}/component.spec
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Build of awips2-httpd-pypies Failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# ----------------------------------------------------------------------------------------------- #

# Prepare to build the rpm.
LDM_PROJECT_DIR="Installer.rpm/awips2.core/Installer.ldm"
LDM_RPM="awips2-ldm-6.8.1-*.i386.rpm"

export WORKSPACE_DIR=${WORKSPACE}
LDM_PROJECT_DIR="${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.ldm"
# Build awips2-ldm
rpmbuild -ba --target=i386 --buildroot ${BUILDROOT_DIR} ${LDM_PROJECT_DIR}/component.spec
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Build of awips2-ldm Failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# ----------------------------------------------------------------------------------------------- #

# Prepare to build the rpm. awips2-python-pygtk.i386
PYGTK_PROJECT_DIR="${WORKSPACE_DIR}/Installer.rpm/python.site-packages/Installer.pygtk"
PYGTK_RPM="awips2-python-pygtk-2.8.6-1.i386.rpm"

rpmbuild -ba --target=i386 \
   --buildroot ${BUILDROOT_DIR} \
   --define '_baseline_workspace %(echo ${WORKSPACE_DIR})' \
   --define '_build_root %(echo ${BUILDROOT_DIR})' \
   ${PYGTK_PROJECT_DIR}/component.spec
if [ $? -ne 0 ]; then
   echo "ERROR: Build of awips2-python-pygtk Failed."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

# ----------------------------------------------------------------------------------------------- #

# Prepare to build the rpm. awips2-python-pycairo.i386
PYCAIRO_PROJECT_DIR="${WORKSPACE_DIR}/Installer.rpm/python.site-packages/Installer.pycairo"
PYCAIRO_RPM="awips2-python-pycairo-1.2.2-1.i386.rpm"

rpmbuild -ba --target=i386 \
   --buildroot ${BUILDROOT_DIR} \
   --define '_baseline_workspace %(echo ${WORKSPACE_DIR})' \
   --define '_build_root %(echo ${BUILDROOT_DIR})' \
   ${PYCAIRO_PROJECT_DIR}/component.spec
if [ $? -ne 0 ]; then
   echo "ERROR: Build of awips2-python-pycairo Failed."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

# ----------------------------------------------------------------------------------------------- #

# Copy the rpms that we just built to the specified destination.
cp -v ${RPM_DEST_DIR}/${HTTPD_PYPIES_RPM} ${REPO_DEST} 
cp -v ${RPM_DEST_DIR}/${LDM_RPM} ${REPO_DEST}
cp -v ${RPM_DEST_DIR}/${PYGTK_RPM} ${REPO_DEST}
cp -v ${RPM_DEST_DIR}/${PYCAIRO_RPM} ${REPO_DEST}
