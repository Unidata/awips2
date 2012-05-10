#!/bin/bash

# TODO - Check Number Of Arguments.
export WORKSPACE=${1}
export SHARE_DIR=${2}
# This is where we will copy the rpms to once they are
# successfully built.
export REPO_DEST=${3}

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
PYPIES_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.core/Installer.httpd-pypies"
COLLABORATION_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.core/Installer.httpd-collaboration"

httpd_version="2.2.3"
HTTPD_PYPIES_pattern="awips2-httpd-pypies-${httpd_version}-*.i386.rpm"
HTTPD_COLLABORATION_pattern="awips2-httpd-collaboration-${httpd_version}-*.i386.rpm"

# Create a temporary repository destination for the httpd rpms.
if [ -d ${REPO_DEST}/httpd-temp ]; then
   rm -rf ${REPO_DEST}/httpd-temp
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir -p ${REPO_DEST}/httpd-temp/BUILD
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${REPO_DEST}/httpd-temp/RPMS
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${REPO_DEST}/httpd-temp/SOURCES
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${REPO_DEST}/httpd-temp/SPECS
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${REPO_DEST}/httpd-temp/SRPMS
if [ $? -ne 0 ]; then
   exit 1
fi

# Unpack the httpd source.
httpd_src="${WORKSPACE}/Installer.rpm/awips2.core/deploy.builder/httpd.SOURCES"
httpd_src_tar="${httpd_src}/httpd-2.2.3-SOURCES.tar"
tar -xvf ${httpd_src_tar} -C ${REPO_DEST}/httpd-temp/SOURCES
if [ $? -ne 0 ]; then
   exit 1
fi

echo "Building ... awips2-httpd-pypies"
# Build awips2-httpd-pypies
rpmbuild -ba --target=i386 \
   --define '_topdir %(echo ${REPO_DEST}/httpd-temp)' \
   --define '_build_root %(echo ${BUILDROOT_DIR})' \
   --define '_baseline_workspace %(echo ${WORKSPACE})' \
   --buildroot ${BUILDROOT_DIR} \
   ${PYPIES_PROJECT_DIR}/component.spec
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Build of awips2-httpd-pypies Failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

echo "Building ... awips2-httpd-collaboration"
# Build awips2-httpd-collaboration
rpmbuild -ba --target=i386 \
   --define '_topdir %(echo ${REPO_DEST}/httpd-temp)' \
   --define '_build_root %(echo ${BUILDROOT_DIR})' \
   --define '_baseline_workspace %(echo ${WORKSPACE})' \
   --buildroot ${BUILDROOT_DIR} \
   ${COLLABORATION_PROJECT_DIR}/component.spec
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Build of awips2-httpd-collaboration Failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# Copy the httpd rpms to the specified destination directory.
if [ ! -d ${REPO_DEST}/RPMS/i386 ]; then
   mkdir -p ${REPO_DEST}/RPMS/i386
fi
cp -v ${REPO_DEST}/httpd-temp/RPMS/i386/${HTTPD_PYPIES_pattern} \
   ${REPO_DEST}/RPMS/i386
if [ $? -ne 0 ]; then
   exit 1
fi
cp -v ${REPO_DEST}/httpd-temp/RPMS/i386/${HTTPD_COLLABORATION_pattern} \
   ${REPO_DEST}/RPMS/i386
if [ $? -ne 0 ]; then
   exit 1
fi

# Remove the temporary repository destination.
rm -rf ${REPO_DEST}/httpd-temp
if [ $? -ne 0 ]; then
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
cp -v ${RPM_DEST_DIR}/${HTTPD_PYPIES_RPM} ${REPO_DEST}/RPMS/i386 
cp -v ${RPM_DEST_DIR}/${LDM_RPM} ${REPO_DEST}/RPMS/i386
cp -v ${RPM_DEST_DIR}/${PYGTK_RPM} ${REPO_DEST}/RPMS/i386
cp -v ${RPM_DEST_DIR}/${PYCAIRO_RPM} ${REPO_DEST}/RPMS/i386
