#!/bin/bash

# Next, we create the directory structure that will be required to build the rpms.
echo "INFO: Begin - Creating RPM Build Directory Structure."
# Refresh the rpmbuild directory structure.
if [ -d ${WORKSPACE}/rpmbuild ]; then
   rm -rf ${WORKSPACE}/rpmbuild
fi

mkdir -p ${WORKSPACE}/rpmbuild/BUILD
mkdir -p ${WORKSPACE}/rpmbuild/RPMS/i386
mkdir -p ${WORKSPACE}/rmpbuild/SOURCES
mkdir -p ${WORKSPACE}/rpmbuild/SPECS
mkdir -p ${WORKSPACE}/rpmbuild/SRPMS

# Create a repository directory structure within our workspace
mkdir -p ${WORKSPACE}/${REPO_ROOT_DIR}
mkdir -p ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}
mkdir -p ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/boost
mkdir -p ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/cave
mkdir -p ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/core
mkdir -p ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/edex
mkdir -p ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/python.site-packages

# Copy the baselined comps.xml file to the repository.
if [ ! -f ${WORKSPACE}/${REPO_ROOT_DIR}/comps.xml ]; then
   rm -fv ${WORKSPACE}/${REPO_ROOT_DIR}/comps.xml
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
fi

BASELINED_COMPS_XML="${WORKSPACE}/Installer.rpm/common/yum/arch.x86/comps.xml"
if [ ! -f ${BASELINED_COMPS_XML} ]; then
   file ${BASELINED_COMPS_XML}
   exit 1
fi
cp -v ${BASELINED_COMPS_XML} ${WORKSPACE}/${REPO_ROOT_DIR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

echo "INFO: Finished - Creating RPM Build Directory Structure."
