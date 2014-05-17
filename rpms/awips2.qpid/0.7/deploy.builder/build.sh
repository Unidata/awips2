#!/bin/bash
SPECS_FILE="qpid-cpp-mrg.spec"

# Verify that the correct version of swig is installed.
REQUIRED_SWIG_VERSION="SWIG Version 3.0"
COMMAND=`which swig`
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: 'swig v3.0.X' Is Required To Successfully Build The QPID RPMs."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

SWIG_VERSION=`swig -version | grep Version`
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unable To Verify The Installed Version Of 'swig'."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

VERIFY_SWIG_VERSION=`echo "${SWIG_VERSION}" | grep "${REQUIRED_SWIG_VERSION}"`
if [ "${VERIFY_SWIG_VERSION}" = "" ]; then
   echo "ERROR: 'swig v3.0.X' Is Required To Successfully Build The QPID RPMs."
   echo "       '${SWIG_VERSION}' Was Detected."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# We are in the awips2.qpid/deploy.builder directory when we are started.
cd ../

export QPID_WORKSPACE_DIR=`pwd`

# Purge the BUILD directory, if necessary.
if [ -d ${QPID_WORKSPACE_DIR}/BUILD ]; then
   rm -rf ${QPID_WORKSPACE_DIR}/BUILD/*
fi
# Purge the RPMS directory, if necessary.
if [ -d ${QPID_WORKSPACE_DIR}/RPMS/i386 ]; then
   rm -rf ${QPID_WORKSPACE_DIR}/RPMS/*
fi

if [ ! -d ${QPID_WORKSPACE_DIR}/BUILD ]; then
   mkdir -p ${QPID_WORKSPACE_DIR}/BUILD
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
if [ ! -d ${QPID_WORKSPACE_DIR}/SRPMS ]; then
   mkdir -p ${QPID_WORKSPACE_DIR}/SRPMS
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

# Conceal The SWIG Requirement In The Specs File.
perl -p -i -e "s/BuildRequires: swig/#BuildRequires: swig/g" \
   SPECS/${SPECS_FILE}

rpmbuild -ba --define '_topdir %(echo ${QPID_WORKSPACE_DIR})' \
   SPECS/${SPECS_FILE}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

exit 0
