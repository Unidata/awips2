#!/bin/bash

echo "Starting ... $0."

if [ -d ${WORKSPACE}/all/rpmbuild ]; then
   rm -rf ${WORKSPACE}/all/rpmbuild
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
fi

mkdir -p ${WORKSPACE}/all/rpmbuild
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${WORKSPACE}/all/rpmbuild/BUILD
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${WORKSPACE}/all/rpmbuild/RPMS/i386
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${WORKSPACE}/all/rpmbuild/SOURCES
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${WORKSPACE}/all/rpmbuild/SPECS
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mkdir -p ${WORKSPACE}/all/rpmbuild/SRPMS
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

echo "Finished ... $0."

exit 0
