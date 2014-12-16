#!/bin/bash -x

# Verify that the workspace has been specified
if [ -z ${WORKSPACE} ]; then
   echo "Error: the location of the baseline workspace must be specified using the WORKSPACE environment variable."
   exit 1
fi
if [ -z ${AWIPSII_BUILD_ROOT} ]; then
   export AWIPSII_BUILD_ROOT="/tmp/${USER}/awips-component"
   echo "INFO: using default build root - ${AWIPSII_BUILD_ROOT}."
fi

__SPECS=qpid-java.spec
#__SPECS_PATCH0=qpid-java.spec.patch0

# apply the rpm specification patch
#cd ../SPECS
#if [ $? -ne 0 ]; then
#   exit 1
#fi
#patch -p1 -i ${__SPECS_PATCH0}
#if [ $? -ne 0 ]; then
#   exit 1
#fi

pushd . > /dev/null
cd ../
export TOPDIR=`pwd`

# create the rpm directory structure
if [ -d ${TOPDIR}/BUILD ]; then
   rm -rf ${TOPDIR}/BUILD
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir ${TOPDIR}/BUILD
if [ -d ${TOPDIR}/RPMS ]; then
   rm -rf ${TOPDIR}/RPMS
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir ${TOPDIR}/RPMS
if [ -d ${TOPDIR}/SRPMS ]; then
   rm -rf ${TOPDIR}/SRPMS
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir ${TOPDIR}/SRPMS

# build the rpm
rpmbuild -ba \
   --define "_topdir ${TOPDIR}" \
   --define "_baseline_workspace ${WORKSPACE}" \
   SPECS/${__SPECS}
if [ $? -ne 0 ]; then
   exit 1
fi
rpmbuild -ba \
   --define "_topdir ${TOPDIR}" \
   --define "_baseline_workspace ${WORKSPACE}" \
   --define "_build_root ${AWIPSII_BUILD_ROOT}" \
   --buildroot ${AWIPSII_BUILD_ROOT} \
   SPECS/qpid-lib.spec
if [ $? -ne 0 ]; then
   exit 1
fi

popd > /dev/null

exit 0
