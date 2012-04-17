#!/bin/bash

# package.sh - this script will unpack (untar) the openfire binaries,
#              apply any required patches to the configuration, and
#              prepare openfire for packaging in the rpm.

# Expected Arguments:
#   ${1} == the baseline workspace
#   ${2} == the 'RPM_BUILD_ROOT'

baseline_workspace="${1}"
RPM_BUILD_ROOT="${2}"

srcdir="${baseline_workspace}/Installer.rpm/awips2.core/Installer.xmpp/dist"
source_tar="${srcdir}/openfire_3_7_1.tar.gz"
patch_file0="${srcdir}/openfire.patch0"

# Prepare the installation directory.
if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi

# Unpack the source.
tar -xf ${source_tar} \
   -C ${RPM_BUILD_ROOT}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi

# Apply the patch.
pushd .
cd ${RPM_BUILD_ROOT}/awips2/openfire
patch -p1 -i ${patch_file0}
if [ $? -ne 0 ]; then
   exit 1
fi
popd

exit 0
