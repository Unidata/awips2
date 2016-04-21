#!/bin/bash

# package.sh - this script will unpack (untar) the openfire binaries,
#              apply any required patches to the configuration, and
#              prepare openfire for packaging in the rpm.

# Expected Arguments:
#   ${1} == the baseline workspace
#   ${2} == the 'RPM_BUILD_ROOT'

baseline_workspace="${1}"
RPM_BUILD_ROOT="${2}"

AWIPS2_STATIC=${AWIPSCM_SHARE}/awips2-static

srcdir="${baseline_workspace}/rpms/awips2.core/Installer.xmpp/dist"
source_tar="${AWIPS2_STATIC}/foss/openfire/openfire_3_9_1.tar.gz"
patch_file0="${srcdir}/openfire.patch0"
patch_file1="${srcdir}/openfire.patch1"
plugin_dir="${srcdir}/plugins"

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

patch -p1 -i ${patch_file1}
if [ $? -ne 0 ]; then
   exit 1
fi
popd

# Build and include any openfire plugins.
cd ${baseline_workspace}/build.openfire.plugin
if [ $? -ne 0 ]; then
   exit 1
fi
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=${RPM_BUILD_ROOT}/awips2/openfire/plugins
if [ $? -ne 0 ]; then
   exit 1
fi
# Include the non-Raytheon openfire plugins
cp -v ${plugin_dir}/*.jar \
   ${RPM_BUILD_ROOT}/awips2/openfire/plugins
if [ $? -ne 0 ]; then
   exit 1
fi

exit 0
