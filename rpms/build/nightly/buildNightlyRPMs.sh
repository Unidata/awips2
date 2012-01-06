#!/bin/bash

echo "Starting ... $0."

RPM_BUILD_SCRIPT="build.sh"
RPM_PROJECT_DIR="${WORKSPACE}/all/Installer.rpm"
export RPM_TOP_DIR="${WORKSPACE}/all/rpmbuild"
RPM_DEST_DIR="${RPM_TOP_DIR}/RPMS/i386"
export AWIPSII_BUILD_TAG=`perl ${RPM_PROJECT_DIR}/awips2.core/deploy.builder/extractTag.pl ${1}`

# Build the Core RPMs.
cd ${RPM_PROJECT_DIR}/awips2.core/deploy.builder
time ./${RPM_BUILD_SCRIPT}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
# Copy the RPMs To The Repo Directory ~ core.
cp -v ${RPM_DEST_DIR}/* ${REPO_SHARE_ROOT}/${BUILD_DATE}/core
cp -v ${RPM_DEST_DIR}/../noarch/* ${REPO_SHARE_ROOT}/${BUILD_DATE}/core
rm -fv ${RPM_DEST_DIR}/*

# Build the Python Site-Package RPMs.
cd ${RPM_PROJECT_DIR}/python.site-packages/deploy.builder
time ./${RPM_BUILD_SCRIPT}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
# Copy the RPMs to the Repo Directory ~ python.site-packages.
cp -v ${RPM_DEST_DIR}/* ${REPO_SHARE_ROOT}/${BUILD_DATE}/python.site-packages
rm -fv ${RPM_DEST_DIR}/*

export AWIPSCM_SHARE="/share1"
export WORKSPACE_DIR="${WORKSPACE}"
# Build the Edex RPMs.
cd ${RPM_PROJECT_DIR}/awips2.edex/deploy.builder
time ./${RPM_BUILD_SCRIPT}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
# Copy the RPMs to the Repo Directory ~ edex.
cp -v ${RPM_DEST_DIR}/* ${REPO_SHARE_ROOT}/${BUILD_DATE}/edex
rm -fv ${RPM_DEST_DIR}/*

# Build the CAVE RPMs.
cd ${RPM_PROJECT_DIR}/awips2.cave/deploy.builder
time ./${RPM_BUILD_SCRIPT}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
# Copy the RPMs to the Repo Directory ~ cave.
cp -v ${RPM_DEST_DIR}/* ${REPO_SHARE_ROOT}/${BUILD_DATE}/cave
rm -fv ${RPM_DEST_DIR}/*

# Copy the baselined comps.xml to the repository.
if [ -f ${REPO_SHARE_ROOT}/comps.xml ]; then
   rm -fv ${REPO_SHARE_ROOT}/comps.xml
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
fi

if [ ! -f ${RPM_PROJECT_DIR}/common/yum/arch.x86/comps.xml ]; then
   file ${RPM_PROJECT_DIR}/common/comps.xml
   exit 1
fi
cp -v ${RPM_PROJECT_DIR}/common/comps.xml ${REPO_SHARE_ROOT}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

echo "Finished ... $0."

exit 0
