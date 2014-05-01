#!/bin/bash

# This script will build the CAVE zip, the p2 repository zips, and copy them to the CAVE rpm dist
# directory.

# This script is started by the awips2.cave rpm build.sh script; so, it is able to get the workspace
# directory from the environment as well as the build architecture.

if [ "${UFRAME_ECLIPSE}" = "" ]; then
   export UFRAME_ECLIPSE="/opt/uframe-eclipse"
fi

CAVE_RPM_DIST_DIR="${WORKSPACE}/rpms/awips2.cave/setup/dist"
if [ ! -d ${CAVE_RPM_DIST_DIR} ]; then
   echo "ERROR: ${CAVE_RPM_DIST_DIR} does not exist."
   exit 1
fi
rm -f ${CAVE_RPM_DIST_DIR}/*
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unable to remove the contents of ${CAVE_RPM_DIST_DIR}."
   exit 1
fi

if [ ! -d ${WORKSPACE}/build ]; then
   echo "ERROR: The CAVE build directory was not found in the workspace - ${WORKSPACE}/build."
   echo "       When it was checked out of SVN was it accidentally named build.cave instead?"
   exit 1
fi
cd ${WORKSPACE}/build

# Build the CAVE zip file.
if [ ! -f build.sh ]; then
   echo "ERROR: Unable to find the CAVE build script."
   exit 1
fi

# Execute the CAVE PDE Build.
# The Sun JDK Build.
time /bin/bash build.sh -eclipse=${UFRAME_ECLIPSE}
RC=$?

if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unable to build the CAVE zip file."
   exit 1
fi

# Copy the CAVE zip file to the awips2.cave dist directory.
CAVE_ZIP_NAME_32="CAVE-linux.gtk.x86.zip"
CAVE_ZIP_NAME_64="CAVE-linux.gtk.x86_64.zip"
CAVE_ZIP_LOC="cave/tmp/I.CAVE"
CAVE_ZIP="${CAVE_ZIP_LOC}/${CAVE_ZIP_NAME_32}"
if [ ! -f ${CAVE_ZIP} ]; then
   echo "ERROR: ${CAVE_ZIP} does not exist."
   exit 1
fi
cp -v ${CAVE_ZIP} ${CAVE_RPM_DIST_DIR}
CAVE_ZIP="${CAVE_ZIP_LOC}/${CAVE_ZIP_NAME_64}"
if [ ! -f ${CAVE_ZIP} ]; then
   echo "ERROR: ${CAVE_ZIP} does not exist."
   exit 1
fi
cp -v ${CAVE_ZIP} ${CAVE_RPM_DIST_DIR}

# Build the p2 repo zip files.
if [ ! -f p2-build.xml ]; then
   echo "ERROR: Unable to find the p2 repo ant script."
   exit 1
fi

# Execute the P2 Repo PDE Build.
# The Sun JDK Build.
time /awips2/ant/bin/ant -f p2-build.xml \
   -Declipse.dir=${UFRAME_ECLIPSE} \
   -Dbuild.version=${AWIPSII_VERSION} \
   -Dbuild.arch=${CAVE_BUILD_ARCH} \
   -Declipse.dir=${UFRAME_ECLIPSE}
RC=$?

if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unable to build the CAVE p2 repo zip files."
   exit 1
fi 

# Copy the p2 repo zip files to the awips2.cave dist directory.
P2_REPO_ZIP_LOC="cave/p2/dist"
if [ ! -d ${P2_REPO_ZIP_LOC} ]; then
   echo "ERROR: Unable to find the CAVE p2 repo zip files."
   exit 1
fi
cp -v ${P2_REPO_ZIP_LOC}/* ${CAVE_RPM_DIST_DIR}

# Finished
exit 0
