#!/bin/bash
set -x
# Check out the AWIPS II baseline and generate the Javadocs.

RPM_BUILD_ROOT="${WORKSPACE_DIR}/../tmp/awips-component"
WORKSPACE_DIR="${WORKSPACE_DIR}"

function checkoutProjects()
{
   CONNECTION="$WORKSPACE_DIR"
   if [ "${CONNECTION}" = "" ]; then
      echo "ERROR: No CONNECTION Was Provided."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi

   cd ${CONNECTION}

   if [ -d ${RPM_BUILD_ROOT}/tmp ]; then
      rm -rf ${RPM_BUILD_ROOT}/tmp
   fi

   mkdir -p ${RPM_BUILD_ROOT}/tmp/projects/awips
   
   mv * ${RPM_BUILD_ROOT}/tmp/projects/awips/
   mv ${RPM_BUILD_ROOT}/tmp/projects/awips/rpms* .
}

checkoutProjects

# Remove binLightning before creating the ADE source jar
#find ${RPM_BUILD_ROOT}/tmp/projects/awips -type d -name "*binlightning*" | while read DIR
#do
#   echo "Removing binLightning directory: '"${DIR}"'"
#   rm -rf "${DIR}"
#done

mkdir -p ${RPM_BUILD_ROOT}/tmp/projects/javadocs

cd ${WORKSPACE_DIR}/rpms/awips2.ade/tar.baseline/javadoc

ant -Dawips.src.dir=${RPM_BUILD_ROOT}/tmp/projects/awips

# copy the javadocs to the baseline javadocs directory
cp -r ${WORKSPACE_DIR}/rpms/awips2.ade/tar.baseline/javadoc/build/api/* \
   ${RPM_BUILD_ROOT}/tmp/projects/javadocs

# remove uframe-eclipse from the projects directory
rm -rf ${RPM_BUILD_ROOT}/tmp/projects/awips/uframe-eclipse

# jar the baseline
cp ${WORKSPACE_DIR}/rpms/awips2.ade/tar.baseline/jar/build.xml \
   ${RPM_BUILD_ROOT}/tmp
cd ${RPM_BUILD_ROOT}/tmp
ant -d
