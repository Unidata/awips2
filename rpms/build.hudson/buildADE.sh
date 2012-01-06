#!/bin/bash

# We need to copy the following RPMs into the ADE:
#   1) awips2-java
#   2) awips2-python
#   3) awips2-eclipse
#   4) awips2-ant
#   5) awips2-python-cherrypy
#   6) awips2-python-h5py
#   7) awips2-python-matplotlib
#   8) awips2-python-nose
#   9) awips2-python-numpy
#   10) awips2-python-pil
#   11) awips2-python-pmw
#   12) awips2-python-pupynere
#   13) awips2-python-qpid
#   14) awips2-python-scientific
#   15) awips2-python-scipy
#   16) awips2-python-tables
#   17) awips2-python-thrift
#   18) awips2-python-tpg
#   19) awips2-python-werkzeug
#   20) awips2-qpid-client-devel
#   21) awips2-qpid-server-devel
#   22) qmf-devel

# Hudson:
# mkdir -p awips/ade
# mkdir -p awips/ade/projects
# mkdir -p awips/ade/projects/awips
# mkdir -p awips/ade/projects/awips/Installer.rpm
# mkdir -p awips/ade/projects/awips/nativeLib
# mkdir -p awips/ade/projects/javadocs
# mkdir -p awips/ade/ade-${AWIPSII_VERSION}-${AWIPSII_RELEASE}.${CURRENT_DATE}
# rsync ... edexOsgi/* cave/* cots/* RadarServer/* ncep/* awips/ade/projects/awips
# rsync ... rpms/* awips/ade/projects/awips/Installer.rpm
# rsync ... nativeLib/* awips/ade/projects/awips/nativeLib

# Generate the javadocs
cd ${WORKSPACE}/Installer.rpm/awips2.ade/jar.baseline/javadoc
ant -Dawips.src.dir=${WORKSPACE}/ade/projects/awips
# Copy the javadocs to the baseline javadocs directory.
cp -r ${WORKSPACE}/Installer.rpm/awips2.ade/jar.baseline/javadoc/build/api/* \
   ${WORKSPACE}/ade/projects/javadocs

# Remove uframe-eclipse from the projects directory
rm -rf ${WORKSPACE}/ade/projects/awips/uframe-eclipse

# Jar the source and javadocs
cp ${WORKSPACE}/Installer.rpm/awips2.ade/jar.baseline/jar/build.xml \
   ${WORKSPACE}/ade
cd ${WORKSPACE}/ade
ant -d

CURRENT_DATE=`date +"%Y%m%d"`
ADE_TAR_STORE="ade-${AWIPSII_VERSION}-${AWIPSII_RELEASE}.${CURRENT_DATE}"
mkdir -p ${WORKSPACE}/ade/${ADE_TAR_STORE}
# Copy the sources jar to the ade zip storage.
cp -v awips2-ade-baseline-SOURCES.jar ${WORKSPACE}/ade/${ADE_TAR_STORE}

RPMS_DIR="${WORKSPACE}/Installer.rpm/rpmbuild/RPMS/i386"
# Collect all of the rpms that will be included in the ADE.
if [ ! "${AWIPSII_RPM_REPOSITORY}" = "" ]; then
   RPMS_DIR="${AWIPSII_RPM_REPOSITORY}"   
fi

function copyRPM()
{
   # ${1} == RPM Name Pattern
   # ${2} == RPM Class
   RPM_NAME_PATTERN="${1}"
   RPM_CLASS="${2}"

   if [ ! "${AWIPSII_RPM_REPOSITORY}" = "" ]; then
      local RPM_REPO_BRANCH_DIR="${AWIPSII_VERSION}-${AWIPSII_RELEASE}"
      RPM_NAME_PATTERN="${RPM_REPO_BRANCH_DIR}/${RPM_CLASS}/${RPM_NAME_PATTERN}"
   fi

   RPM=`ls ${RPMS_DIR}/${RPM_NAME_PATTERN}`
   if [ ! -f "${RPM}" ]; then
      echo "ERROR: Unable to find the '${RPM_NAME_PATTERN}' RPM."
      exit 1
   fi

   cp -v ${RPM} ${WORKSPACE}/ade/${ADE_TAR_STORE}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "ERROR: Unable to copy the '${RPM}' RPM."
      exit 1
   fi
}

RPM_SUFFIX=".i386.rpm"
copyRPM "awips2-java-*${RPM_SUFFIX}" "base"
copyRPM "awips2-python-[0-9]*${RPM_SUFFIX}" "python"
copyRPM "awips2-eclipse-*${RPM_SUFFIX}" "base"
copyRPM "awips2-ant-*${RPM_SUFFIX}" "base"
copyRPM "awips2-python-cherrypy*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-h5py*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-matplotlib*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-nose*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-numpy*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-pil*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-pmw*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-pupynere*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-qpid*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-scientific*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-scipy*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-tables*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-thrift*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-tpg*${RPM_SUFFIX}" "python"
copyRPM "awips2-python-werkzeug*${RPM_SUFFIX}" "python"
copyRPM "awips2-qpid-client-devel*${RPM_SUFFIX}" "base"
copyRPM "awips2-qpid-server-devel*${RPM_SUFFIX}" "base"
copyRPM "qmf-devel*${RPM_SUFFIX}" "base"

# Copy the convenience scripts into the ADE.
ADE_PROJECT_DIR="${WORKSPACE}/Installer.rpm/awips2.ade"
cp -v ${ADE_PROJECT_DIR}/tar.ade/scripts/*.sh ${ADE_TAR_STORE}

cd ${WORKSPACE}/ade
tar -cjf awips2-ade-${AWIPSII_VERSION}-${CURRENT_DATE}.tar ${ADE_TAR_STORE}
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Failed to create the ade tar file - awips2-ade-${AWIPSII_VERSION}-${CURRENT_DATE}.tar."
   exit 1
fi

# Copy the ade into the repository if a repository has been specified.
if [ ! "${AWIPSII_RPM_REPOSITORY}" = "" ]; then
   cp -v awips2-ade-${AWIPSII_VERSION}-${CURRENT_DATE}.tar \
      ${AWIPSII_RPM_REPOSITORY}/${AWIPSII_VERSION}-${AWIPSII_RELEASE}
else
   cp -v awips2-ade-${AWIPSII_VERSION}-${CURRENT_DATE}.tar \
      ${RPMS_DIR}
fi

exit 0
