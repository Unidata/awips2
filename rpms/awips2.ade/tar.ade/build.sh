#!/bin/bash

set -x

# This script gathers all of the ade components and places them
# into a single directory so that a tar file can be created.

CURRENT_DATE=`date +"%m-%d-%Y"`

if [ -z "${WORKSPACE}" ]; then
  echo "WORKSPACE not set!"
  exit 1
fi

if [ -z "${AWIPSII_VERSION}" ]; then
  echo "AWIPSII_VERSION not set!"
  exit 1
fi

if [ -z "${AWIPSII_RELEASE}" ]; then
  echo "AWIPSII_RELEASE not set!"
  exit 1
fi

BASELINE_JAR_DIR="${RPM_BUILD_ROOT}/tmp"
BASELINE_JAR="awips2-ade-baseline-SOURCES.jar"

mkdir -p /tmp/awips2-ade-${CURRENT_DATE}

cp -v ${BASELINE_JAR_DIR}/${BASELINE_JAR} /tmp/awips2-ade-${CURRENT_DATE}

ADE_RPM_LIST="/home/jenkins/GIT-INTEGRATION/env/AWIPS2-ADE/ade_rpms.txt"
DEST_DIR="/tmp/awips2-ade-${CURRENT_DATE}"

RELEASE_DIR="${WORKSPACE}"/"${AWIPSII_VERSION}"-"${AWIPSII_RELEASE}"

cp "${RELEASE_DIR}"/noarch/awips2-"${AWIPSII_VERSION}"-"${AWIPSII_RELEASE}"".noarch.rpm "$DEST_DIR"/

while IFS= read -r rpm
do
    find "$RELEASE_DIR" -name "$rpm*.rpm" -exec cp {} "$DEST_DIR"/ \;
done < "$ADE_RPM_LIST"

# Copy ufpy and dynamicserialize
# Determine where ufpy and dynamicserialize are located first. They should either
# be in the integration repository or the workspace of RPMBuildNoDeploy-Delivery.
INT_REPO="/var/www/html/repo/awips2-int/i386/python.site-packages"
DEL_WORKSPACE="/usr/share/tomcat5/.hudson/jobs/RPMBuildNoDeploy-Delivery/workspace/var/www/html/repo/awips2-int/i386/python.site-packages"
UFPY_FOUND="N"
DYNAMICSERIALIZE_FOUND="N"

UFPY_RPM="awips2-python-ufpy-${AWIPSII_VERSION}-${AWIPSII_RELEASE}.i386.rpm"
DYNAMICSERIALIZE_RPM="awips2-python-dynamicserialize-${AWIPSII_VERSION}-${AWIPSII_RELEASE}.i386.rpm"

# Check the integration repository first.
if [ -f ${INT_REPO}/${UFPY_RPM} ]; then
   cp -v ${INT_REPO}/${UFPY_RPM} /tmp/awips2-ade-${CURRENT_DATE}
   UFPY_FOUND="Y"
fi

if [ -f ${INT_REPO}/${DYNAMICSERIALIZE_RPM} ]; then
   cp -v ${INT_REPO}/${DYNAMICSERIALIZE_RPM} /tmp/awips2-ade-${CURRENT_DATE}
   DYNAMICSERIALIZE_FOUND="Y"
fi

# Check the workspace next - if the rpm was not found in the repo.
if [ "${UFPY_FOUND}" = "N" ]; then
   if [ ! -f ${DEL_WORKSPACE}/${UFPY_RPM} ]; then
      echo "ERROR: Unable to find - ${UFPY_RPM}."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi
   cp -v ${DEL_WORKSPACE}/${UFPY_RPM} /tmp/awips2-ade-${CURRENT_DATE}
fi

if [ "${DYNAMICSERIALIZE_FOUND}" = "N" ]; then
   if [ ! -f ${DEL_WORKSPACE}/${DYNAMICSERIALIZE_RPM} ]; then
      echo "ERROR: Unable to find - ${DYNAMICSERIALIZE_RPM}."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi
   cp -v ${DEL_WORKSPACE}/${DYNAMICSERIALIZE_RPM} /tmp/awips2-ade-${CURRENT_DATE}
fi

# Copy the installation / removal scripts.
cp -v ${WORKSPACE_DIR}/rpms/awips2.ade/tar.ade/scripts/* \
   /tmp/awips2-ade-${CURRENT_DATE}
chmod a+x /tmp/awips2-ade-${CURRENT_DATE}/*.sh

# Update the installation script.
perl -p -i -e "s/PY_DYNAMICSERIALIZE=/PY_DYNAMICSERIALIZE=${DYNAMICSERIALIZE_RPM}/g" \
   /tmp/awips2-ade-${CURRENT_DATE}/ade_quick_install.sh
perl -p -i -e "s/PY_UFPY=/PY_UFPY=${UFPY_RPM}/g" \
   /tmp/awips2-ade-${CURRENT_DATE}/ade_quick_install.sh

cd /tmp
tar -cjf awips2-ade-${AWIPSII_VERSION}-${CURRENT_DATE}.tar awips2-ade-${CURRENT_DATE}
# Ensure that the tar was successful
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: Failed to create tar file - awips2-ade-${CURRENT_DATE}.tar."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

exit 0
