#!/bin/bash

if [ "${USER}" != "root" ]; then
   echo "ERROR: This script must be ran as root."
   exit 1
fi

source awips/Installer.rpm/awips2.base/deploy.builder/buildSteps.sh
source awips/Installer.rpm/common/functions/rpmBuild.sh
prepareEnvironment

cd awips/Installer.rpm/awips2.base
buildRPM "Installer.ldm/component.spec"
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Failed to build the awips2-ldm rpm."
   exit 1
fi

if [ "${AWIPSII_RPM_REPOSITORY}" = "" ]; then
   exit 0
fi

# Copy the ldm rpm to the repository
LDM_RPM_PATTERN="awips2-ldm-*.i386.rpm"
LDM_RPM=`ls -1 ${AWIPSII_TOP_DIR}/RPMS/i386/${LDM_RPM_PATTERN}`
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unable to find the awips2 ldm rpm in ${AWIPSII_TOP_DIR}/RPMS/i386."
   exit 1
fi

if [ ! -f "${LDM_RPM}" ]; then
   echo "ERROR: The awips2 ldm rpm '${LDM_RPM}' does not exist."
   exit 1
fi

cp -v ${LDM_RPM} \
   ${AWIPSII_RPM_REPOSITORY}/${AWIPSII_VERSION}-${AWIPSII_RELEASE}/base
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: The copy of the awips2 ldm rpm to the repository has failed."
   exit 1
fi

exit 0
