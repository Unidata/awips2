#!/bin/bash

echo "Starting ... $0."

SPECS_FILE="component.spec"
RPM_PROJECT_DIR="${WORKSPACE}/all/Installer.rpm"
RPM_INVENTORY="${RPM_PROJECT_DIR}/awips2-rpm.db"
VERSION_FILE="${RPM_PROJECT_DIR}/version.txt"

# Get the current version.
VERSION=`cat ${VERSION_FILE}`

# Determine the nightly release - based on the date.
RELEASE="${BUILD_DATE}"

function replaceVersionAndRelease()
{
   # $1 == Directory with the specs file.
   local SPECS_DIRECTORY=${1}

   echo "INFO: Updating Specs File In - ${SPECS_DIRECTORY}."

   # Update The Specs File.
   perl -p -i -e "s/Version: 1.0.0/Version: ${VERSION}/g" \
      ${SPECS_DIRECTORY}/${SPECS_FILE}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
   perl -p -i -e "s/Release: 1/Release: ${RELEASE}/g" \
      ${SPECS_DIRECTORY}/${SPECS_FILE}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
}

# Query the db file to find the core rpms that need to be updated.
SQL="SELECT buildDirectory FROM awips2_core_rpms WHERE dynamicVersion='Y';"
for directory in `echo ${SQL} | sqlite3 ${RPM_INVENTORY}`; do
   replaceVersionAndRelease ${RPM_PROJECT_DIR}/${directory}
done

# Query the db file to find the python site-package rpms that need to be updated.
SQL="SELECT buildDirectory FROM awips2_python_site_package_rpms WHERE dynamicVersion='Y';"
for directory in `echo ${SQL} | sqlite3 ${RPM_INVENTORY}`; do
   replaceVersionAndRelease ${RPM_PROJECT_DIR}/${directory}
done

# Query the db file to find the edex rpms that need to be updated.
SQL="SELECT buildDirectory FROM awips2_edex_rpms;"
for directory in `echo ${SQL} | sqlite3 ${RPM_INVENTORY}`; do
   replaceVersionAndRelease ${RPM_PROJECT_DIR}/${directory}
done

# Query the db file to find the cave rpms that need to be updated.
SQL="SELECT buildDirectory FROM awips2_cave_rpms;"
for directory in `echo ${SQL} | sqlite3 ${RPM_INVENTORY}`; do
   replaceVersionAndRelease ${RPM_PROJECT_DIR}/${directory}
done

echo "Finished ... $0."

exit 0
