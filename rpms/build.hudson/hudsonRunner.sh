#!/bin/bash

if [ -d awips ]; then
   rm -rf awips/
fi
mkdir -p awips/
mkdir -p awips/Installer.rpm
mkdir -p awips/dist.native
mkdir -p awips/pythonPackages

rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes RadarServer/* cots/* edexOsgi/* cave/* ncep/* awips
rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes rpms/* awips/Installer.rpm
rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes dist.native/* awips/dist.native
rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes pythonPackages/* awips/pythonPackages

export WORKSPACE="${WORKSPACE}/awips"
chmod a+x *.sh
./buildBaseline.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

rm -rf awips/Installer.rpm
cp -r /share1/bkowal/Installer.rpm awips/

./buildRPMs.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

if [ "${BUILD_ADE}" = "true" ]; then
   mkdir -p awips/ade
   mkdir -p awips/ade/projects
   mkdir -p awips/ade/projects/awips
   mkdir -p awips/ade/projects/awips/Installer.rpm
   mkdir -p awips/ade/projects/awips/nativeLib
   mkdir -p awips/ade/projects/javadocs
   rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes RadarServer/* cots/* edexOsgi/* ncep/* awips/ade/projects/awips
   rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes rpms/* awips/ade/projects/awips/Installer.rpm
   rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes nativeLib/* awips/ade/projects/awips/nativeLib

   chmod a+x buildADE.sh
   ./buildADE.sh
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
fi

if [ "${BUILD_BASE}" = "true" ]; then
   chmod a+x ROOT_buildLDM.sh
   COPY_ENV="export AWIPSII_VERSION=${AWIPSII_VERSION};"
   COPY_ENV="${COPY_ENV} export AWIPSII_RELEASE=${AWIPSII_RELEASE};"
   COPY_ENV="${COPY_ENV} export WORKSPACE=${WORKSPACE};"
   COPY_ENV="${COPY_ENV} export AWIPSII_UFRAME_ECLIPSE=${AWIPSII_UFRAME_ECLIPSE};"
   COPY_ENV="${COPY_ENV} export AWIPSII_RPM_REPOSITORY=${AWIPSII_RPM_REPOSITORY};"
   COPY_ENV="${COPY_ENV} export SVN_URL=${SVN_URL};" 
   sudo su -c "${COPY_ENV} ./ROOT_buildLDM.sh;"
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
fi

# The baselined comps.xml file ...
if [ ! "${AWIPSII_RPM_REPOSITORY}" = "" ]; then
   if [ -f ${WORKSPACE}/Installer.rpm/common/repo/comps.xml ]; then
      cp -vf ${WORKSPACE}/Installer.rpm/common/repo/comps.xml \
         ${AWIPSII_RPM_REPOSITORY}
      RC=$?
      if [ ${RC} -ne 0 ]; then
         exit 1
      fi
      chmod 644 ${AWIPSII_RPM_REPOSITORY}/comps.xml
   else
      echo "WARNING: The baselined comps.xml file could not be found."
   fi 
fi

exit 0
