#!/bin/bash

# Check out the AWIPS II baseline and generate the Javadocs.

function checkoutProjects()
{
   CONNECTION="${SVN_CONNECTION}"
   if [ "${CONNECTION}" = "" ]; then
      echo "ERROR: No SVN URL Was Provided."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi

   SVN_EXE=`which svn`
   
   cd ${RPM_BUILD_ROOT}/tmp

   PRODUCTS_TO_CHECKOUT=( 'edexOsgi' 'cave' 'cots' 'RadarServer' \
      'rpms' 'nativeLib' 'ncep' )
   for product in ${PRODUCTS_TO_CHECKOUT[*]};
   do
      ${SVN_EXE} export -q --force ${CONNECTION}/${product}
   done
}

# checkout the 
# make our projects directory
mkdir -p ${RPM_BUILD_ROOT}/tmp/projects
mkdir -p ${RPM_BUILD_ROOT}/tmp/projects/awips

checkoutProjects
mv ${RPM_BUILD_ROOT}/tmp/rpms \
   ${RPM_BUILD_ROOT}/tmp/projects/awips/Installer.rpm
cp -r ${RPM_BUILD_ROOT}/tmp/edexOsgi/* \
   ${RPM_BUILD_ROOT}/tmp/projects/awips
rm -rf ${RPM_BUILD_ROOT}/tmp/edexOsgi
cp -r ${RPM_BUILD_ROOT}/tmp/cave/* \
   ${RPM_BUILD_ROOT}/tmp/projects/awips
rm -rf ${RPM_BUILD_ROOT}/tmp/cave
cp -r ${RPM_BUILD_ROOT}/tmp/cots/* \
   ${RPM_BUILD_ROOT}/tmp/projects/awips
rm -rf ${RPM_BUILD_ROOT}/tmp/cots
cp -r ${RPM_BUILD_ROOT}/tmp/RadarServer/* \
   ${RPM_BUILD_ROOT}/tmp/projects/awips
rm -rf ${RPM_BUILD_ROOT}/tmp/RadarServer
cp -r ${RPM_BUILD_ROOT}/tmp/ncep/* \
   ${RPM_BUILD_ROOT}/tmp/projects/awips
rm -rf ${RPM_BUILD_ROOT}/tmp/ncep
mv ${RPM_BUILD_ROOT}/tmp/nativeLib \
   ${RPM_BUILD_ROOT}/tmp/projects/awips/nativeLib

# Generate the Javadocs
# create our javadocs directory
mkdir -p ${RPM_BUILD_ROOT}/tmp/projects/javadocs

cd ${WORKSPACE_DIR}/Installer.rpm/awips2.ade/tar.baseline/javadoc

ant -Dawips.src.dir=${RPM_BUILD_ROOT}/tmp/projects/awips
# copy the javadocs to the baseline javadocs directory
cp -r ${WORKSPACE_DIR}/Installer.rpm/awips2.ade/tar.baseline/javadoc/build/api/* \
   ${RPM_BUILD_ROOT}/tmp/projects/javadocs

# remove uframe-eclipse from the projects directory
rm -rf ${RPM_BUILD_ROOT}/tmp/projects/awips/uframe-eclipse

# jar the baseline
cp ${WORKSPACE_DIR}/Installer.rpm/awips2.ade/tar.baseline/jar/build.xml \
   ${RPM_BUILD_ROOT}/tmp
cd ${RPM_BUILD_ROOT}/tmp
ant -d
