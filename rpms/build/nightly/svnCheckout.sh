#!/bin/bash

echo "Starting ... $0."

SVN_EXE=`which svn`
SVN_URL="file:///home/svnsync/mirror/products/awips/trunk2"
INDIVIDUAL_PROJECT_CHECKOUTS=('cave' 'cots' 'edexOsgi' 'nativeLib' 'RadarServer' 'ncep')
ENTIRE_PROJECT_CHECKOUTS=('rpms' 'pythonPackages')

# Create the workspace directory.
if [ -d all ]; then
   rm -rf all/
fi
mkdir -p all

cd all/

for project in ${INDIVIDUAL_PROJECT_CHECKOUTS[*]}; do
   for i in `${SVN_EXE} list ${SVN_URL}/${project}`; do
      if [ "${i}" != ".project" ]; then
         ${SVN_EXE} export -q --force ${SVN_URL}/${project}/${i}
         RC=$?
         if [ ${RC} -ne 0 ]; then
            exit 1
         fi
      fi
   done
done

for project in ${ENTIRE_PROJECT_CHECKOUTS[*]}; do
   ${SVN_EXE} export -q --force ${SVN_URL}/${project}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
done

if [ -d rpms ]; then
   mv rpms Installer.rpm
fi
echo "Finished ... $0."

exit 0
