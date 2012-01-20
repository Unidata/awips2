#!/bin/bash

echo "Starting ... $0."

ECLIPSE_HOME="/opt/uframe-eclipse"

function buildProject()
{
   # $1 == BUILD DIRECTORY

   BUILD_DIRECTORY=${1}

   if [ ! -d ${WORKSPACE}/all/${BUILD_DIRECTORY} ]; then
      echo "Unable To Find ... ${BUILD_DIRECTORY}"
      return 1
   fi
   cd ${WORKSPACE}/all/${BUILD_DIRECTORY}
   time ./build.sh -eclipse=${ECLIPSE_HOME}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      echo "Failed To Build Project(s) In ... ${BUILD_DIRECTORY}."
      return 1
   fi
}

PROJECTS_TO_BUILD=('build.edex' 'build.rcm' 'build')

for project in ${PROJECTS_TO_BUILD[*]}; do
   buildProject ${project}
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
done

echo "Finished ... $0."

exit 0
