#!/bin/bash

# Next, we need to build edex, cave, and alertviz.

# TODO: Consider building only the projects that are required (ex: if the user specifies awips2-edex, there is no reason to build cave, rcm, ...)
echo "INFO: Begin - Building edex, cave, rcm, and alertviz."

function buildProject()
{
   # Arguments:
   #   ${1} == BUILD DIRECTORY

   BUILD_DIRECTORY=${1}

   cd ${BUILD_DIRECTORY}
   echo "INFO: Building ... ${BUILD_DIRECTORY}"
   time ./build.sh -eclipse=${ECLIPSE_HOME}
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "ERROR: Failed To Build ... '${BUILD_DIRECTORY}."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi

   # Exit the build directory
   cd ${WORKSPACE}
}

PROJECTS_TO_BUILD=( 'build.edex' 'build.rcm' 'build' )
for project in ${PROJECTS_TO_BUILD[*]};
do
   buildProject ${project}
done

echo "INFO: Finished - Building edex, cave, rcm, and alertviz."
