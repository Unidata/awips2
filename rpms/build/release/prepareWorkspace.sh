#!/bin/bash

# Next, we need to prepare our workspace because Hudson does not have 'export' capability.
echo "INFO: Begin - Preparing Workspace"

# Remove all SVN metadata.
find ./ -name .svn -exec rm -rf {} \;

# There are a few projects we need to move around. We want all of the individual cave,
# cots, edexOsgi, nativeLib, and RadarServer projects in our workspace.
PROJECT_LIST=( 'cave' 'cots' 'edexOsgi' 'nativeLib' 'RadarServer' 'ncep' 'localization' )
for project in ${PROJECT_LIST[*]};
do
   # Move the individual projects out of the project directory into the workspace.
   mv ${project}/* .
   # Remove the empty project directory.
   rm -rf ${project}
done

echo "INFO: Finished - Preparing Workspace"
