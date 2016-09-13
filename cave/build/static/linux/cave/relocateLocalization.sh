#!/bin/bash

# Post-installation script that copies the unpacked localization files
# to /awips2/cave/etc.

pushd . > /dev/null 2>&1
cd /awips2/cave/plugins
for localizationDirectory in `find . -maxdepth 2 -name localization -type d`;
do
   # copy the contents of the localization directory to the
   # etc directory.
   cp -rf ${localizationDirectory}/* /awips2/cave/etc
   if [ $? -ne 0 ]; then
      exit 1
   fi
   
   # remove the localization directory.
   rm -rf ${localizationDirectory}
   if [ $? -ne 0 ]; then
      exit 1
   fi
done
popd > /dev/null 2>&1