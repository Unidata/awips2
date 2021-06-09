#!/bin/bash
#
# Create a signed application bundle from the garbage that Eclipse exports
#
# Author: mjames@ucar.edu
# Last updated: June 12, 2018
#
workspace="$( cd "$(dirname "$0")" ; pwd -P )"
if [ -z "$1" ]; then
  echo "No directory given"
  exit
fi

template=$workspace/awips2-cave-template
cd $template
rm -rf Cave.app

#
# Copy to template directory
#
cp -R ../${1}/Cave.app .
cp ../${1}/MacOS/cave Cave.app/Contents/MacOS/
cp -R ../${1}/Resources Cave.app/Contents/
cp -R ../${1}/jre Cave.app/Contents/Resources/
cp -R ../${1}/configuration Cave.app/Contents/Resources/
cp -R ../${1}/features Cave.app/Contents/Resources/
cp -R ../${1}/plugins Cave.app/Contents/Resources/
rm -rf Cave.app/Contents/MacOS/cave.ini

#
# Custom Info.plist
#
cp $workspace/Info.plist Cave.app/Contents/
cp $workspace/libjep.dylib Cave.app/Contents/Resources/jre/jre/lib/

#
# jspawnhelper must be executable for pydev config within CAVE
#
chmod 755 Cave.app/Contents/Resources/jre/jre/lib/jspawnhelper

#
# codesign the app
#
#codesign --force --sign "Developer ID Application: University Corporation for Atmospheric Research (DQ4ZFL4KLF)" $template/Cave.app
. ${workspace}/cert.sh
codesign --force --sign "${cert}" $template/Cave.app
