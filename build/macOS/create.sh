#!/bin/bash
#
# Create a signed application bundle from the garbage that Eclipse exports
#
# Author: mjames@ucar.edu
# Author: srcarter@ucar.edu
# 
# Updates
# June 12, 2018 - mjames last updates
# June 15, 2021 - srcarter updates for notarization errors (hardened runtime when signing)
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
cp -R ../${1}/jre/jre Cave.app/Contents/Resources/
cp -R ../${1}/configuration Cave.app/Contents/Resources/
cp -R ../${1}/features Cave.app/Contents/Resources/
cp -R ../${1}/plugins Cave.app/Contents/Resources/
rm -rf Cave.app/Contents/MacOS/cave.ini

#
# Custom Info.plist
#
cp $workspace/Info.plist Cave.app/Contents/
cp $workspace/libjep.dylib Cave.app/Contents/Resources/jre/lib/

. ${workspace}/cert.sh

#
# jspawnhelper must be executable for pydev config within CAVE
#
jsh=$template/Cave.app/Contents/Resources/jre/lib/jspawnhelper
chmod 755 $jsh
# min sdk must be updated
minSDKCmd=$workspace/fixMonoMinVersion
$minSDKCmd $jsh
# must also be signed
codesign --force --options=runtime --sign "${cert}" $jsh

#
# Need to sign specific packages
#
resDir=$template/Cave.app/Contents/Resources
jreDir=$resDir/jre

# jre/bin
for file in ${jreDir}/bin/*;
do
        $minSDKCmd $file
	codesign --force --options=runtime --sign "${cert}" $file
done

# all .dylib files
find $resDir -name "*.dylib" -exec $minSDKCmd {} \; -exec codesign --force --options=runtime --sign "${cert}" "{}" \;

# eclipse .so
eclipseSO=$resDir/plugins/org.eclipse.equinox.launcher.cocoa.macosx.x86_64_1.1.401.v20161122-1740/eclipse_1615.so
codesign --force --options=runtime --sign "${cert}" $eclipseSO

# jython jar -- need to extract a file, sign it, and then update the jar
jythonDir=$resDir/plugins/org.python.pydev.jython_5.8.0.201706061859/
jythonLib=jni/Darwin/libjffi-1.2.jnilib
# extract the file that needs signing
cd ${jythonDir}
jar xvf ${jythonDir}/jython.jar ${jythonLib}
# sign the file
codesign --force --options=runtime --sign "${cert}" $jythonLib
# update the jar with this signed file
jar uf $jythonDir/jython.jar $jythonDir/$jythonLib
# remove the extracted file
rm -rf $jythonDir/jni
cd ${template}

## ----- Done signing included packages -----

#
# codesign the app
#
#codesign --force --sign "Developer ID Application: University Corporation for Atmospheric Research (DQ4ZFL4KLF)" $template/Cave.app
codesign --force --entitlements $workspace/entitlements.xml --options=runtime --sign "${cert}" $template/Cave.app

