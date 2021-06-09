#!/bin/bash -fv
#
# Build script for CAVE macOS DMG
# Requires product.developer be exported from eclipse (export wizard)
#
# Author: mjames@ucar.edu
# Last updated: 06/2018
#

workspace="$( cd "$(dirname "$0")" ; pwd -P )"
VERS=17.1.1-6
DMG_TMP=osx_release/awips2-cave-$VERS-rw.dmg
DMG_REL=osx_release/awips2-cave-$VERS-release.dmg
VOL_NAME='AWIPS CAVE '$VERS

#
# Remove if exists
#
rm -rf $DMG_TMP
rm -rf $DMG_REL

#
# Create read-write dmg of 800M
#
hdiutil create "${DMG_TMP}" -volname "${VOL_NAME}" -srcfolder ${workspace}/awips2-cave-template -format UDRW -fs HFS+ -size 800M

#
# Mount
#
DEVICE=$(hdiutil attach -readwrite -noverify "${DMG_TMP}" | egrep '^/dev/' | sed 1q | awk '{print $1}')
echo $DEVICE

#
# Resize the window, change the icon size, placement, etc.
#
echo '
   tell application "Finder"
     tell disk "'${VOL_NAME}'"
           open
           set current view of container window to icon view
           set toolbar visible of container window to false
           set statusbar visible of container window to false
           set the bounds of container window to {400, 100, 920, 440}
           set viewOptions to the icon view options of container window
           set arrangement of viewOptions to not arranged
           set icon size of viewOptions to 148
           set position of item "Cave" of container window to {160, 100}
           set position of item "Applications" of container window to {360, 100}
           close
           open
           update without registering applications
           delay 2
           close
     end tell
   end tell
' | osascript

sync

#
# Detach/unmount/eject
#
hdiutil detach $DEVICE

#
# Convert read-write image to compressed
#
hdiutil convert "${DMG_TMP}" -format UDZO -imagekey zlib-level=9 -o "${DMG_REL}"

#
# Sign our app
#
. cert.sh
codesign --force --sign "${cert}" ${DMG_REL}
