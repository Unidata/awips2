#!/bin/bash

# configure.sh - this script will configure the openfire instance that has
#                been installed.

# Expected Arguments:
#   NONE

pushd . > /dev/null 2>&1

# run the redhat-postinstall.sh script.
cd /awips2/openfire/bin/extra
/bin/bash redhat-postinstall.sh > /dev/null 2>&1
if [ $? -ne 0 ]; then
   exit 1
fi

# create a link to the openfire script.
cd /awips2/openfire/bin
if [ -L openfire.sh ]; then
   rm -f openfire.sh
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
ln -s openfire openfire.sh
if [ $? -ne 0 ]; then
   exit 1
fi
exit 0

popd > /dev/null 2>&1
