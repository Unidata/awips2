#!/bin/bash

SCRIPT_DIR=`dirname $_`
source awips/Installer.rpm/common/functions/rpmBuild.sh
prepareEnvironment
buildBaselineProjects
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "The Required Baseline Projects Were Not Built Successfully."
   exit 1
fi

exit 0
