#!/bin/bash

# Set all paths required by CAVE before installing.
export LD_LIBRARY_PATH=/awips2/java/lib:/awips2/python/lib:$LD_LIBRARY_PATH
export LD_PRELOAD=libpython.so
if [ -d /awips2/cave/lib ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib/lib_illusion:$LD_LIBRARY_PATH
fi
if [ -d /awips2/cave/lib64 ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib64/lib_illusion:$LD_LIBRARY_PATH
fi
# Need to use awips2-java to do this.
export PATH=/awips2/java/bin:/awips2/python/bin:${PATH}
export JAVA_HOME="/awips2/java/jre"

# Set the CAVE logfile location.
export LOGFILE_CAVE=/dev/null

# Use the eclipse p2 manager.
CAVE_EXE="/awips2/cave/cave"
NOSPLASH_ARG="-nosplash"
DIRECTOR_APP="-application org.eclipse.equinox.p2.director"
DESTINATION_ARG="-destination /awips2/cave"
INSTALL_ARG="-i com.raytheon.uf.common.base.feature.feature.group"
UNINSTALL_ARG="-u com.raytheon.uf.common.base.feature.feature.group"
REPO="-repository file:/awips2/cave/.repository/"

COMMON_CMD="${CAVE_EXE} ${NOSPLASH_ARG} ${DIRECTOR_APP} ${DESTINATION_ARG}"
INSTALL_CMD="${COMMON_CMD} ${INSTALL_ARG} ${REPO}"
UNINSTALL_CMD="${COMMON_CMD} ${UNINSTALL_ARG}"

# Uninstall any existing components since the p2 director does not
# support updating.
# If the feature is not installed, this does not fail quietly.
# Determine if the feature needs to be uninstalled.
${UNINSTALL_CMD} -verifyOnly > /dev/null 2>&1
if [ $? -eq 0 ]; then
   LOG_TIMESTAMP=`date`
   echo "uninstall previous STARTED: ${LOG_TIMESTAMP}"
   ${UNINSTALL_CMD}
   LOG_TIMESTAMP=`date`
   echo "uninstall previous COMPLETE: ${LOG_TIMESTAMP}"
fi

# complete the install
LOG_TIMESTAMP=`date`
echo "installation STARTED: ${LOG_TIMESTAMP}"
${INSTALL_CMD}
if [ $? -ne 0 ]; then
   exit 1
fi
LOG_TIMESTAMP=`date`
echo "installation COMPLETE: ${LOG_TIMESTAMP}"
   
# remove the repository
if [ -f /awips2/cave/.repository/artifacts.xml ]; then
   rm -f /awips2/cave/.repository/artifacts.xml
fi
   
if [ -f /awips2/cave/.repository/content.xml ]; then
   rm -f /awips2/cave/.repository/content.xml
fi
   
if [ -d /awips2/cave/.repository/features ]; then
   rm -rf /awips2/cave/.repository/features
fi
   
if [ -d /awips2/cave/.repository/plugins ]; then
   rm -rf /awips2/cave/.repository/plugins
fi 
