#!/bin/bash

# Is awips2-notification Installed?
rpm -q awips2-notification > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   return
fi

# Determine Where awips2-notification Has Been Installed.
NOTIFICATION_INSTALL="/awips2/notification"
if [ "${NOTIFICATION_INSTALL}" = "" ]; then
   return
fi

# Update The Environment.
# Determine if awips2-notification is Already On LD_LIBRARY_PATH
CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${NOTIFICATION_INSTALL}`
if [ "${CHECK_PATH}" = "" ]; then
   # awips2-notification Is Not On LD_LIBRARY_PATH; Add It.
   export LD_LIBRARY_PATH=${NOTIFICATION_INSTALL}/lib:${LD_LIBRARY_PATH}
fi

# Determine if awips2-notification Is Already Part Of The Path.
CHECK_PATH=`echo ${PATH} | grep ${NOTIFICATION_INSTALL}`
if [ "${CHECK_PATH}" = "" ]; then
   # awips2-notification Is Not In The Path; Add It To The Path.
   export PATH=${NOTIFICATION_INSTALL}/bin:${PATH}
fi
