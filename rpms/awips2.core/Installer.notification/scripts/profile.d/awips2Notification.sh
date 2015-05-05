#!/bin/bash

if [ -d /awips2/notification ]; then
   # Determine Where awips2-notification Has Been Installed.
   NOTIFICATION_INSTALL="/awips2/notification"
   QPID_LIB_DIR="/awips2/qpid/lib"

   # Update The Environment.
   # Determine if awips2-notification is Already On LD_LIBRARY_PATH
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${NOTIFICATION_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      # awips2-notification Is Not On LD_LIBRARY_PATH; Add It.
      _lib_dir=${NOTIFICATION_INSTALL}/lib
      if [ -d ${NOTIFICATION_INSTALL}/lib64 ]; then
         _lib_dir=${NOTIFICATION_INSTALL}/lib64
      fi
      export LD_LIBRARY_PATH=${_lib_dir}:${LD_LIBRARY_PATH}
   fi

   # Determine if the qpid lib directory is already on LD_LIBRARY_PATH
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${QPID_LIB_DIR}`
   if [ "${CHECK_PATH}" = "" ]; then
      export LD_LIBRARY_PATH=${QPID_LIB_DIR}:$LD_LIBRARY_PATH
   fi

   # Determine if awips2-notification Is Already Part Of The Path.
   CHECK_PATH=`echo ${PATH} | grep ${NOTIFICATION_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      # awips2-notification Is Not In The Path; Add It To The Path.
      export PATH=${NOTIFICATION_INSTALL}/bin:${PATH}
   fi
fi
