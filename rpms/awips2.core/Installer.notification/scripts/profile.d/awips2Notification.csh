#!/bin/csh

# Determine where notification has been installed.
set NOTIFICATION_INSTALL="/awips2/notification"
set QPID_LIB_DIR="/awips2/qpid/lib"

if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH ${NOTIFICATION_INSTALL}/lib:${QPID_LIB_DIR}:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH ${NOTIFICATION_INSTALL}/lib:${QPID_LIB_DIR}
endif

if $?PATH then
   setenv PATH ${NOTIFICATION_INSTALL}/bin:$PATH
else
   setenv PATH ${NOTIFICATION_INSTALL}/bin
endif
