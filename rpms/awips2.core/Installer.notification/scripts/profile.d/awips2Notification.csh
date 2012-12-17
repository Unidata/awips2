#!/bin/csh

# Determine where notification has been installed.
set NOTIFICATION_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}' awips2-notification`

if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH ${NOTIFICATION_INSTALL}/lib:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH ${NOTIFICATION_INSTALL}/lib
endif

if $?PATH then
   setenv PATH ${NOTIFICATION_INSTALL}/bin:$PATH
else
   setenv PATH ${NOTIFICATION_INSTALL}/bin
endif
