#!/bin/csh

# Determine where ant has been installed.
set ANT_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}' awips2-ant`
setenv ANT_HOME "${ANT_INSTALL}"

if $?PATH then
   setenv PATH ${ANT_INSTALL}/bin:$PATH
else
   setenv PATH ${ANT_INSTALL}
endif
