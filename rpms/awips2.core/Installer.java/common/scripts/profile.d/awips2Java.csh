#!/bin/csh

set JAVA_INSTALL="/awips2/java"
setenv JAVA_HOME "${JAVA_INSTALL}"

if $?PATH then
   setenv PATH ${JAVA_INSTALL}/bin:$PATH
else
   setenv PATH ${JAVA_INSTALL}/bin
endif
