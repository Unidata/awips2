#!/bin/csh

setenv GROOVY_HOME="/awips2/groovy"

if $?PATH then
   setenv PATH ${GROOVY_HOME}/bin:$PATH
else
   setenv PATH ${GROOVY_HOME}/bin
endif
