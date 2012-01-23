#!/bin/csh

set WES2BRIDGE_PATH="/awips2/wes2bridge/macro"

if $?PATH then
   setenv PATH ${WES2BRIDGE_PATH}:$PATH
else
   setenv PATH ${WES2BRIDGE_PATH}
endif
