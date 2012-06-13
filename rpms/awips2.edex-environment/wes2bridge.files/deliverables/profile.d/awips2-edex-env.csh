#!/bin/csh

set EDEX_ENV_PATH="/awips2/edex-environment/macro"

if $?PATH then
   setenv PATH ${EDEX_ENV_PATH}:$PATH
else
   setenv PATH ${EDEX_ENV_PATH}
endif
