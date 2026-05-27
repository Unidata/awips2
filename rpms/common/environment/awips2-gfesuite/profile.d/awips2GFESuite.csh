#!/bin/csh

set GFESUITE_PATH="/awips2/GFESuite/bin"

if $?PATH then
   setenv PATH ${GFESUITE_PATH}:${PATH}
else
   setenv PATH ${GFESUITE_PATH}
endif
