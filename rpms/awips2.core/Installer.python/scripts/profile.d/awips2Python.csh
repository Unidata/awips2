#!/bin/csh

set PYTHON_INSTALL="/awips2/python"
setenv AWIPS_PYTHON ${PYTHON_INSTALL}

if $?PATH then
   setenv PATH ${PYTHON_INSTALL}/bin:$PATH
else
   setenv PATH ${PYTHON_INSTALL}/bin
endif

if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH ${PYTHON_INSTALL}/lib:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH ${PYTHON_INSTALL}/lib
endif
