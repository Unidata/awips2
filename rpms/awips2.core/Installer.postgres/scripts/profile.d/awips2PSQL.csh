#!/bin/csh

# Determine where psql has been installed.
set PSQL_INSTALL="/awips2/psql"

if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH ${PSQL_INSTALL}/lib:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH ${PSQL_INSTALL}/lib
endif

if $?PATH then
   setenv PATH ${PSQL_INSTALL}/bin:$PATH
else
   setenv PATH ${PSQL_INSTALL}/bin
endif
