#!/bin/csh

set HDF5_TOOLS_INSTALL=`rpm -q --queryformat '%{FILENAMES}' awips2-tools`

if $?PATH then
   setenv PATH ${HDF5_TOOLS_INSTALL}/bin:$PATH
else
   setenv PATH ${HDF5_TOOLS_INSTALL}/bin
endif

if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH ${HDF5_TOOLS_INSTALL}/lib:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH ${HDF5_TOOLS_INSTALL}/lib
endif
