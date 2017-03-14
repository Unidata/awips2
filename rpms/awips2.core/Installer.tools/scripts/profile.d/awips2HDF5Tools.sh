#!/bin/bash

if [ -d /awips2/tools ]; then
   # Determine Where awips2-tools Has Been Installed.
   HDF5_TOOLS_INSTALL=/awips2/tools
   if [ "${HDF5_TOOLS_INSTALL}" = "" ]; then
      return
   fi

   # Update The Environment.
   # Determine If awips2-tools Is Already Part Of The Path.
   CHECK_PATH=`echo ${PATH} | grep ${HDF5_TOOLS_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      # awips2-tools Is Not In The Path; Add It.
      export PATH=${HDF5_TOOLS_INSTALL}/bin:${PATH}
   fi

   # Determine If awips2-tools Is On LD_LIBRARY_PATH.
   CHECK_PATH=`echo ${LD_LIBRARY_PATH} | grep ${HDF5_TOOLS_INSTALL}`
   if [ "${CHECK_PATH}" = "" ]; then
      # awips2-tools Is Not On LD_LIBRARY_PATH; Add It.
      export LD_LIBRARY_PATH=${HDF5_TOOLS_INSTALL}/lib:${LD_LIBRARY_PATH}
   fi
fi
