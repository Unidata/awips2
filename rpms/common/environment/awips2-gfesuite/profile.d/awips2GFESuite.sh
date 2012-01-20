#!/bin/bash

GFESUITE_PATH="/awips2/GFESuite/bin"

# Verify existence
if [ ! -d ${GFESUITE_PATH} ]; then
   return
fi

# Ensure that it is not already in the path
CHECK_PATH=`echo ${PATH} | grep ${GFESUITE_PATH}`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi

# Add it to the path
export PATH=${GFESUITE_PATH}:${PATH}
