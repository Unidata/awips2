#!/bin/bash

EDEX_ENV_PATH="/awips2/edex-environment/macro"

CHECK_PATH=`echo ${PATH} | grep ${EDEX_ENV_PATH}`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi

export PATH=${EDEX_ENV_PATH}:${PATH}
