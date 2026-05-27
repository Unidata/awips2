#!/bin/bash

if [ $(id -u) -eq 0  -a ! -v A2LIBS ]; then
   return;
fi

EDEX_ENV_PATH="/awips2/edex-environment/macro"

CHECK_PATH=`echo ${PATH} | grep ${EDEX_ENV_PATH}`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi

export PATH=${EDEX_ENV_PATH}:${PATH}
