#!/bin/bash

WES2BRIDGE_PATH="/awips2/wes2bridge/macro"

CHECK_PATH=`echo ${PATH} | grep ${WES2BRIDGE_PATH}`
if [ ! "${CHECK_PATH}" = "" ]; then
   return
fi

export PATH=${WES2BRIDGE_PATH}:${PATH}
