#!/bin/bash

if [ -d /awips2/yajsw ]; then
   YAJSW_INSTALL="/awips2/yajsw"
# Update The Environment
   export YAJSW_HOME=${YAJSW_INSTALL}
fi
