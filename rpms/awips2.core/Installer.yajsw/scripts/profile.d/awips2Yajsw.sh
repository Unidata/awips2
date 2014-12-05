#!/bin/bash

# Is Yajsw installed?
rpm -q awips2-yajsw > /dev/null 2>&1
if [ $? -ne 0 ]; then
	return
fi

YAJSW_INSTALL="/awips2/yajsw"
# Update The Environment
export YAJSW_HOME=${YAJSW_INSTALL}