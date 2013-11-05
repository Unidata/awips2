#!/bin/bash

# Ensure that awips2-cli is installed.
rpm -q awips2-cli > /dev/null 2>&1
RC="$?"
if [ ! "${RC}" = "0" ]; then
   # awips2-cli is not installed.
   return
fi

# Determine where awips2-cli has been installed.
CLI_INSTALL=`rpm -q --queryformat '%{INSTPREFIXES}\n' awips2-cli`
if [ "${CLI_INSTALL}" = "" ]; then
   return
fi

export PATH=${CLI_INSTALL}/bin:${PATH}
