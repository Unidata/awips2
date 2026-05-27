#!/bin/bash

if [ $(id -u) -eq 0  -a ! -v A2LIBS ]; then
   return;
fi

# Determine where awips2-cli has been installed.
if [ -d /awips2/fxa ]; then
   CLI_INSTALL=/awips2/fxa
   export PATH=${CLI_INSTALL}/bin:${PATH}
fi

