#!/bin/csh

# Ensure that awips2-cli is installed.
rpm -q awips2-cli >& /dev/null
set RC="$?"

#if installed, set the variable
if ( "${RC}" == "0" ) then
	set CLI_INSTALL=/awips2/fxa
	setenv PATH ${CLI_INSTALL}/bin:${PATH}
endif

