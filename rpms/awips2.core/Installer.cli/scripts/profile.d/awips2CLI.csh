#!/bin/csh

# Ensure that awips2-cli is installed.
rpm -q awips2-cli >& /dev/null
set RC="$?"

#if installed, set the variable
if ( "${RC}" == "0" ) then
	set CLI_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}' awips2-cli`
	#if check CLI_INSTALL is set, set it in the path
	if ( "${CLI_INSTALL}" != "" ) then
		setenv PATH ${CLI_INSTALL}/bin:${PATH}
	endif
endif

