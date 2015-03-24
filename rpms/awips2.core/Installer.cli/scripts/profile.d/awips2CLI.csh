#!/bin/csh

#if installed, set the variable
if ( -d /awips2/fxa ) then
	set CLI_INSTALL=/awips2/fxa
	#if check CLI_INSTALL is set, set it in the path
	if ( "${CLI_INSTALL}" != "" ) then
		setenv PATH ${CLI_INSTALL}/bin:${PATH}
	endif
endif

