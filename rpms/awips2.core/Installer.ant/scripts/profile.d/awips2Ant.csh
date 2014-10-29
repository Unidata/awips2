#!/bin/csh
if ( $USER != "awips" && $USER != 'root' ) then
  return
  exit 0
endif

# Determine where ant has been installed.
setenv ANT_INSTALL "/awips2/ant"

if $?PATH then
   setenv PATH ${ANT_INSTALL}/bin:$PATH
else
   setenv PATH ${ANT_INSTALL}
endif
