if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH /awips2/ldm/lib:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH /awips2/ldm/lib
endif

if $?PATH then
   setenv PATH /awips2/ldm/bin:/awips2/ldm/decoders:/awips2/ldm/util:$PATH
else
   setenv PATH /awips2/ldm/bin:/awips2/ldm/decoders:/awips2/ldm/util
endif
