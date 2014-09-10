if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH /usr/local/ldm/lib:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH /usr/local/ldm/lib
endif

if $?PATH then
   setenv PATH ${PATH}:/usr/local/ldm/bin:/usr/local/ldm/decoders:/usr/local/ldm/util
else
   setenv PATH ${PATH}:/usr/local/ldm/bin:/usr/local/ldm/decoders:/usr/local/ldm/util
endif
