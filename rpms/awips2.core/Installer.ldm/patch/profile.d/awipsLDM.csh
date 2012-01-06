if $?LD_LIBRARY_PATH then
   setenv LD_LIBRARY_PATH /usr/local/ldm/lib:$LD_LIBRARY_PATH
else
   setenv LD_LIBRARY_PATH /usr/local/ldm/lib
endif

if $?PATH then
   setenv PATH /usr/local/ldm/bin:$PATH
else
   setenv PATH /usr/local/ldm/bin
endif
