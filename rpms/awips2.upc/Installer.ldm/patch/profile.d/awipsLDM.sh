CHECK_PATH=`echo ${LD_LIBRARY_PATH}`
if [ "${CHECK_PATH}" = "" ]; then
   export LD_LIBRARY_PATH=/awips2/ldm/lib
else
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/awips2/ldm/lib
fi

CHECK_PATH=`echo ${PATH}`
if [ "${CHECK_PATH}" = "" ]; then
   export PATH=/awips2/ldm/bin:/awips2/ldm/decoders:/awips2/ldm/util
else
   export PATH=${PATH}:/awips2/ldm/bin:/awips2/ldm/decoders:/awips2/ldm/util
fi
