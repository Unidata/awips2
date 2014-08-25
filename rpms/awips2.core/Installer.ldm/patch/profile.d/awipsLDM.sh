CHECK_PATH=`echo ${LD_LIBRARY_PATH}`
if [ "${CHECK_PATH}" = "" ]; then
   export LD_LIBRARY_PATH=/usr/local/ldm/lib
else
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/ldm/lib
fi

CHECK_PATH=`echo ${PATH}`
if [ "${CHECK_PATH}" = "" ]; then
   export PATH=/usr/local/ldm/bin:/usr/local/ldm/decoders:/usr/local/ldm/util
else
   export PATH=${PATH}:/usr/local/ldm/bin:/usr/local/ldm/decoders:/usr/local/ldm/util
fi
