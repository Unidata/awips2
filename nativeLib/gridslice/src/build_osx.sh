#!/bin/bash -fv
#
# build_osx.sh: Build gridslice library for macOS
# author: mjames@ucar
#
export C_INCLUDE_PATH=/System/Library/Frameworks/Python.framework/Headers
export CFLAGS="-I/System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7/ -I /System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python/numpy/core/include $CFLAGS"
gcc -c -fPIC $CFLAGS gridslice.c -o gridslice.o
gcc -c -fPIC $CFLAGS sliceConvert.c -o sliceConvert.o
gcc gridslice.o sliceConvert.o -lpython -shared -o gridslice.so
# cp gridslice.so /Library/Python/2.7/site-packages/
