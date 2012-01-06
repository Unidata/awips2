#!/bin/csh
#
# 1. Create a Java project and source folder....
#
# 2. Configure build path (see M. Li's lecture note): 
#	Right click the working project -> Build Path -> Configure build path 
#	-> Libraries -> Add JARs -> com.sun.jna – jna.jar
#
# 3. cp /usr/lib/gcc/i386-redhat-linux/3.4.6/libg2c.so locally, e.g.,
#	$AWIPS2/tools/lib and make myLinkFlags to contain -L$AWIPS2/tools/lib 
#
# 4. Use this script to create a shared library (.so)
#
# 5. Deploy the SL and add the path, e.g., AWIPS2/lib, to LD_LIBRARY_PATH 
#    (in ~/.alias)
#
rm -f *.o

echo "Compiling program... "
gcc -fPIC -g -c -Wall *.c *.h

echo "Creating a Shared Library object... "
gcc -shared -Wl,-soname,libaodtv64.so -o libaodtv64.so *.o -lc

echo "library is created and cp to build.cave\n "
echo " "
cp libaodtv64.so $DEV_BASE/workspace/build.cave/static/common/cave/caveEnvironment/lib 
endif
