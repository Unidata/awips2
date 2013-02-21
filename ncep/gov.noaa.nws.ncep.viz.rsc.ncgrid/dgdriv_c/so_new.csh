#!/bin/csh
#
# 1. Create a Java project and source folder....
#
# 2. Configure build path (see M. Li's lecture note): 
#	Right click the working project -> Build Path -> Configure build path 
#	-> Libraries -> Add JARs -> com.sun.jna – jna.jar
#
# 3. cp /usr/lib/gcc/i386-redhat-linux/3.4.6/libg2c.so locally, e.g.,
#	$AWIPS2/lib/so 
#
# 4. Use this script to create a shared library (.so)
#
# 5. Deploy the SL and add the path, e.g., AWIPS2/lib, to LD_LIBRARY_PATH 
#    (in ~/.alias)
#

$RM *.o *.so

set myLinkflags = "-L/usr/lib/gcc/i386-redhat-linux/3.4.6/ -shared -Wl,-soname,libgempak.so -o libgdtest.so"
set myLinktail = "-lg2c -lc"

#
# Set C flags.  Include necessary *.h files from application and library functions, e.g., diaglib/dg/*.h 
#	gdlist/gdlist.h for gdldsp.c.  Note that diaglib/dg/*.h has layers of *.h, de.h, dl.h, df.h, dv.h.
#
set myCflags = "$CFLAGS -I$GEMPAK/source/diaglib/dg -I$GEMPAK/source/gemlib/er -DDEBUG -c"

#
# Set F flags.  Include gemlib/grdcmn for DG library
#
set myFflags = "-fPIC -g -c -Wall -fno-second-underscore"

echo
echo "Cflags and Fflags have been set"
echo " "

#
# Add libraries for gdlist (following gdlist.mk)
#

set myLibs = "$OS_LIB/ginitp_alt.o $OS_LIB/gendp_alt.o $OS_LIB/libsflist.a $OS_LIB/libgdlist.a $OS_LIB/libdiaglib.a $OS_LIB/libgridlib.a $OS_LIB/libprmcnvlib.a $OS_LIB/libgemlib.a $OS_LIB/libsflib.a $OS_LIB/libgplt.a $OS_LIB/libdevice.a $OS_LIB/libgn.a $OS_LIB/libcgemlib.a $OS_LIB/libgemlib.a $OS_LIB/libnetcdf.a $OS_LIB/libtextlib.a $OS_LIB/libxslt.a $OS_LIB/libxml2.a $OS_LIB/libiconv.a $OS_LIB/libz.a $OS_LIB/libbz2.a"

#
# Compile all C programs
# 	gcc "$myCflags" *.c"
#
echo "Compiling C programs... "
echo " "
$CC $myCflags *.c

#
# Compile all Fortran  programs
# 	g77 "$myFflags *.f"
#
echo "Compiling Fortran programs... "
echo " "
$FC $myFflags *.f

#
# Create a shared library
#	gcc "$myLinkflags" *.o $OS_LIB/libgdlist.a $myLibs $myLinktail"
#
echo "To create a Shared Library object... "
$CC $myLinkflags *.o $myLibs $myLinktail

set newlib = `ll -altr *.so | awk '{print $9}'`

echo " "
if ( $newlib == "libgdtest.so" ) then
	echo " "
	echo "****** Shared library is created ******"
	echo " "
	cp libgdtest.so libgempak.so
        /bin/rm *.o
else
        echo " "
	echo "****** Houston, we got problems ******"
	echo " "
endif

exit
