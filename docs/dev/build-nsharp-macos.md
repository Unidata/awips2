A little known fact in the world of AWIPS(II) is just how dependent the system still is on NAWIPS-GEMPAK.  The entire National Centers Perspective is dependent on pre-built shared object files for 64-bit Linux, which means that all of the D2D plugins which extend NSHARP (for bufr obs, NPP profiles, forecast models, etc.) also depend on these libraries.

This dependency has prevented use of the NSHARP plugin in the first release (15.1.1) of the [OS X CAVE client](http://www.unidata.ucar.edu/downloads/awips2/awips2-cave.dmg).  These are the steps taken to build NSHARP and GEMPAK libraries for OS X AWIPS 16.2.2.

You will need the [https://github.com/Unidata/awips2-gemlibs](https://github.com/Unidata/awips2-gemlibs) repository on your Mac, as well as gcc and gfortran (from XCode).  Pay attention to any version-specific include path or linked files, such as `/usr/local/Cellar/gcc/4.9.2_1/lib/gcc/4.9/`, always account for the correct versions and locations on your own system.

## NSHARP pre-built libraries

> libbignsharp.dylib

Using the script below, the NSHARP dynamic library is built from C and FORTRAN source files (and their required include files supplied by the `awips2-gemlibs` repository, and as linked against `$GEMINC`, meaning that GEMPAK for OS X must be built and installed).

	git clone https://github.com/Unidata/awips2-gemlibs.git
	cd awips2-gemlibs/nsharp/

An optional step, which can be performed in a separate script or within the build script below, is to create *ld-style* *.a files in `$OS_LIB` which can then be referenced with `-l` flags (e.g. `-lgemlib`):

	libs=(snlist sflist nxmlib gemlib gplt cgemlib rsl device xwp xw ps gn nsharp netcdf textlib)
	for file in ${libs[@]}
	do
	  if [ ! -f $OS_LIB/lib$file.a ]; then
	    echo "$OS_LIB/lib$file.a does not exist"
	    if [ -f $OS_LIB/$file.a ]; then
	      cp $OS_LIB/$file.a $OS_LIB/lib$file.a
	      echo "copied OS_LIB/$file.a to OS_LIB/lib$file.a for linking"
	    fi
	  fi
	done


Build libbignsharp.dylib with the following script (Note the GEMPAK includes and links `-I$NSHARP`, `-I$GEMPAK/include`, `-L$OS_LIB`, etc.).
	
	#!/bin/bash
	cd ~/awips2-gemlibs/nsharp/
	. $NAWIPS/Gemenviron.profile
	CC=gcc
	FC=gfortran
	
	export NSHARP=$GEMPAK/source/programs/gui/nsharp
	export NWX=$GEMPAK/source/programs/gui/nwx
	
	myLibs="$OS_LIB/ginitp_alt.o $OS_LIB/gendp_alt.o"
	
	myCflags="$CFLAGS -I. -I./Sndglib -I$NSHARP  -I$GEMPAK/include  -I$OS_INC -I$NWX \
	-I/opt/X11/include/X11 -I/usr/include/Xm -I/opt/local/include -I/usr/include/malloc -Wcomment -Wno-return-type -Wincompatible-pointer-types -DUNDERSCORE -fPIC -DDEBUG -c"
	
	myFflags="-I. -I$OS_INC -I$GEMPAK/include -I$NSHARP -fPIC -g -c -fno-second-underscore -fmax-errors=200 -std=f95"
	
	myLinkflags="-L/usr/local/Cellar/gcc/4.9.2_1/lib/gcc/4.9/ -L/opt/local/lib -L$OS_LIB -L. -L./Sndglib -L/usr/X11R6/lib \
	-shared -Wl -Wcomment -Wincompatible-pointer-types -Wimplicit-function-declaration -Wno-return-type,-install_name,libbignsharp.dylib -o libbignsharp.dylib"
	
	myLibsInc="$OS_LIB/ginitp_alt.o $OS_LIB/gendp_alt.o $OS_LIB/libnxmlib.a $OS_LIB/libsnlist.a \
	 $OS_LIB/libsflist.a $OS_LIB/libgemlib.a $OS_LIB/libcgemlib.a $OS_LIB/libgplt.a $OS_LIB/libdevice.a \
	 $OS_LIB/libxwp.a $OS_LIB/libxw.a $OS_LIB/libps.a  $OS_LIB/libgn.a $OS_LIB/libcgemlib.a $OS_LIB/libgemlib.a \
	 $OS_LIB/libnetcdf.a $OS_LIB/libtextlib.a $OS_LIB/libxml2.a $OS_LIB/libxslt.a \
	 $OS_LIB/libgemlib.a $OS_LIB/libcgemlib.a $OS_LIB/librsl.a $OS_LIB/libbz2.a"
	
	myLinktail="-I$OS_INC \
	  -I$GEMPAK/include -I$NWX -I$NSHARP -I. -I./Sndglib  -I/opt/X11/include/X11 -I/usr/include -I/usr/include/Xm -I/opt/local/include/ -I/opt/local/include -lhdf5 -lgfortran -ljasper -lpng -liconv -lc -lXt -lX11 -lz -lm -lXm"
	  
	$CC $myCflags *.c Sndglib/*.c
	$FC $myFflags *.f
	$CC $myLinkflags *.o $myLibsInc $myLinktail
	
	cp libbignsharp.dylib ~/awips2-ncep/viz/gov.noaa.nws.ncep.ui.nsharp.macosx/
		
	
## GEMPAK pre-built libraries

> libgempak.dylib

libgempak.dylib is built in a similar way as libbignsharp.dylib:

	#!/bin/bash
	cd ~/awips2-gemlibs/gempak/
	. $NAWIPS/Gemenviron.profile
	CC=gcc
	FC=gfortran

	myCflags="$CFLAGS -I. -I$GEMPAK/source/diaglib/dg -I$GEMPAK/source/gemlib/er \
	-I/opt/X11/include/X11 -I/usr/include/Xm -I/opt/local/include -I/usr/include/malloc -fPIC -DDEBUG -c"
 
 	myFflags="-I. -I$OS_INC -I$GEMPAK/include -fPIC -g -c -Wtabs -fno-second-underscore"

	myLinkflags="-L/usr/local/Cellar/gcc/4.9.2_1/lib/gcc/4.9/ -L/opt/local/lib -L$OS_LIB -L. \
	-shared -Wl -Wno-return-type,-install_name,libgempak.dylib -o libgempak.dylib"
	
	myLibs="$OS_LIB/ginitp_alt.o $OS_LIB/gendp_alt.o $OS_LIB/libcgemlib.a \
	$OS_LIB/libsflist.a $OS_LIB/gdlist.a $OS_LIB/libcgemlib.a $OS_LIB/libgemlib.a \
	$OS_LIB/libcgemlib.a $OS_LIB/libgplt.a $OS_LIB/libdevice.a $OS_LIB/libcgemlib.a \
	$OS_LIB/libgn.a $OS_LIB/libgemlib.a $OS_LIB/libcgemlib.a $OS_LIB/libnetcdf.a \
	$OS_LIB/libcgemlib.a $OS_LIB/libtextlib.a $OS_LIB/libxml2.a $OS_LIB/libxslt.a \
	$OS_LIB/libcgemlib.a $OS_LIB/libgemlib.a $OS_LIB/libcgemlib.a $OS_LIB/libcgemlib.a \
	$OS_LIB/librsl.a $OS_LIB/libcgemlib.a $OS_LIB/libbz2.a"
	
	myLinktail="-I$OS_INC -I$GEMPAK/include -I. -I/opt/X11/include/X11 -I/usr/include \
	-I/usr/include/Xm -I/opt/local/include/ -I/opt/local/include \
	-lhdf5 -lgfortran -ljasper -lpng -liconv -lc -lXt -lX11 -lz -lm -lXm"

	$CC $myCflags *.c
	$FC $myFflags *.f
	$CC $myLinkflags *.o $myLibs $myLinktail

	cp libgempak.dylib ~/awips2-ncep/viz/gov.noaa.nws.ncep.viz.gempak.nativelib.macosx/


> libcnflib.dylib


	#!/bin/bash
	cd ~/awips2-gemlibs/cnflib/
	. $NAWIPS/Gemenviron.profile
	CC=gcc
	FC=gfortran

	myCflags="$CFLAGS -I/opt/X11/include/X11 -I/usr/include/Xm -I/opt/local/include \
	-I/usr/include/malloc -Wno-return-type -DUNDERSCORE  -fPIC -DDEBUG -g -c"
		
	myLinkflags="-L/usr/local/Cellar/gcc/4.9.2_1/lib/gcc/4.9/ -L/opt/local/lib \
	-shared -Wl -Wno-return-type,-install_name,libcnflib.dylib -o libcnflib.dylib"
	
	myLinktail="-lgfortran -lc"
	
	myLibs="$OS_LIB/ginitp_alt.o $OS_LIB/gendp_alt.o $OS_LIB/gdlist.a $OS_LIB/gdcfil.a \
	$OS_LIB/libgemlib.a $OS_LIB/libgplt.a $OS_LIB/libdevice.a $OS_LIB/libgn.a \
	$OS_LIB/libcgemlib.a $OS_LIB/libgemlib.a $OS_LIB/libnetcdf.a $OS_LIB/libtextlib.a \
	$OS_LIB/libxslt.a $OS_LIB/libxml2.a -liconv \
	$OS_LIB/libz.a $OS_LIB/librsl.a -lbz2"

	$CC $myCflags *.c
	$CC $myLinkflags *.o $myLibs $myLinktail
	
	cp libcnflib.dylib ~/awips2-ncep/viz/gov.noaa.nws.ncep.viz.gempak.nativelib.macosx/


> libaodtv64.dylib

	#!/bin/bash
	CC=gcc
	FC=gfortran
	
	cd ~/awips2-gemlibs/aodt/AODTLIB/
	
	gcc -fPIC -g -c -Wall *.c *.h
	gcc -shared -Wl,-Wno-return-type,-install_name,libaodtv64.dylib -o libaodtv64.dylib *.o -lc
	
	cp libaodtv64.dylib ~/awips2-ncep/viz/gov.noaa.nws.ncep.viz.gempak.nativelib.macosx/
	
	

> libg2g.dylib

	#!/bin/bash
	cd ~/awips2-gemlibs/g2g/
	. $NAWIPS/Gemenviron.profile
	CC=gcc
	FC=gfortran
	
	myCflags="$CFLAGS -I$GEMPAK/include -I. -I$GEMPAK/source/diaglib/dg \
	-I$GEMPAK/source/gemlib/er -I/opt/X11/include/X11 -I/usr/include/Xm \
	-I/opt/local/include -I/usr/include/malloc -Wno-return-type -DUNDERSCORE \
	-fPIC -DDEBUG -c"

	myFflags="-I. -I$OS_INC -I$GEMPAK/include -fPIC -g -c -Wtabs -fno-second-underscore"

	myLinkflags="-L/usr/local/Cellar/gcc/4.9.2_1/lib/gcc/4.9/ -L/opt/local/lib \
	-L/usr/X11R6/lib -shared -Wl -Wno-return-type,-install_name,libg2g.dylib -o libg2g.dylib"
	
	myLinktail="-lgfortran $OS_LIB/libjasper.a -lpng  -lc"

	myLibs="$OS_LIB/ginitp_alt.o $OS_LIB/gendp_alt.o $OS_LIB/gdlist.a \
	$OS_LIB/gdcfil.a $OS_LIB/libgemlib.a $OS_LIB/libgplt.a $OS_LIB/libdevice.a \
	$OS_LIB/libgn.a $OS_LIB/libcgemlib.a $OS_LIB/libgemlib.a $OS_LIB/libnetcdf.a \
	$OS_LIB/libtextlib.a $OS_LIB/libxslt.a $OS_LIB/libxml2.a \
	-liconv $OS_LIB/libz.a $OS_LIB/librsl.a -lbz2"
	
	$CC $myCflags *.c
	$FC $myFflags *.f
	$CC $myLinkflags *.o $myLibs $myLinktail

	cp libg2g.dylib ~/awips2-ncep/viz/gov.noaa.nws.ncep.viz.gempak.nativelib.macosx/
