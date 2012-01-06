
The hdf-java product has been built and tested on:

32-bit Java 2 SDK
    * Linux
    * Solaris
    * Mac Intel
    * Windows (Vista/XP/2000)

64-bit Java 2 SDK
    * Linux 64-bit 
    * Linux 64-bit iitanium 
    * Solaris 64-bit
    * Mac Intel


This code will likely work on most platforms that support HDF and Java,
although the configure and Makefile may need to be adjusted.

This code does not compile on HP_UX 11, due to limitations in the HP
Java Native Interface.



Unix Build Instructions
=======================

Prerequisites:
    1. HDF4 binary distribution (source is not needed)
    2. HDF5 binary distribution (source is not needed)
    3. Java Development Kit 1.5.x or above
    4. Exterenal libraries: szip, zlib, jpeg (for HDF4 only)

Build:

1. Configure

Use the example configure file at Config/config.sh

The configure step must specify the paths to the required software and
where to install.  

  ./configure --prefix=INSTDIR \
    --with-hdf5=INC,LIB \
    --with-hdf4=INC,LIB --with-jdk=INC,LIB \
    --with-libjpeg=$JPEG/include,$JPEG/lib \
    --with-libsz=$SZIP/include,$SZIP/lib \
    --with-libz=$ZLIB/include,$ZLIB/lib \

Where
    --with-hdf5=INC,LIB = path to where the HDF5 include and library are installed,
        e.g., /usr/local/include,/usr/local/lib

    --with-hdf4=INC,LIB = path to where the HDF4.1r5 include and library are installed, 
        e.g., /usr/local/include,/usr/local/lib

    --with-jdk=INC,LIB = path to where the JDK1.3.1 include and library are installed, 
        e.g., /usr/local/jdk1.3.1/include,/usr/local/jdk1.3.1/jre/lib

    --prefix=INSTDIR = where to install the HDF java products, 
        e.g., /work/hdf-java

For convenience, Config/config.sh is provided as an example script for configuration.

2. Build

Build with the command 'make'.

To install, use 'make install'

To generate javadocs, 'make javadocs'



Windows Build Instructions
=======================

Prerequisites:

    1. HDF4 binary distribution (source is not needed)
    2. HDF5 binary distribution (source is not needed)
    3. Java Development Kit 1.5.x or above
    4. Microsfot Visual C++

Build:

1. Build Java source code
    a) configure/modify the batch file, windows/Makefile_java.bat
        i) set the %JAVAHOME% in the batch file. 
           %JAVAHOME% is the home directory of the Java Development Kit, 
           such as SET JAVAHOME=d:\java\jdk1.5.0
        ii) set the %HDFJAVA% in the batch file. 
           %HDFJAVA% is the directory of this HDF object source code, 
           such as SET HDFJAVA=D:\work\hdf-java
    b) run batch file, windows/Makefile_java.bat

2. Build the HDF4 native C code with Microsfot Visual C++
    a) modify make file, windows/jhdf.mak
    b) nmake /f jhdf.mak

3. Build the HDF5 native C code with Microsfot Visual C++
    a) modify make file, windows/jhdf5.mak
    b) nmake /f jhdf5.mak


For More Information:
====================

  http://www.hdfgroup.org/hdf-java-html

  help@hdfgroup.org


