#!/bin/sh

## This is an example script to set the configure parameters for
## the HDF Java products.
##
## The paths need to be set according to the local configuration
##
## May need to adjust the arguments to configure
##
##

##
## IMPORTANT NOTE:  The make files require 'gmake'
##
##  Be sure to 'setenv MAKE gmake' if necessary
##

INSTDIR= #where to install the hdfview.

JAVAINC=  #path to java includes (jni.h, etc.)
JAVALIB=  #path to java lib (the rt.jar, etc.)

HDF5= # path to HDF5 installation, e.g., /usr/local/hdf5-1.6.2
HDF4= # path to HDF4 installation (if used)
HDF45= # path to HDF4 to HDF5 installation (if used)

## Autoconf detects shared libraries, but we need static versions
## must set these paths for external libraries needed for HDF libraries.

# JPEG is required by HDF4.  If HDF4 is used, _must_ set JPEG
JPEG= # path to JPEG installation (the path to libjpeg.a is needed)

# GZIP is required by HDF4 and optional for HDF5.  
# If HDF4 is used, _must_ set GZIP
# If HDF5 is used and zlib is used, _must_ set GZIP
GZIP= # path to GZIP installation (the path to libz.a is needed)

# SZIP is optional for HDF4 and HDF5.  
# If szip is used in one or both HDF libraries, _must_ set SZIP
SZIP= # path to SZIP installation (the path to libsz.a is needed)

####

./configure --prefix=$INSTDIR --with-jdk=$JAVAINC,$JAVALIB --with-hdf5=$HDF5/include,$HDF5/lib --with-hdf4=$HDF4/include,$HDF4/lib --with-libsz=$SZIP/include,$SZIP/lib --with-libz=$GZIP/include,$GZIP/lib --with-libjpeg=$JPEG/include,$JPEG/lib 

# other options
#  
#  --without-hdf4  -- omit HDF4
#  --without-hdf5  -- omit HDF5
#  --without-libsz  -- omit SZIP
#  --without-libz  -- omit GZIP
#  --without-libsz  -- omit GZIP
#

#
#  Some options required only for macOSX
# 
#  -build=powerpc-apple 
#  --with-jdkclasses=   # path to classes if not in 'jdk/lib'
#  --with-javabin=  # path to java bin, if not in 'jdk/bin'

