#!/bin/sh

#****************************************************************************
#* NCSA HDF                                                                 *
#* National Comptational Science Alliance                                   *
#* University of Illinois at Urbana-Champaign                               *
#* 605 E. Springfield, Champaign IL 61820                                   *
#*                                                                          *
#* For conditions of distribution and use, see the accompanying             *
#* COPYING file.                                                            *
#*                                                                          *
#****************************************************************************/
JH45INSTALLDIR=.

LIBDIR=$JH45INSTALLDIR"/lib"

CLASSPATH=$LIBDIR"/jh4toh5.jar:"$LIBDIR"/jhdf.jar:"$LIBDIR"/jhdf5.jar:"$LIBDIR"/jhdf5obj.jar:"$LIBDIR/"jhdfobj.jar:../.."

LD_LIBRARY_PATH=$LIBDIR"/linux"

export CLASSPATH
export LD_LIBRARY_PATH

#modify to try different files
/common/awips/jdk1.6.0_05/bin/java -Djava.library.path=$LD_LIBRARY_PATH -Dncsa.hdf.hdflib.HDFLibrary.hdflib=$LIBDIR"/linux/libjhdf.so" test.object.TestH4File vg_all_test.hdf

#check output?

