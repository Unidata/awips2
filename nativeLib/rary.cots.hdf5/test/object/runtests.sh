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

TESTS="testh4file testh5file"

JH45INSTALLDIR=.

LIBDIR=$JH45INSTALLDIR"/lib"

CLASSPATH=$LIBDIR"/jh4toh5.jar:"$LIBDIR"/jhdf.jar:"$LIBDIR"/jhdf5.jar:"$LIBDIR"/jhdf5obj.jar:"$LIBDIR/"jhdfobj.jar:../.."

LD_LIBRARY_PATH=$LIBDIR"/linux"

export CLASSPATH
export LD_LIBRARY_PATH

## note: disabled tests for HDF4--unresolved problems in test suite.

##cp ../h4toh5/testfiles/*.hdf .
cp ../h4toh5/Expected/*.h5 .

##echo "Test HDF4"
##TESTFILES="image_lib_lonetest image_lib_test sds_lib_lonetest sds_lib_test testallvgroup vdata_lib_lonetest vdata_lib_test vg_all_testname vg_bas_test"

###HDF4 tests
##for i in $TESTFILES
##do
##
##echo "***"
##echo $i
##echo "***"
##/common/awips/jdk1.6.0_05/bin/java -Djava.library.path=$LD_LIBRARY_PATH -Dncsa.hdf.hdflib.HDFLibrary.hdflib=$LIBDIR"/linux/libjhdf.so" test.object.TestH4File $i
##
##
### check against correct output....
##
##done
##

TESTFILES="image_lib_test.h5 sds_lib_test.h5 testallvgroup.h5 vdata_lib_test.h5 vg_all_test.h5 vg_all_testname.h5"

echo "Test HDF5"
#HDF5 tests
for i in $TESTFILES
do

echo "***"
echo $i
echo "***"
/common/awips/jdk1.6.0_05/bin/java -Djava.library.path=$LD_LIBRARY_PATH -Dncsa.hdf.hdf5lib.H5.hdf5lib=$LIBDIR"/linux/libjhdf5.so" test.object.TestH5File $i


# check against correct output....

done
