#!/bin/bash

set -o errexit
#set -o xtrace

# recursively copies the given directory and then removes it
cprm() {

	if [ -d $1 ]; then
	    mkdir -p $2
	    cp -fa $1/* $2
    	rm -fr $1
	fi

}

cprm $tmp_location/bin $tmp_location/edex/bin
cprm $tmp_location/lib $tmp_location/edex/lib/native/linux32
cprm $tmp_location/awips $tmp_location
cprm $tmp_location/edex/lib/native/linux32/python2.5 $tmp_location/lib/python2.5/plat-linux2

hydro_base_dir=$tmp_location/awipsShare/hydroapps
cprm $tmp_location/hydroapps $hydro_base_dir

# in order for the ohd lib java gui interfaces to execute they must have
# access to the runso executable and ohd.util library.  the following
# places them into a location easily accessibly through a single mount point
# to the hydroapps directory 

mkdir -p $hydro_base_dir/bin
mkdir -p $hydro_base_dir/lib/native/linux32
cp $tmp_location/edex/bin/runso $hydro_base_dir/bin
ohd_util_libraries="libecpg_compat.so.3 libecpg_compat.so.3.4 libecpg.so.6 libecpg.so.6.4 library.ohd.util.so libjasper.so.1 libjasper.so.1.0.0 libjvm.so libpgtypes.so.3 libpgtypes.so.3.3 library.ohd.ofs.so library.ohd.pproc.so libgfortran.so.1 library.empty.motif.so library.ohd.whfs.so"
for lib in $ohd_util_libraries; do
	cp -P $tmp_location/edex/lib/native/linux32/$lib $hydro_base_dir/lib/native/linux32
done
