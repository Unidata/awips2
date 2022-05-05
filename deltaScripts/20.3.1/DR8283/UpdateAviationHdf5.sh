#! /bin/bash
# NOTE: This script must be run on a machine where /awips2/edex/data/share/aviation 
#       is mounted (e.g. dv3)
#
# This delta script runs the ptrepack script on the HDF5 files in 
# /awips2/edex/data/share/aviation/*.hd5
# 
echo "Running delta script ${0} for RODO DR 8283"

for h5file in /awips2/edex/data/share/aviation/*.hd5
do
    backfile=${h5file}.DR8283
    
    # if backfile doesn't exist
    if [ ! -e ${backfile} ]
    then
        # copy original file to backfile
        cp -p ${h5file} ${backfile}
        
        /awips2/python/bin/ptrepack --keep-source-filters --overwrite ${backfile}:/ ${h5file}:/
    fi
done

echo "Delta script ${0} complete"
 