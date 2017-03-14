#!/bin/bash

# #5733
# Convert sea level pressure from Pa to hPa in all fssobs h5 files
# Author: tom.gurney@raytheon.com

TARGET=/awips2/edex/data/hdf5/fssobs
THIS_LOCATION=$(dirname $0)
success=0

for item in $(find $TARGET -type f -name "*.h5"); do
    $THIS_LOCATION/_update_fssobs_slp_values.py $item
    if [[ $? -ne 0 ]]; then
        success=1
    fi
done

if [[ success -eq 0 ]]; then
    echo INFO: No errors reported.
else
    echo "ERROR: There was a problem with one or more updates; see above."
fi
