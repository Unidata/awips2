#!/bin/bash

# #5345
# Convert sea level pressure from hPa to Pa in all h5 files

TARGET=/awips2/edex/data/hdf5/obs
THIS_LOCATION=$(dirname $0)
success=0

for item in $(find $TARGET -type f -name "*.h5"); do
    $THIS_LOCATION/_update_metar_slp_values.py $item
    if [[ $? -ne 0 ]]; then
        success=1
    fi
done

if [[ success -eq 0 ]]; then
    echo INFO: No errors reported.
else
    echo "ERROR: There was a problem with one or more updates; see above."
fi
