#!/bin/bash

# Deletes the subclass cache for all users of CAVE
# Run on all lx
# Author: tgurney

filePath=caveData/.metadata/.plugins/com.raytheon.uf.viz.core/subclassCache.txt

echo INFO: Starting to delete subclass cache for all CAVE users.
for dir in /home/*; do
    if [[ -d $dir && -d $dir/caveData ]]; then
        rm -vf $dir/$filePath
    fi
done
echo INFO: Done.
