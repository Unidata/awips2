#!/bin/bash

# Deletes orphaned files from /awips2/edex/data/processing/.
# Run on all dx and px systems
# Author: dgilling

filePath=/awips2/edex/data/processing/

echo "INFO: Running delta script for DR 7522: Purging ${filePath}."
find "${filePath}" -type f -exec rm --force --verbose {} \;
find "${filePath}" -type d -empty -delete
echo "INFO: Done."
