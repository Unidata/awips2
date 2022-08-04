#!/bin/bash

# This script deletes all .pyc and .pyo Python files from the localization
# tree. It also deletes the md5 files corresponding to those files.

dir=/awips2/edex/data/utility
echo "INFO: Started deleting all compiled Python files from ${dir}"
find "${dir}" -type f -regex '.*\.py[co]\(\.md5\)?$' -delete
echo "INFO: Finished deleting compiled Python files from ${dir}"
