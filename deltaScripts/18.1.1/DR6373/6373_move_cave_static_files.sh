#!/bin/bash

# 6373 - Move SCAN localization files to common_static.
#
# Author: tgurney
# Aug 11, 2017

did_work=false
echo INFO: Moving SCAN localization files to common_static
for old_location in /awips2/edex/data/utility/cave_static/*/*/scan; do
    if [[ ! -e "${old_location}" ]]; then
        continue
    fi
    new_location=${old_location/cave_static/common_static}
    echo INFO: Moving "${old_location}" to "${new_location}"
    did_work=true
    if [[ ! -d "${new_location}" ]]; then
        sudo -u awips mkdir -p -m 750 "${new_location}"
    else
        echo "WARN: ${new_location} already exists. Just copying newer files"
    fi
    rsync -aux "${old_location}" "${new_location}/.." &&
        rm -rf --one-file-system "${old_location}"
    find "${new_location}" -xdev -type f -name '*.md5' -delete
    echo INFO: Done moving "${old_location}" to "${new_location}"
done

if [[ "${did_work}" == "false" ]]; then
    echo INFO: There are no files to move. Did nothing
else
    echo INFO: Done moving localization files
fi
