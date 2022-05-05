#!/bin/bash
# Omaha #8143
# This script updates host names in all setup.env files to point to the new
# VM host names.
#
# Author: tgurney

echo INFO: Starting $0
now=$(date +%Y%m%d%H%M%S)

for file in /awips2/GFESuite/bin/setup.env /awips2/fxa/bin/setup.env /awips2/edex/bin/setup.env; do
    if [[ ! -f "${file}" ]]; then
        continue
    fi
    cp -a "${file}" "${file}.${now}.tmp"
    # This could be done with fewer regexes but it would be messy
    sed -r --in-place 's/dx1f?-/dv1-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/dx2f?-/dv2-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/dx3f?-/dv3-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/dx4f?-/dv4-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/dx5f?-/dv5-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/dx6f?-/dv6-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/dx7f?-/dv7-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/dx8f?-/dv8-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/px1f?-/pv1-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/px2f?-/pv2-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/px3f?-/pv3-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/px4f?-/pv4-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/cpsbn1-/cpv1-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/cp1f-/cpv1-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/cpsbn2-/cpv2-/g' "${file}.${now}.tmp"
    sed -r --in-place 's/cp2f-/cpv2-/g' "${file}.${now}.tmp"
    if ! diff "${file}" "${file}.${now}.tmp"; then
        cp -a "${file}" "${file}.${now}.bak"
        mv -v "${file}.${now}.tmp" "${file}"
    else
        echo INFO: No update needed for "${file}"
        rm -f "${file}.${now}.tmp"
    fi
done

echo INFO: $0 complete
