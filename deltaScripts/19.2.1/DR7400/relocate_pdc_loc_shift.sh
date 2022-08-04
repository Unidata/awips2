#!/bin/bash

# DR #7400: Relocates cave_static/base/hydro/config/pdc_loc_shift.txt to
# common_static/workstation/*/hydro/pdc_loc_shift.txt.


echo "INFO: Running delta script for DR 7400: Relocation pdc_loc_shift.txt."

if [[ -z "${LX_WORKSTATIONS}" ]]; then
    echo "ERROR: Environment variable LX_WORKSTATIONS is not defined. Exiting."
    exit -1
fi

# save checksum of base file
# if user hasn't altered it no reason to save it
base_checksum=$(md5sum /awips2/edex/data/utility/common_static/base/hydro/pdc_loc_shift.txt | cut -d ' ' -f 1)

for workstation in ${LX_WORKSTATIONS}; do
    workstation=`echo ${workstation} | tr '[A-Z]' '[a-z]'`
    # echo "DEBUG: WS: ${workstation}"
    
    rm --force /tmp/pdc_loc_shift.txt
    
    cmd="scp -B ${workstation}:/awips2/cave/etc/hydro/config/pdc_loc_shift.txt /tmp/pdc_loc_shift.txt"
    # echo "DEBUG: remote copy: ${cmd}"
    ${cmd}
    if [[ $? -ne 0 ]] ; then
        echo "ERROR: Failed to copy file [/awips2/cave/etc/hydro/config/pdc_loc_shift.txt] from host [${workstation}]."
        continue
    fi
    
    ws_checksum=$(md5sum /tmp/pdc_loc_shift.txt | cut -d ' ' -f 1)
    if [[ "${base_checksum}" == "${ws_checksum}" ]]; then
        echo "INFO: Skipping host [${workstation}] because file matches default."
        continue
    fi

    dest="/awips2/edex/data/utility/common_static/workstation/${workstation}/hydro"
    # echo "DEBUG: dest: ${dest}"
    mkdir --parents ${dest}
    if [[ ! -d "${dest}" ]] ; then
        echo "ERROR: Failed to create localization path [${dest}] for host [${workstation}]."
        continue
    fi

    cmd="cp --verbose /tmp/pdc_loc_shift.txt ${dest}/pdc_loc_shift.txt"
    # echo "DEBUG: copy to localization: ${cmd}"
    ${cmd}
    if [[ $? -ne 0 ]] ; then
        echo "ERROR: Failed to copy file [/tmp/pdc_loc_shift.txt] to [${dest}/pdc_loc_shift.txt]."
        continue
    fi
    
    chmod 750 ${dest}
    chown awips:fxalpha ${dest}
    chmod 640 ${dest}/pdc_loc_shift.txt
    chown awips:fxalpha ${dest}/pdc_loc_shift.txt
done

echo "INFO: Script complete."
