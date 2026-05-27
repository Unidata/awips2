#!/bin/bash

# 6372 - Move AbstractMenuUtil menu localization files to common_static.
#
# Author: mapeters
# Aug 21, 2017

# files under menus/ dir that are handled by AbstractMenuUtil 
declare -a menu_files=("ffmp/ffmp.xml"
                       "scan/scan.xml"
                       "fog/baseFog.xml"
                       "safeseas/baseSafeSeas.xml"
                       "snow/baseSnow.xml"
                       "satellite/baseDerivedProductsImagery.xml"
                       "satellite/baseComposite.xml"
                       "upperair/baseRAOB.xml"
                       "upperair/uaMenus.xml"
                       "upperair/.upperairMenuTime"
                       "radar/index.xml"
                       "radar/dialRadars.xml"
                       "radar/airportRadars.xml"
                       "radar/radarindex.xml"
                       "radar/.radarMenuTime"
                       "ncepHydro/cpc/cpcMenus.xml" 
                       "ncepHydro/spc/hazardMenus.xml"
                       "ncepHydro/hpc/hpcMenus.xml"
                       "ncepHydro/mpc/mpcMenus.xml"
                       "ncepHydro/nco/ncoMenus.xml"
                       "ncepHydro/cpc/.ncepHydro/cpcMenuTime"
                       "ncepHydro/spc/.ncepHydro/spcMenuTime"
                       "ncepHydro/hpc/.ncepHydro/hpcMenuTime"
                       "ncepHydro/mpc/.ncepHydro/mpcMenuTime"
                       "ncepHydro/nco/.ncepHydro/ncoMenuTime"
                       )

function get_fs() {
    df -P -- "$1" | awk 'NR==2 {print $1}'
}

did_work=false
echo INFO: Moving AbstractMenuUtil menu localization files to common_static
for menu_file in "${menu_files[@]}"; do
    for old_location in /awips2/edex/data/utility/cave_static/*/*/menus/"${menu_file}"; do
        if [[ ! -e "${old_location}" ]]; then
            continue
        fi
        new_location=${old_location/cave_static/common_static}
        echo INFO: Moving "${old_location}" to "${new_location}"
        did_work=true
        new_location_dir=$(dirname "${new_location}")
        if [[ ! -d "${new_location_dir}" ]]; then
            sudo -u awips mkdir -p -m 750 "${new_location_dir}"
        else
            echo "WARN: ${new_location} already exists. Just copying newer files"
        fi
        if rsync -aux "${old_location}" "${new_location}"; then
            # cut context directory (e.g. /awips2/.../site/OAX) from old_location
            old_location_ctx_dir=$(echo "${old_location}" | cut -d/ -f-8)
            # filesystem that context dir is on
            ctx_fs=$(get_fs "${old_location_ctx_dir}")

            # remove moved file if on same filesystem as context dir
            file_fs=$(get_fs "${old_location}")
            if [[ "${file_fs}" = "${ctx_fs}" ]]; then
                rm -f "${old_location}" "${old_location}".md5
            fi

            old_location_dir=$(dirname "${old_location}")
            # remove empty directories up until the context dir
            while [[ "${old_location_ctx_dir}" != "${old_location_dir}" ]]; do
                # filesystem that dir is on
                dir_fs=$(get_fs "${old_location_dir}")
                if [[ "${dir_fs}" = "${ctx_fs}" ]]; then
                    # remove empty directory if on same filesystem
                    # stop moving up directory tree if we find non-empty directory
                    rmdir "${old_location_dir}" 2>/dev/null || break
                fi
                old_location_dir=$(dirname "${old_location_dir}")
            done
        fi
        rm -f "${new_location}".md5
        echo INFO: Done moving "${old_location}" to "${new_location}"
    done
done

if [[ "${did_work}" == "false" ]]; then
    echo INFO: There are no files to move. Did nothing
else
    echo INFO: Done moving localization files
fi
