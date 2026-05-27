#!/bin/bash
#
# This script must be run BEFORE the 17.1.1 RPM upgrade.
# 
# This script will move site-specific edex_static.base localization files to
# common_static.configured or common_static.site.
#
# This update is required with 17.1.1.
#
# This update is only for edex servers which host the localization files.
#

function setOwnershipAndPerms {
    chown -R awips:fxalpha "$1"
    find "$1" -type f -exec chmod 664 {} \+
    find "$1" -type d -exec chmod 775 {} \+
}

echo "INFO: Moving site-specific localization files from edex_static.base to"\
     "common_static.site or common_static.configured"

IFS=$'\n'
utilityDir=/awips2/edex/data/utility
errcode=0
deletePrioritiesFile=0
deleteMarineInfoFile=0

# Copy awipsPriorities*.txt and MarineInfo.txt from edex_static.base to all
# common_static.configured dirs, unless they already exist in a given configured dir
configDirs=$(find $utilityDir/common_static/configured/* -maxdepth 0 2>/dev/null)
for dir in $configDirs; do
    if [[ -e $utilityDir/edex_static/base/dissemination/awipsPriorities.txt ]]; then
        if [[ -e "$dir"/dissemination/awipsPriorities.txt ]]; then
            echo "WARN: Cannot copy $utilityDir/edex_static/base/dissemination/awipsPriorities.txt"\
                 "to $dir/dissemination/awipsPriorities.txt because the destination file aready exists"
            deletePrioritiesFile=1
        else
            mkdir -vp "$dir"/dissemination
            cp -v $utilityDir/edex_static/base/dissemination/awipsPriorities*.txt "$dir"/dissemination/ || errcode=1
        fi
    fi

    # MarineInfo.txt is saved to infofiles/ when NDM file is dropped in, move
    # to infofiles/marinesites/ to match BASE file
    if [[ -e $utilityDir/edex_static/base/infofiles/MarineInfo.txt ]]; then
        if [[ -e "$dir"/infofiles/marinesites/MarineInfo.txt ]]; then
            echo "WARN: Cannot copy $utilityDir/edex_static/base/infofiles/MarineInfo.txt"\
                 "to $dir/infofiles/marinesites/MarineInfo.txt because the destination file aready exists"
            deleteMarineInfoFile=1
        else
            mkdir -vp "$dir"/infofiles/marinesites
            cp -v $utilityDir/edex_static/base/infofiles/MarineInfo.txt "$dir"/infofiles/marinesites/ || errcode=1
        fi
    fi
done

# If the files were successfully copied to all dirs, delete them from edex_static
if [[ deletePrioritiesFile -eq 0 ]]; then
    rm -v $utilityDir/edex_static/base/dissemination/awipsPriorities*.txt 2>/dev/null
    rm -v $utilityDir/edex_static/base/dissemination/awipsPriorities*.txt.md5 2>/dev/null
else
    echo "WARN: $utilityDir/edex_static/base/dissemination/awipsPriorities.txt"\
         "will not be deleted since it was not copied to all configured directories (see above output)"
fi
if [[ deleteMarineInfoFile -eq 0 ]]; then
    rm -v $utilityDir/edex_static/base/infofiles/MarineInfo.txt 2>/dev/null
    rm -v $utilityDir/edex_static/base/infofiles/MarineInfo.txt.md5 2>/dev/null
else
    echo "WARN: $utilityDir/edex_static/base/infofiles/MarineInfo.txt"\
         "will not be deleted since it was not copied to all configured directories (see above output)"
fi

# Move WAN_exclude_${site}.txt and NWWS_exclude_${site}.txt to common_static.configured.${site}
excludeFiles=$(find $utilityDir/edex_static/base/dissemination/* -maxdepth 0 -name "WAN_exclude_*.txt" -o -name "NWWS_exclude_*.txt" 2>/dev/null)
for file in $excludeFiles; do
    fileName="$(basename "$file")"
    siteId="$(echo "$fileName" | cut -d'_' -f3 | cut -d'.' -f1)"
    if [[ -e $utilityDir/common_static/site/"$siteId"/dissemination/$fileName ]]; then
        echo "WARN: Cannot move $file to $utilityDir/common_static/site/"$siteId"/dissemination/$fileName"\
             "because the destination file aready exists"
    else
        mkdir -vp $utilityDir/common_static/site/"$siteId"/dissemination
        mv -v "$file" $utilityDir/common_static/site/"$siteId"/dissemination || errcode=1
        rm -v "$file".md5 2>/dev/null
    fi
done

# Set ownership/permissions on all created dirs/files
destDirs=$(find $utilityDir/common_static/configured/*/dissemination \
                $utilityDir/common_static/configured/*/infofiles \
                $utilityDir/common_static/site/*/dissemination \
                -maxdepth 0 2>/dev/null)
for dir in $destDirs; do
    # Ensure context directory (e.g. /.../configured/OAX) has correct
    # permissions, since it may have been newly created
    chown awips:fxalpha $(dirname "$dir")
    chmod 775 $(dirname "$dir")

    setOwnershipAndPerms "$dir"
done

if [[ $errcode -ne 0 ]]; then
    echo "ERROR: There were one or more errors when moving files. See above output."
else
    echo "INFO: The update finished successfully."
fi
exit $errcode
