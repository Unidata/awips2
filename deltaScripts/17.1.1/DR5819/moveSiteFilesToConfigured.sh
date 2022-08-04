#!/bin/bash
# This script will move auto-generated localization files that aren't
# overridden from SITE to CONFIGURED.
#
# This update is required with 17.1.1.
#
# This update is only for edex servers which host the localization files.
#

echo "INFO: Moving auto-generated localization files from SITE to CONFIGURED."

IFS=$'\n'
utilityDir=/awips2/edex/data/utility

function moveFiles {
    dirOrFile=$1
    fileType=$2
    errorcode=0

    # Move files from site to configured
    echo "INFO: Moving $dirOrFile from SITE to CONFIGURED."
    siteFiles=$(find $utilityDir/common_static/site/*/$dirOrFile -type f 2>/dev/null)
    if [[ -z $siteFiles ]]; then
        echo "WARN: Cannot move $dirOrFile because it does not exist."
    fi
    for f in $siteFiles; do
        newf=${f/\/site\//\/configured\/}
        if [[ $f == *.md5 ]]; then
            rm -v "$f"
        elif [[ -e $newf ]]; then
            echo "WARN: Cannot move $f because $newf already exists."
        else
            mkdir -vp $(dirname "$newf")
            mv -v "$f" "$newf" || errorcode=1
        fi
    done

    # Set ownership/permissions on configured files
    configuredPath="$utilityDir/common_static/configured/*/$dirOrFile"
    # -9 will go to first dir/file below context directories
    # (e.g. /.../configured/OAX/topDirOrFile)
    topDirOrFile=$(echo "$configuredPath" | cut -d'/' -f'-9')
    topConfiguredFiles=$(find $topDirOrFile -maxdepth 0 2>/dev/null)
    for f in $topConfiguredFiles; do
        # Ensure context directory (e.g. /.../configured/OAX) has correct
        # permissions, since it may have been newly created
        chown awips:fxalpha $(dirname "$f")
        chmod 775 $(dirname "$f")

        chown -R awips:fxalpha "$f"
        find "$f" -type f -exec chmod 664 {} \+
        find "$f" -type d -exec chmod 775 {} \+
    done

    # Cleanup old site directories/files
    if [[ $fileType = dir ]]; then
        find $utilityDir/common_static/site/*/$dirOrFile -type d -empty -delete 2>/dev/null
    else
        find $utilityDir/common_static/site/*/${dirOrFile}.md5 -size 32c -delete 2>/dev/null
    fi

    parentDir=$(dirname $dirOrFile)
    while [[ $parentDir != "." ]]; do
        rmdir $utilityDir/common_static/site/*/$parentDir 2>/dev/null
        parentDir=$(dirname $parentDir)
    done

    return $errorcode
}

errcode=0

dirs=("ffmp/ffti" "ffmp/sources" "ffmp/templates")
for dir in "${dirs[@]}"; do
    moveFiles "$dir" dir || errcode=1
done

if [[ $errcode -ne 0 ]]; then
    echo "ERROR: There were one or more errors when moving files. See above output."
else
    echo "INFO: The update finished successfully."
fi
exit $errcode
