#!/bin/bash
# This script will move any activetable backups stored in localization to the
# new backup location at: /awips2/edex/data/activetable/${SITEID}/backup/.
#
# For completeness sake, it will check both edex_static and common_static
# locations.
#
# This update is required with 17.1.1.
#
# This update is only for edex servers which host the localization files
#

echo "INFO: Moving activetable backups from localization to /awips2/edex/data/activetable/."

function moveActiveTableBackups {
    errorcode=0

    echo "INFO: Moving active table backups to /awips2/edex/data/activetable/."
    edexFiles=$(find /awips2/edex/data/utility/edex_static/site/*/vtec/backup -type f 2>/dev/null)
    commonFiles=$(find /awips2/edex/data/utility/common_static/site/*/vtec/backup -type f 2>/dev/null)
    files="$edexFiles $commonFiles"
    if [[ "$files" = "" ]]; then
        echo "WARN: No active table backups found."
    fi
    for f in $files; do
        local fileName=$(basename "$f")
        local siteId=$(basename $(dirname $(dirname $(dirname "$f"))))
        local newf="/awips2/edex/data/activetable/$siteId/backup/$fileName"
        if [[ $f == *.md5 ]]; then
            rm -v "$f"
        elif [ -e "$newf" ]; then
            echo "WARN: Cannot move $f because $newf already exists"
        else
            mkdir -vp $(dirname "$newf")
            mv -v "$f" "$newf" || errorcode=1
        fi
    done

    setOwnershipAndPerms /awips2/edex/data/activetable

    find /awips2/edex/data/utility/edex_static/site/*/vtec/backup -type d -empty -delete 2>/dev/null
    find /awips2/edex/data/utility/common_static/site/*/vtec/backup -type d -empty -delete 2>/dev/null

    return $errorcode
}

function setOwnershipAndPerms {
    chown -R awips:fxalpha "$1"
    find "$1" -type f -exec chmod 664 {} \+
    find "$1" -type d -exec chmod 775 {} \+
}

errcode=0

moveActiveTableBackups || errcode=1

if [[ "$errcode" -ne 0 ]]; then
    echo "ERROR: There were one or more errors when moving files. See above output."
else
    echo "INFO: The update finished successfully."
fi
exit $errcode
