#!/bin/bash
# This script will move any non-base localization files from edex_static to
# common_static for the following plugins:
#
#   com.raytheon.edex.plugin.binlightning
#   com.raytheon.edex.plugin.bufrua
#   com.raytheon.edex.plugin.gfe
#   com.raytheon.edex.plugin.ldad
#   com.raytheon.edex.plugin.radar
#   com.raytheon.edex.plugin.satellite
#   com.raytheon.edex.plugin.shef
#   com.raytheon.edex.rpgenvdata
#   com.raytheon.edex.utilitysrv
#   com.raytheon.uf.edex.datadelivery.registry
#   com.raytheon.uf.edex.datadelivery.retrieval
#   com.raytheon.uf.edex.ndm.dataplugin
#   com.raytheon.uf.edex.plugin.activetable
#   com.raytheon.uf.edex.plugin.bufrascat
#   com.raytheon.uf.edex.plugin.bufrobs
#   com.raytheon.uf.edex.plugin.bufrssmi
#   com.raytheon.uf.edex.plugin.cwa
#   com.raytheon.uf.edex.plugin.cwat
#   com.raytheon.uf.edex.plugin.goesr.dmw
#   com.raytheon.uf.edex.plugin.grid.netcdf
#   com.raytheon.uf.edex.plugin.loctables
#   com.raytheon.uf.edex.plugin.modelsounding
#   com.raytheon.uf.edex.plugin.npp.viirs
#   com.raytheon.uf.edex.plugin.qc (partially)
#   com.raytheon.uf.edex.plugin.redbook
#   com.raytheon.uf.edex.plugin.satellite.gini
#   com.raytheon.uf.edex.plugin.satellite.mcidas
#   com.raytheon.uf.edex.plugin.scan
#   com.raytheon.uf.edex.plugin.text.subscription
#   com.raytheon.uf.edex.pointdata
#   com.raytheon.uf.edex.registry.ebxml
#   com.raytheon.uf.edex.site
#
# This update is required with 17.1.1.
#
# This update is only for edex servers which host the localization files
#

echo "INFO: Moving localization files to common_static."

IFS=$'\n'

function moveFiles {
    type=$1
    dirOrFile=$2
    fileType=$3
    destDir=$4
    errorcode=0

    echo "INFO: Moving $dirOrFile from $type to common_static."
    filesToMove=$(find /awips2/edex/data/utility/$type/*/*/$dirOrFile -type f 2>/dev/null)
    if [[ "$filesToMove" = "" ]]; then
        echo "WARN: Cannot move $dirOrFile because it does not exist."
    fi
    for f in $filesToMove; do
        newf=${f/$type/common_static}
        if [[ -n "$destDir" && "$fileType" = "dir" ]]; then
            newf=${newf/$dirOrFile/$destDir}
        fi
        if [[ $f == *.md5 ]]; then
            rm -v "$f"
        elif [ -e "$newf" ]; then
            echo "WARN: Cannot move $f because $newf already exists"
        else
            mkdir -vp $(dirname $newf)
            mv -v "$f" "$newf" || errorcode=1
        fi
    done

    # Set ownership/permissions on common_static files
    commonFilesPath="/awips2/edex/data/utility/common_static/*/*/$dirOrFile"
    if [[ -n "$destDir" && "$fileType" = "dir" ]]; then
        commonFilesPath=${commonFilesPath/$dirOrFile/$destDir}
    fi
    # -9 will go to first dir/file below context directories
    # (e.g. /.../site/OAX/topDirOrFile)
    topDirOrFile=$(echo "$commonFilesPath" | cut -d'/' -f'-9')
    topCommonFiles=$(find $topDirOrFile -maxdepth 0 2>/dev/null)
    for f in $topCommonFiles; do
        # Ensure context directory (e.g. /.../site/OAX) has correct
        # permissions, since it may have been newly created
        chown awips:fxalpha $(dirname "$f")
        chmod 775 $(dirname "$f")

        setOwnershipAndPerms "$f"
    done

    if [[ $fileType = dir ]]; then
        find /awips2/edex/data/utility/$type/*/*/$dirOrFile -type d -empty -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/base/$dirOrFile -iname "*.md5" -size 32c -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/base/$dirOrFile -iname "*.pyc" -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/base/$dirOrFile -iname "*.pyo" -delete 2>/dev/null
                find /awips2/edex/data/utility/$type/base/$dirOrFile -type d -empty -delete 2>/dev/null
    else
        find /awips2/edex/data/utility/$type/*/*/${dirOrFile}.md5 -size 32c -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/base/${dirOrFile}.md5 -size 32c -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/*/*/${dirOrFile}.pyc -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/base/${dirOrFile}.pyc -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/*/*/${dirOrFile}.pyo -delete 2>/dev/null
        find /awips2/edex/data/utility/$type/base/${dirOrFile}.pyo -delete 2>/dev/null
        fi

    parentDir=$(dirname $dirOrFile)
    while [[ $parentDir != "." ]]; do
        rmdir /awips2/edex/data/utility/$type/*/*/$parentDir 2>/dev/null
        rmdir /awips2/edex/data/utility/$type/base/$parentDir 2>/dev/null
        parentDir=$(dirname $parentDir)
    done

    return $errorcode
}

function moveActiveTableBackups {
    errorcode=0

    echo "INFO: Moving active table backups to /awips2/edex/data/activetable/."
    commonFiles=$(find /awips2/edex/data/utility/edex_static/site/*/vtec/backup -type f 2>/dev/null)
    if [[ "$commonFiles" = "" ]]; then
        echo "WARN: No active table backups found."
    fi
    for f in $commonFiles; do
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

    return $errorcode
}

function setOwnershipAndPerms {
    chown -R awips:fxalpha "$1"
    find "$1" -type f -exec chmod 664 {} \+
    find "$1" -type d -exec chmod 775 {} \+
}

errcode=0

dirs=("binlightning" "bufrua" "ldad" "menuTemplate/satellite" "bufrobs" "cwa" "netcdf" \
      "viirs" "redbook" "satellite/gini" "scan" "adaptivePlots" "modelsounding" \
      "spatialTables" "satellite/mcidas" "util" "binlightning/filters" "cwat/locations" \
      "rpgenvdata" "ebxml" "plugin-filters" "dissemination" "infofiles" "registry/versions" \
      "mapping")
for dir in "${dirs[@]}"
do
    moveFiles edex_static $dir dir || errcode=1
done

moveFiles edex_static "smartinit" dir "gfe/smartinit" || errcode=1
moveFiles edex_static "config/gfe" dir "gfe/config" || errcode=1
moveFiles edex_static "textproducts" dir "gfe/textproducts" || errcode=1
moveFiles edex_static "gfe" dir "gfe/python" || errcode=1
moveFiles edex_static "python/gfe" dir "gfe/python" || errcode=1

moveActiveTableBackups || errcode=1

files=("protectedFiles.txt" "python/qcNetCDF.py" "config/activeSites.txt" \
       "ndm/fsl-w88d.shp" "ndm/fsl-w88d.shx" "ndm/fsl-w88d.dbf" "vtec/remote-etn-partners.properties")
for file in "${files[@]}"
do
    moveFiles edex_static $file file || errcode=1
done

if [[ "$errcode" -ne 0 ]]; then
    echo "ERROR: There were one or more errors when moving files. See above output."
else
    echo "INFO: The update finished successfully."
fi
exit $errcode
