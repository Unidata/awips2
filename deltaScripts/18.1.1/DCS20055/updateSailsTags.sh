#!/bin/bash
# This searches for RadarUpperText xml files in common_static and updates supplemental 
# elevation display tags from sails to productDependentRepElev.
#
# The base files should be updated when upgraded to 18.1.1.

COMMON=/awips2/edex/data/utility/common_static
DIR=`dirname $0`

for dir in `ls ${COMMON}` ; do
    if [[ "$dir" != "base" && "$dir" != "configured" && -d "${COMMON}/$dir" ]] ; then
      echo "+++ checking $dir +++"

      for d in `ls ${COMMON}/$dir/` ; do
        pDir="${COMMON}/$dir/$d/styleRules"

        if [[ -d "$pDir" ]] ; then
          if [[ -f "$pDir/RadarUpperText.xml" ]] ; then
            rm $pDir/*.tmp
            sed -e 's/<sails/<productDependentRepElev/g' $pDir/RadarUpperText.xml > $pDir/RadarUpperText.tmp
            cmp -s $pDir/RadarUpperText.xml $pDir/RadarUpperText.tmp
            if [[ $? != 0 ]] ; then
              rm -f $pDir/RadarUpperText.bak
              mv $pDir/RadarUpperText.xml $pDir/RadarUpperText.bak
              mv $pDir/RadarUpperText.tmp $pDir/RadarUpperText.xml
              chmod 664 $pDir/RadarUpperText.xml
              chown awips:fxalpha $pDir/RadarUpperText.xml
              echo "converted $pDir/RadarUpperText.xml"
            else 
              echo "No conversion needed for $pDir/RadarUpperText.xml"
              rm -f $pDirRadarUpperText.tmp
            fi
          fi
        fi
      done

    fi
done
