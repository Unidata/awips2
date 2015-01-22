#!/bin/bash
# This searches for archiver xml files in common_static and sends them to updateRetentionTags.sh
# to update retention hour tags.
#
# The base files should be updated when upgraded to 15.1.1 but this will handle any that are added.

COMMON=/awips2/edex/data/utility/common_static
DIR=`dirname $0`

echo "+++ checking base +++"
$DIR/updateRetentionTags.sh ${COMMON}/base/archiver/purger/*.xml

for dir in `ls ${COMMON}` ; do
    if [[ "$dir" != "base" && "$dir" != "configured" && -d "${COMMON}/$dir" ]] ; then
      echo "+++ checking $dir +++"
      for d in `ls ${COMMON}/$dir/` ; do
        pDir="${COMMON}/$dir/$d/archiver/purger"
        if [[ -d "$pDir" ]] ; then
          for f in `ls $pDir` ; do
            if [[ "${f##*.}" == "xml" ]] ; then
              $DIR/updateRetentionTags.sh $pDir/$f
            fi
          done
        fi
      done
    fi
done
