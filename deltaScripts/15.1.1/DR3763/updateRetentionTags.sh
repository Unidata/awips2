#!/bin/bash
# This assumes arguments are archiver xml files that need to have the tag minRetentionHours
# changed to defaultRetentionHours and extRetentionHours to selectedRetentionHours.
# If the conversion is not needed the file is left untouched otherwise the original has '.bak' appended
# to its name and it is replaced with the converted file.

for f in $* ; do
  rm -f ${f}.$$
  sed -e 's/minRetentionHours>/defaultRetentionHours>/g' -e 's/extRetentionHours>/selectedRetentionHours>/g' $f > ${f}.$$
  cmp -s $f ${f}.$$
  if [[ $? != 0 ]] ; then
    rm -f ${f}.bak
    mv $f ${f}.bak
    mv ${f}.$$ $f
    chmod 664 $f
    chown awips:awips $f
    echo "converted $f"
  else 
    echo "No conversion needed for $f"
    rm -f ${f}.$$
  fi
done
