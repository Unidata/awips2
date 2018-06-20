#!/bin/sh
# sed/changeword
# changes one model to another model for SCANRunSiteConfig.xml files
#

files=$(find /awips2/edex/data/utility/common_static | grep SCANRunSiteConfig.xml)
echo "Updating all SCANRunSiteConfig.xml files to use HRRR instead of RAP13."

old=RAP13
new=HRRR

for f in $files
do
echo "Processing file: " $f
  if test -f "$f"
  then
     sed "s/$old/$new/g" $f > $f.new
     mv $f $f.orig
     mv $f.new $f
     rm $f.orig
     echo $f done
  fi
done
