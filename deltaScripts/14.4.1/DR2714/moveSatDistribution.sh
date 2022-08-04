#!/bin/bash

# Omaha #2714 find all satellite distribution files in localization and rename to match plugin

for x in $(find /awips2/edex/data/utility/edex_static -regex '^.*distribution/satellite.xml$')
do
   target=${x/satellite.xml/satellite-gini.xml}
   echo Renaming $x to $target
   mv "$x" "$target"
done
echo Done
