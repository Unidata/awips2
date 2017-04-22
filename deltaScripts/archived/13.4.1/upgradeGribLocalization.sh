#!/bin/bash
# This script will update any gribParamInfo files to use the gridParamInfo tag
# and move gribModels files from common_static to edex_static and remove 
# deprecated fields.
#
# This update is optional with 13.4.1 but it must be performed before grib can
# move to the futue.
#
# This update is only for edex servers which host the cave localization files

echo "INFO: Moving all parameterInfo files to grid."

IFS=$'\n'
gribFiles=`find /awips2/edex/data/utility/edex_static/*/*/grib/parameterInfo/ -iname '*.xml'`

for f in $gribFiles; do
    newf=${f//grib/grid}
    if [ -e "$newf" ]; then
        echo Cannot upgrade $f because $newf already exists
    else
    	mkdir -p `dirname $newf`
        mv $f $newf
    fi
done

echo "INFO: Upgrading all parameterInfo files."

gridFiles=`find /awips2/edex/data/utility/edex_static/*/*/grid/parameterInfo/ -iname '*.xml'`

for f in $gridFiles; do
    sed -n 's/gribParamInfo/gridParamInfo/g;p;' -i $f
done

echo "INFO: Moving all gribModels to edex_static."

commonFiles=`find /awips2/edex/data/utility/common_static/*/*/grib/models/ -iname '*.xml'`

for f in $commonFiles; do
    newf=${f//common_static/edex_static}
    if [ -e "$newf" ]; then
        echo Cannot upgrade $f because $newf already exists
    else
    	mkdir -p `dirname $newf`
        mv $f $newf
    fi
done

echo "INFO: Cleaning all gribModel files."

edexFiles=`find /awips2/edex/data/utility/edex_static/*/*/grib/models/ -iname '*.xml'`

for f in $edexFiles; do
    sed '/^\s*<title>.*<\/title>\s*$/d' -i $f
    sed '/^\s*<alias>.*<\/alias>\s*$/d' -i $f
    sed '/^\s*<dt>.*<\/dt>\s*$/d' -i $f
    sed '/^\s*<paramInfo>.*<\/paramInfo>\s*$/d' -i $f
done

echo "INFO: The update finished successfully."
exit 0