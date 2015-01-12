#!/bin/bash
# This script will update any FFMPSourceConfig.xml files
# to use grid data in place of grib
#
# This update needs to be performed with build ???.
# This update is only for edex servers which host FFMPSourceConfig.xml files

echo ""
echo "Press Enter to undo the updates Ctrl-C to quit."
read done

files=`find /awips2/edex/data/utility/common_static -iname FFMPSourceConfig.xml`

if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

for f in $files; do
	echo Updating $f
	bf=$f.bak.`date +%m%d%y`
	cp $f $bf
	# reconstruct data uris from grib to grid
	awk -F '/' '
	/<dataPath>\/grid/ {print $1 "/grib/" $3 "/" $4 "/" $8 "/" $9 "/" $10 "/" $11 "/null/null/" $12 "/" $13; next;}
	{gsub(/<plugin>grid<\/plugin>/,"<plugin>grib</plugin>"); print; }
	' $bf > $f
done



echo "INFO: The update was successfully removed."
exit 0
