#!/bin/bash
# This script will update any *-harvester.xml files
# to use grid data in place of grib
#
# This update needs to be performed with build 13.5.1.
# This update is only for edex servers which host *-harvester.xml files

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

files=`find /awips2/edex/data/utility/common_static -iname \*-harvester.xml`

if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

for f in $files; do
        echo Updating $f
        bf=$f.bak.`date +%m%d%y`
        cp $f $bf
        # remove the registry backup tags
        awk -F '/' ' { gsub(/<primaryRegistryHost>127.0.0.1<\/primaryRegistryHost>/,""); 
                       gsub(/<secondaryRegistryHost>127.0.0.1<\/secondaryRegistryHost>/,""); 
                       gsub(/<tertiaryRegistryHost>127.0.0.1<\/tertiaryRegistryHost>/,""); 
                       print; } ' $bf > $f 
 
done



echo "INFO: The update finished successfully."
exit 0
