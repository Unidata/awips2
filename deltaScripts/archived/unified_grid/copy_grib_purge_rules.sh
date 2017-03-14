#!/bin/bash
# This script will copy any grib purge rules to a equivalent grid purge rules file
#
# This update needs to be performed with build ???.

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

IFS=$'\n'
files=`find /awips2/edex/data/utility/common_static/site/*/purge/gribPurgeRules.xml`

if [ $? -ne 0 ]; then
echo "No site level grib purge files found!"
exit 0
fi

for f in $files; do
	nf=${f/grib/grid}
    echo Copying $f to $nf
    cp $f $nf
    # level
	sed -n 's/modelInfo\.level\.levelonevalue=/info.level.levelonevalue=/g;p;' -i $nf
	sed -n 's/modelInfo\.level\.leveltwovalue=/info.level.leveltwovalue=/g;p;' -i $nf
	sed -n 's/modelInfo\.level\.masterLevel.name=/info.level.masterLevel.name=/g;p;' -i $nf
	# parameter
	sed -n 's/modelInfo\.parameterAbbreviation=/info.parameter.abbreviation=/g;p;' -i $nf
	# dataset
    sed -n 's/modelInfo\.modelName=/info.datasetId=/g;p;' -i $nf
done



echo "INFO: The update finished successfully."
exit 0
