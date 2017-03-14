#!/bin/bash
# This script will update any saved displays from the grib format to the grid format
#
# This update needs to be performed with build ???.
# This update only needs to be run if there are saved displays being stored outside of localization.


if [ $# -eq 0 ]; then
    echo "Please provide a list of saved displays to update."
fi

IFS=$'\n'

for f in "$@"; do
	echo Updating $f
	#bf=$f.bak.`date +%m%d%y`
	#cp $f $bf
	# its probably not efficient to execute sed 20 times but its not slow...
	# replace perturbationNumbers with ensmble ids
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)ctl1\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\21\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)ctl2\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\22\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)n1\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\23\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)p1\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\24\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)n2\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\25\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)p2\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\26\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)n3\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\27\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)p3\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\28\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)n4\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\29\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)p4\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\210\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)n5\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\211\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.ensembleId\("\s*>\s*<\s*constraint\s\+constraintValue="\)p5\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1modelInfo.perturbationNumber\212\3/g;p;}' -i $f
	# handle grid version
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)info\.secondaryId\("\s*>\s*<\s*constraint\s\+constraintValue="\)Version\([0-9]\{1,2\}\)\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1gridVersion\2\3\4/g;p;}' -i $f
	# level
	sed -n 's/key="info\.level\.levelonevalue"/key="modelInfo.level.levelonevalue"/g;p;' -i $f
	sed -n 's/key="info\.level\.leveltwovalue"/key="modelInfo.level.leveltwovalue"/g;p;' -i $f
	sed -n 's/key="info\.level\.masterLevel.name"/key="modelInfo.level.masterLevel.name"/g;p;' -i $f
	# parameter
	sed -n 's/key="info\.parameter.abbreviation"/key="modelInfo.parameterAbbreviation"/g;p;' -i $f
	# dataset
	sed -n 's/key="info\.datasetId"/key="modelInfo.modelName"/g;p;' -i $f
	#plugin name
	sed -n 's/constraintValue="grid"/constraintValue="grib"/g;p;' -i $f
	#diff $f $bf > /dev/null
	#if [ $? -eq 0 ]; then rm $bf; echo "No Changes"; fi
done
