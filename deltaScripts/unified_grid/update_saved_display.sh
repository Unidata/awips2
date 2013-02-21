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
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)1\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2ctl1\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)2\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2ctl2\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)3\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2n1\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)4\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2p1\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)5\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2n2\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)6\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2p2\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)7\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2n3\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)8\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2p3\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)9\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2n4\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)10\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2p4\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)11\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2n5\3/g;p;}' -i $f
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)modelInfo\.perturbationNumber\("\s*>\s*<\s*constraint\s\+constraintValue="\)12\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.ensembleId\2p5\3/g;p;}' -i $f
	# handle grid version
	sed -n '1h;1!H;${;g;s/\(<mapping\s\+key="\)gridVersion\("\s*>\s*<\s*constraint\s\+constraintValue="\)\([0-9]\{1,2\}\)\("\s\+constraintType="EQUALS"\s*\/>\s*<\/mapping>\)/\1info.secondaryId\2Version\3\4/g;p;}' -i $f
	# level
	sed -n 's/key="modelInfo\.level\.levelonevalue"/key="info.level.levelonevalue"/g;p;' -i $f
	sed -n 's/key="modelInfo\.level\.leveltwovalue"/key="info.level.leveltwovalue"/g;p;' -i $f
	sed -n 's/key="modelInfo\.level\.masterLevel.name"/key="info.level.masterLevel.name"/g;p;' -i $f
	# parameter
	sed -n 's/key="modelInfo\.parameterAbbreviation"/key="info.parameter.abbreviation"/g;p;' -i $f
	# dataset
	sed -n 's/key="modelInfo\.modelName"/key="info.datasetId"/g;p;' -i $f
	#plugin name
	sed -n 's/constraintValue="grib"/constraintValue="grid"/g;p;' -i $f

	#Also need to map the productIdentifierKey in best res resource data.
	sed -n 's/productIdentifierKey="modelInfo\.level\.levelonevalue"/productIdentifierKey="info.level.levelonevalue"/g;p;' -i $f
	sed -n 's/productIdentifierKey="modelInfo\.level\.leveltwovalue"/productIdentifierKey="info.level.leveltwovalue"/g;p;' -i $f
	sed -n 's/productIdentifierKey="modelInfo\.level\.masterLevel.name"/productIdentifierKey="info.level.masterLevel.name"/g;p;' -i $f
	sed -n 's/productIdentifierKey="modelInfo\.modelName"/productIdentifierKey="info.datasetId"/g;p;' -i $f
	sed -n 's/productIdentifierKey="modelInfo\.parameterAbbreviation"/productIdentifierKey="info.parameter.abbreviation"/g;p;' -i $f


	#diff $f $bf > /dev/null
	#if [ $? -eq 0 ]; then rm $bf; echo "No Changes"; fi
done
