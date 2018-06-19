#!/bin/bash 
# DR #4515 - remove the <xmit> tags from aviationForecasterConfig.xml files.

cave_static=/awips2/edex/data/utility/cave_static

file_name=aviation/avnwatch/aviationForecasterConfig.xml

function updateFile {
	grep -sq '<xmit>' $1
	if [ $? == 0 ] ; then 
		echo "Updating $1"
		# this sed script combines all lines in order to pick up
		# <xmit>...</xmit> pair placed on multiple lines.
		sed -i-old -e ':loop
$!{
N
/\n$/!b loop
}
s/<xmit>[^<]*<\/xmit>//g
s/\n\s*\n/\n/g' ${1}
	fi
}

function renameFile {
	echo "Renaming $1"
	rm -f ${1}-bad
	mv $1 ${1}-bad
}

echo "INFO: removing <xmit> tags from aviationForecasterConfig.xml files."

for d in region site ;
do
	echo Checking $d
	for f in `ls ${cave_static}/${d}/*/$file_name 2> /dev/null` ;
	do
			updateFile $f
	done
	echo "Done $d"
done

for d in user workstation ;
do
	echo "Checking $d"
	for f in `ls ${cave_static}/${d}/*/$file_name 2> /dev/null` ;
	do
		renameFile $f
	done
	echo "Done $d"
done

echo "INFO: Done."