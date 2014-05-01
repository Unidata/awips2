#!/bin/sh

# $1 == directory to run against
# $2 == TO number
# $3 == Build number

# Searches all subfolders in the the passed in directory for any MANIFEST.MF that is within a directory that starts with "com.raytheon"
# For each MANIFEST.MF found it replaces the version with the TO and Build number in the format
# Bundle-Version: 1.TO.Build.qualifier
#
# Example
# Bundle-Version: 1.10.4.qualifier
#
# Note: qualifier will be replaced in the build with the date.

function print_usage {
        echo "usage: ./updateCaveManifestFiles.sh directory TO Build"
        echo "example using TO10 Build 5: ./updateCaveManifestFiles.sh /home/user/workspace 10 5"
        exit
}
readonly -f print_usage
declare -t print_usage

if [ "$1" = "" -o "$2" = "" -o "$3" = "" ]
then
	print_usage
fi

echo "============================================================"
echo "Updating each MANIFEST.MF that is located in a subfolder that starts with com.raytheon with version 1.${2}.${3}"
echo "============================================================"
echo "***Be sure to Refresh, then check in all changes when the script is done***"
echo ""
echo "Looking for MANIFEST.MF files in all subdirectories of the directory: ${1}"

cd $1
# example find with exclusion
# export LIST=`find . -name "MANIFEST.MF" | grep 'com.raytheon' | grep -v './build.edex/opt'`
export LIST=`find . -name "MANIFEST.MF" | grep 'com.raytheon'`
for mf in $LIST
do
	echo "Updating file: ${mf}"
	perl -p -i -e "s!Bundle-Version: .*\Z!Bundle-Version: 1.${2}.${3}.qualifier!g" ${mf}
done
