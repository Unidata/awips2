#!/bin/bash

# Move offline stat files from localize location to /awips2/edex/data/stats

. /awips2/edex/bin/setup.env

oldStatDir=/awips2/edex/data/utility/common_static/configured/${AW_SITE_IDENTIFIER}/stats
newStatParent=/awips2/edex/data

if  [ ! -d ${oldStatDir} ] ; then
	echo "ERROR: ${oldStatDir} dirctory does not exist"
	echo "FATAL: The update has failed."
	exit 1
fi

if [ ! -d ${newStatParent} ] ; then
	rm -rf ${newStatParent}
	mkdir -p ${newStatParent}
	if [ $? -ne 0 ] ; then
		echo "ERROR: Unable to create ${newStatParent}"
		echo "FATAL: The update has failed"
		exit 1
	fi
fi

cp -R -p ${oldStatDir} ${newStatParent}

if [ $? -ne 0 ] ; then
	echo "ERROR: copying ${oldStatDir} to ${newStatParent} failed."
	echo "FATAL: The update has failed."
	exit 1
fi

rm -rf ${oldStatDir}

if [ $? -ne 0 ] ; then
	echo "WARNING: Deleting ${oldStatDir} failed"
fi

echo "INFO: ${newStatParent}/stats updated from ${oldStatDir}"
