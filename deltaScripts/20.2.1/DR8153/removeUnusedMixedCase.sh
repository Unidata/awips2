#!/bin/bash
#
# This script removes all common_static/site/*/mixedCaseProductIds.txt 
# files except the one for the AW_SITE_IDENTIFIER site.
#
# This delta script should be run on dx3-6 as user awips or root
#
user=$(whoami)
if [ "$user" != "awips" ] && [ "$user" != "root" ]
then
    echo "ERROR: Script must be run as the user 'awips' or 'root'." 
    exit 1
fi

host=$(hostname)
if ! [[ "$host" =~ ^dx[3-6](-.*)?$ ]]
then
    echo "ERROR: Script must be run on host dx3-6." 
    exit 1
fi

echo "INFO: Running delta script for DR 8153"
source /awips2/edex/bin/setup.env
if [ -z ${AW_SITE_IDENTIFIER} ]
then
	echo "ERROR: AW_SITE_IDENTIFIER not set"
	exit 1
fi
for x in /awips2/edex/data/utility/common_static/site/*/mixedCase/mixedCaseProductIds.txt*
do
	if [[ "$x" != "/awips2/edex/data/utility/common_static/site/${AW_SITE_IDENTIFIER}/mixedCase/mixedCaseProductIds.txt"* ]]
	then
		rm "$x"
	fi
done
echo "INFO: Delta script for DR 8153 complete"