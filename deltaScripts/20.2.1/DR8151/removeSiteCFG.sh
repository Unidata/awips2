#!/bin/bash
#
# This script removes all common_static/configured/*/siteCFG.py files 
# Only the AW_SITE_IDENTIFIER version needs to be present.
# It will be regenerated upon the first GFE site activation at EDEX startup.
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

echo "INFO: Running delta script for DR 8151"
rm /awips2/edex/data/utility/common_static/configured/*/gfe/python/SiteCFG.py*
echo "INFO: Delta script for DR 8151 complete"