#!/bin/bash
# This script will remove any configured level Redbook
# menus per requirements in DR 21560.
#

echo "INFO: Removing the following configured-level Redbook menus per DR 21560 requirements"
echo

find /awips2/edex/data/utility/common_static/configured/*/menus/ -name '*Menus*'

sleep 5

find /awips2/edex/data/utility/common_static/configured/*/menus/ -name '*Menus*' -exec rm -f {} \;

echo
echo "INFO: Confirming all menus have been removed (there should be nothing returned here):"
echo

find /awips2/edex/data/utility/common_static/configured/*/menus/ -name '*Menus*'

echo "INFO: The update is finished"
