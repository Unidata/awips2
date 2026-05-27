#!/bin/sh
# This script removes temporary Ref*.xml edit areas from user level directories 
#    /awips2/edex/data/utility/common_static/user/*/gfe/editAreas/
#
# If this script has errors it can be re-run once any issues are corrected
# This script SHOULD NOT be run while forecasters are running GFE formatters
#
rm /awips2/edex/data/utility/common_static/user/*/gfe/editAreas/Ref*.xml*