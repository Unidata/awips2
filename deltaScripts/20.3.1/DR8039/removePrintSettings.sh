#! /bin/bash
# This script should be run on dv3
# It removes obsolete printSettings files
echo "Running delta script ${0} for RODO DR 8039"
rm /awips2/edex/data/utility/common_static/user/*/printSettings
rm /awips2/edex/data/utility/common_static/user/*/printSettings.md5
echo "Delta script ${0} complete"
