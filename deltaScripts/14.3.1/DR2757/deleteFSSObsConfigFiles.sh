#!/bin/bash

echo "INFO: delete FSSObs config files - removing safeseas, snow and fog area configuration files."
echo "Replace site AKQ with current one."

cd /awips2/edex/data/utility/common_static/site/AKQ 
rm -rf fog safeseas snow

echo "INFO: the update has completed successfully!"

exit 0
