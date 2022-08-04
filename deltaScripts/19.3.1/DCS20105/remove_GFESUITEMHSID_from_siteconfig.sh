#!/bin/bash
# DCS #20105 - This script removes GFESUITE_MHSID from siteConfig.py files

FILES="/awips2/edex/data/utility/common_static/site/*/gfe/config/siteConfig.py"
for f in $FILES
do
        sed -i '/GFESUITE_MHSID/d' $f
        echo "Removed GFESUITE_MHSID from $f"
done
