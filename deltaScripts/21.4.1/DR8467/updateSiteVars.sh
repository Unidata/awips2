#!/bin/bash
#
# This delta script updates the PRODUCTdir and LDAD_DATA paths in the 
# /awips2/GFESuite/hti/etc/sitevars.xxx file for the sites 
#
# This delta script should be run from dv3 
#
echo "Running delta script ${0} for RODO DR 8467"
for sitevars in /awips2/GFESuite/hti/etc/sitevars.*
do
    # skip the baseline sitevars.ccc file
    if [ "$(basename ${sitevars})" != "sitevars.ccc" ]
    then
        sed -i -e 's#PRODUCTdir="${HTI_HOME}/data"#PRODUCTdir="${HTI_HOME}/data/${site}"#; s#LDAD_DATA="/data/ldad/hti"#LDAD_DATA="/data/ldad/hti/${site}"#' ${sitevars}
    fi
done
echo "Delta script ${0} complete"
