#!/bin/bash

#########################################################################
# NOTE: THIS IS NOT A DELTA SCRIPT THAT WILL BE RUN WITH A FRESH AWIPS
# INSTALLATION. IT WILL BE RUN AS PART OF A NATIONAL ACTIVATION OF
# HAZSIMP FOR NON-PRECIPITATION WEATHER (NPW) HAZARDS.
#
# Script: gfeHazSimpHeatTurnkeyInstall.sh
# Machine to be run on: dv3
# User account to run this script: awips
# Sudo required?: No
#
# Description: There is a hazard simplification effort surrounding
# Non-Precipitation Weather (NPW) heat hazards. This script activates
# the HazSimp heat components inside GFE.
#
# The actual file pushing is performed by pushTurnKeyFiles.py inside
# the same folder as this script.
#
# Running this script will perform the following:
#
# a. Overwrite the BASE-level gfeConfig.py and MakeHazardConfig.py files
# b. Scan all overrides of gfeConfig.py and MakeHazardConfig.py and alert
#     the user of manual changes to be made
#
# Summary of Changes:
#   - Replace EH.A --> XH.A and EH.W --> XH.W in gfeConfig.py
#   - Replace EH.A --> XH.A and EH.W --> XH.W in the "Non-Precipitation"
#     category in MakeHazardConfig.py
#########################################################################

# Verify the user is awips
user=$(whoami)
if [ "$user" != "awips" ]
then
    echo "ERROR: Script must be run as the user 'awips'."
    exit 1
fi

# Verify the setup.env file exists
if [ ! -f "/awips2/edex/bin/setup.env" ]
then
        echo "ERROR: /awips2/edex/bin/setup.env not found. Re-run on dv3."
        exit 1
fi

# Source setup.env to get the $AW_SITE_IDENTIFIER
source /awips2/edex/bin/setup.env
if [ -z ${AW_SITE_IDENTIFIER} ]
then
        echo "ERROR: AW_SITE_IDENTIFIER not set"
        exit 1
fi

# Perform the turnkey operations
python pushTurnKeyFiles.py -s ${AW_SITE_IDENTIFIER}

echo "INFO: GFE NPW HazSimp turnkey script for DR 2036917 complete"
