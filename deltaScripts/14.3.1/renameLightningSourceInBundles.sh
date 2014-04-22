#!/bin/bash

# This script will rename the lightning source column in any D2D bundle files
# This update is only for edex servers which host the cave localization files

MY_DIR=`dirname $0`
bash $MY_DIR/utility/updateLightningNameInXML.sh -b
exit 0

