#!/bin/bash

# This script calls _upgrade_postgis.sh to complete the PostGIS upgrade.
#
# Do not Ctrl+C or otherwise interrupt the script while it is running as this
# may prevent proper cleanup.
#
# Author: tgurney

if [[ "$(id -u)" -ne 0 ]]; then
    echo "$(basename $0): need to be root."
    exit 1
fi

ts=$(date +%F_%H%M%S)
logdir=/data/fxa/INSTALL/a2logs/21.4.1/postgis_upgrade-${ts}
mkdir -p "${logdir}" || exit 1
chown -R awips:fxalpha "${logdir}"
chmod 2775 "${logdir}"
echo "INFO: PostGIS upgrade logs will be saved to ${logdir}"
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $logdir
sudo -nu awips bash "${scriptdir}"/_upgrade_postgis.sh "${scriptdir}" | sudo -nu awips tee ./postgis_upgrade_$(date +%F_%H%M%S).log
