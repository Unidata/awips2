#!/bin/bash

# This script calls _upgrade_postgresql_database.sh to complete the PostgreSQL
# and PostGIS upgrades. Refer to that file for further documentation on the
# script behavior
#
# Author: tgurney

if [[ "$(id -u)" -ne 0 ]]; then
    echo "$(basename $0): need to be root."
    exit 1
fi

ts=$(date +%F_%H%M%S)
logdir=/data/fxa/INSTALL/a2logs/19.3.5/pg_upgrade-${ts}
mkdir -p "${logdir}" || exit 1
chown -R awips:fxalpha "${logdir}"
chmod 2775 "${logdir}"
echo "INFO: Postgres upgrade logs will be saved to ${logdir}"
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $logdir
sudo -nu awips bash "${scriptdir}"/_upgrade_postgresql_database.sh "${scriptdir}" | sudo -nu awips tee ./postgres_upgrade_$(date +%F_%H%M%S).log
