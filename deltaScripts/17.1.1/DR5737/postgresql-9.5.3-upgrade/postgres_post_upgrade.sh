#!/bin/bash

if [[ "$(id -u)" -ne 0 ]]; then
    echo "$(basename $0): need to be root."
    exit 1
fi

ts=$(date +%F_%H%M%S)
logdir=/data/fxa/INSTALL/a2logs/17.1.1/pg_upgrade-${ts}
mkdir -p "${logdir}" || exit 1
chown -R awips:fxalpha "${logdir}"
chmod 2775 "${logdir}"
echo INFO: Postgres upgrade logs will be saved to ${logdir}
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $logdir
sudo -nu awips bash "${scriptdir}"/_postgres_post_upgrade.sh | sudo -nu awips tee ./postgres_upgrade_$(date +%F_%H%M%S).log
