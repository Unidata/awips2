#!/bin/bash

if [[ "$(id -u)" -ne 0 ]]; then
    echo "$(basename $0): need to be root."
    exit 1
fi

sudo -nu awips bash $(dirname $0)/_postgres_pre_upgrade_openfire.sh
