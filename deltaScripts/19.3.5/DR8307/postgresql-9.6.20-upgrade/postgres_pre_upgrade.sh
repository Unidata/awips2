#!/bin/bash

# This script just calls _postgres_pre_upgrade.sh as user awips
#
# Author: tgurney

if [[ "$(id -u)" -ne 0 ]]; then
    echo "$(basename $0): need to be root."
    exit 1
fi

scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
sudo -nu awips bash "$scriptdir/_postgres_pre_upgrade.sh"
