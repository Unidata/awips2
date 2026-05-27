#!/bin/bash

# This script creates a copy of the current PostgreSQL installation (not the
# database cluster itself), which is needed to perform the upgrade.
#
# Author: tgurney

source "$(dirname "$0")/settings.sh" || exit 1

if [[ "${postgres_version}" != ${old_ver} ]]; then
    echo -n "ERROR: Currently installed version of PostgreSQL is "
    echo "${postgres_version}. Expected ${old_ver}. Cannot continue."
    exit 1
fi

echo "INFO: Copying ${postgres_dir} to ${postgres_copy_of_old}."

if [[ -e ${postgres_copy_of_old} ]]; then
    rm -rvf ${postgres_copy_of_old}
fi

cp -av ${postgres_dir} ${postgres_copy_of_old}
if [[ "$?" -ne 0 ]]; then
    echo -en "\nERROR: Failed to copy ${postgres_dir} to ${postgres_copy_of_old}"
    echo "Cannot continue."
    exit 1
fi

sync

echo "INFO: Pre-upgrade preparation is complete. No errors reported."
