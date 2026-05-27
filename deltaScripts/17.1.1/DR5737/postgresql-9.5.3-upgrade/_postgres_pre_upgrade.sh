#!/bin/bash

source "$(dirname $0)/settings.sh" || exit 1

if [[ "${POSTGRES_VERSION}" != ${OLD_VER} ]]; then
    echo -n "ERROR: Currently installed version of PostgreSQL is "
    echo "${POSTGRES_VERSION}. Expected ${OLD_VER}. Cannot continue."
    exit 1
fi

echo "INFO: Copying ${POSTGRES_DIR} to ${POSTGRES_COPY_OF_OLD}."

if [[ -e ${POSTGRES_COPY_OF_OLD} ]]; then
    rm -rf ${POSTGRES_COPY_OF_OLD}
fi

cp -a ${POSTGRES_DIR} ${POSTGRES_COPY_OF_OLD}
if [[ "$?" -ne 0 ]]; then
    echo -en "\nERROR: Failed to copy ${POSTGRES_DIR} to ${POSTGRES_COPY_OF_OLD}"
    echo "Cannot continue."
    exit 1
fi

sync

echo "INFO: Pre-upgrade preparation is complete. No errors reported."
