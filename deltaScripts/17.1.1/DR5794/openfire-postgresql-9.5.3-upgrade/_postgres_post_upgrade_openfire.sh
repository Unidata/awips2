#!/bin/bash

source "$(dirname $0)/settings_openfire.sh" || exit 1

# directory for temporarily keeping old config files
TEMP_CONFIG_DIR=$(mktemp -d) || exit 1

export LD_LIBRARY_PATH=${POSTGRES_DIR}/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${PSQL_DIR}/lib:$LD_LIBRARY_PATH


# roll back as much as possible in the event of an error
cleanup_exit() {
    echo "INFO: Cleaning up"
    # remove symlinked psql
    if [[ -h "${POSTGRES_DIR}/bin/psql" ]]; then
        rm -vf "${POSTGRES_DIR}/bin/psql"
    fi
    # put back any tablespaces that had been moved
    for TS_DIR in "${TABLESPACES[@]}"; do
        if [[ -d "${TEMP_NEW_DATA}/${TS_DIR}" && ! -e "${POSTGRES_DATA_DIR}/${TS_DIR}" ]]; then
            mv -fv "${TEMP_NEW_DATA}/${TS_DIR}" "${POSTGRES_DATA_DIR}/${TS_DIR}"
        fi
    done
    # restore original pg_hba.conf
    if [[ -f "${TEMP_CONFIG_DIR}/pg_hba.conf" ]]; then
        cp -fv "${TEMP_CONFIG_DIR}/pg_hba.conf" "${POSTGRES_DATA_DIR}/pg_hba.conf"
    fi
    # clean up temp directories
    rm -rf "${TEMP_CONFIG_DIR}"
    rm -rf "${TEMP_OLD_DATA}"
    rm -rf "${TEMP_NEW_DATA}"
    sync
    echo -e "\nERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "ERROR: !!          UPGRADE FAILED            !!"
    echo "ERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo -e "\nERROR: One or more errors occurred. See above output."
    echo -en "\nAny changes made by this script have been rolled back. You "
    echo -en "may run this script again once any issues have been resolved.\n"
    exit 1
}


if [[ ! -d "${POSTGRES_COPY_OF_OLD}" ]]; then
    echo -n "ERROR: Did not find the old version of PostgreSQL at "
    echo "${POSTGRES_COPY_OF_OLD}. Upgrade might already be complete."
    echo "ERROR: Cannot continue."
    cleanup_exit
fi

if [[ "${POSTGRES_VERSION}" != "${NEW_VER}" ]]; then
    echo -n "ERROR: Currently installed version of PostgreSQL is "
    echo "${POSTGRES_VERSION}. Expected ${NEW_VER}. Cannot continue."
    cleanup_exit
fi

if [[ "${PSQL_VERSION}" != "${NEW_VER}" ]]; then
    echo -n "ERROR: Currently installed version of psql is "
    echo "${PSQL_VERSION}. Expected ${NEW_VER}. Cannot continue."
    cleanup_exit
fi

${POSTGRES_DIR}/bin/pg_ctl -D "${POSTGRES_DATA_DIR}" status
if [[ "$?" -eq 0 ]]; then
    echo "ERROR: It looks like PostgreSQL is running. Cannot continue."
    cleanup_exit
fi


# identify any tablespaces that need to be moved after running pg_upgrade
TABLESPACES=()
for TS_LINK in "${PG_TBLSPC}"/* ; do
    THIS_TS=$(readlink "${TS_LINK}")
    if [[ "${THIS_TS}" == "${POSTGRES_DATA_DIR}"* ]]; then
        TABLESPACES+=("$(basename ${THIS_TS})")
    fi
done


echo -e "\nINFO: Backing up config files"
for CONF in "${CONFIG_FILES[@]}"; do
    cp -v "${POSTGRES_DATA_DIR}/${CONF}" "${TEMP_CONFIG_DIR}/${CONF}" || cleanup_exit
done


echo -e "\nINFO: Initializing new cluster at ${TEMP_NEW_DATA}"
rm -rf "${TEMP_NEW_DATA}"
"${INITDB}" -D "${TEMP_NEW_DATA}" || cleanup_exit


echo -e "\nINFO: Starting new cluster on port ${TEMP_PORT}"
"${POSTGRES_DIR}/bin/postgres" --port="${TEMP_PORT}" -D "${TEMP_NEW_DATA}" 2>&1 &
sleep 10 # wait for server to start


echo -e "\nINFO: Setting up awips db user"
"${PSQL}" --port="${TEMP_PORT}" --db=postgres << EOF
    begin;
    alter role awips with password 'awips';
    commit;
EOF


echo -e "\nINFO: Stopping new cluster"
"${POSTGRES_DIR}/bin/pg_ctl" -D "${TEMP_NEW_DATA}" stop
sleep 3


# prevent pg_upgrade from prompting for a password for the old cluster
echo -e "\nINFO: Updating old pg_hba.conf to trust local connections"
grep -E "local\s+all\s+all\s+trust" "${POSTGRES_DATA_DIR}/pg_hba.conf" >/dev/null
if [[ "$?" -ne 0 ]]; then
    echo -e "local\tall\tall\ttrust" >> "${POSTGRES_DATA_DIR}/pg_hba.conf" || cleanup_exit
fi

# Need to symlink psql,
# because pg_upgrade expects $POSTGRES_DIR/bin/psql to exist
echo -e "\nINFO: Creating link to ${PSQL} at ${POSTGRES_DIR}/bin/psql"
if [[ ! -x "${POSTGRES_DIR}/bin/psql" ]]; then
    ln -fvs "${PSQL}" "${POSTGRES_DIR}/bin/psql" || exit 1
fi

echo -e "\nINFO: Starting upgrade"

${PG_UPGRADE} --jobs=4 --link \
    --old-port="${TEMP_PORT}" \
    --new-port="${TEMP_PORT}" \
    --old-bindir="${POSTGRES_COPY_OF_OLD}/bin" \
    --new-bindir="${POSTGRES_DIR}/bin" \
    --old-datadir="${POSTGRES_DATA_DIR}" \
    --new-datadir="${TEMP_NEW_DATA}" \
    --username=awips \
    || cleanup_exit
# analyze_new_cluster.sh is created by pg_upgrade but is not needed.
# admin should run the included "rebuild_stats.sh" instead.
rm -f analyze_new_cluster.sh


echo -e "\nINFO: Fixing postgresql.conf for 9.5"
TEMP_POSTGRESQL_CONF="${TEMP_CONFIG_DIR}/postgresql.conf"
cp -v "${TEMP_POSTGRESQL_CONF}" "${TEMP_POSTGRESQL_CONF}.old" \
    && $(dirname $0)/_update_postgresql_conf.pl \
        "${TEMP_POSTGRESQL_CONF}" \
        >"${TEMP_POSTGRESQL_CONF}.new" \
    && mv -v "${TEMP_POSTGRESQL_CONF}.new" "${TEMP_POSTGRESQL_CONF}"
[[ "$?" -eq 0 ]] || cleanup_exit


echo -e "\nINFO: Copying config files to new data directory"
for CONF in "${CONFIG_FILES[@]}"; do
    cp -v "${TEMP_CONFIG_DIR}/${CONF}" "${TEMP_NEW_DATA}/${CONF}" || cleanup_exit
done


echo -e "\nINFO: Moving tablespaces"
for TS_DIR in "${TABLESPACES[@]}"; do
    mv -v "${POSTGRES_DATA_DIR}/${TS_DIR}" "${TEMP_NEW_DATA}" || cleanup_exit
done


echo -e "\nINFO: Moving new data dir to ${POSTGRES_DATA_DIR}"
rm -rf "${TEMP_OLD_DATA}"
mkdir "${TEMP_OLD_DATA}" || cleanup_exit
mv "${POSTGRES_DATA_DIR}"/* "${TEMP_OLD_DATA}"
mv "${TEMP_NEW_DATA}"/* "${POSTGRES_DATA_DIR}"


echo -e "\nINFO: Deleting temp directories"
rm -rf "${TEMP_OLD_DATA}"
rm -rf "${TEMP_NEW_DATA}"
rm -rf "${TEMP_CONFIG_DIR}"

echo -e "\nINFO: Removing symlinked psql"
if [[ -h "${POSTGRES_DIR}/bin/psql" ]]; then
    rm -f "${POSTGRES_DIR}/bin/psql"
fi

echo -e "\nINFO: Syncing disks"
sync


echo -e "\nINFO: ****************************************"
echo "INFO: **         UPGRADE COMPLETE           **"
echo "INFO: ****************************************"
