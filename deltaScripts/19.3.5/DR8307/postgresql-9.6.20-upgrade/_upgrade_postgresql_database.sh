#!/bin/bash

# This script performs the PostgreSQL upgrade and calls upgrade_postgis.sh to
# perform the PostGIS upgrade. This script should only be called by
# upgrade_postgresql_database.sh and not by itself.
#
# The procedure is broken into two parts: the database cluster upgrade and the
# PostGIS upgrade. Between them is effectively a checkpoint: if the cluster
# upgrade succeeds but the PostGIS upgrade fails, you may rerun this script
# and it will retry only the PostGIS upgrade. Likewise, if the cluster upgrade
# fails, you may rerun the script and it will start from the beginning.
#
# Do not Ctrl+C or otherwise interrupt the script while it is running as this
# may prevent proper cleanup.
#
# Author: tgurney

# $1 is the directory containing this script
here="$1"
source "$here/settings.sh" || exit 1

# directory for temporarily keeping config files from the data dir
temp_config_dir=$(mktemp -d) || exit 1

export LD_LIBRARY_PATH="${postgres_lib}:${psql_dir}/lib:$LD_LIBRARY_PATH"


# roll back as much as possible in the event of an error
cleanup_exit() {
    echo "INFO: Cleaning up"
    # remove symlinked psql
    if [[ -h "${postgres_dir}/bin/psql" ]]; then
        rm -vf "${postgres_dir}/bin/psql"
    fi
    # put back any tablespaces that had been moved
    for ts_dir in "${tablespaces[@]}"; do
        if [[ -d "${temp_new_data}/${ts_dir}" && ! -e "${postgres_data_dir}/${ts_dir}" ]]; then
            mv -fv "${temp_new_data}/${ts_dir}" "${postgres_data_dir}/${ts_dir}"
        fi
    done
    # restore original pg_hba.conf
    if [[ -f "${temp_config_dir}/pg_hba.conf" ]]; then
        cp -fv "${temp_config_dir}/pg_hba.conf" "${postgres_data_dir}/pg_hba.conf"
    fi
    # clean up temp directories
    rm -rf "${temp_config_dir}"
    rm -rf "${temp_old_data}"
    rm -rf "${temp_new_data}"
    # clean up symlinked libs
    rm -vf "${postgres_lib}/postgis-${old_postgis_ver}.so"
    rm -vf "${postgres_lib}/postgis_topology-${old_postgis_ver}.so"
    rm -vf "${postgres_lib}/rtpostgis-${old_postgis_ver}.so"
    rm -vf "${postgres_lib}/liblwgeom-${old_postgis_ver}.so.0"
    sync
    echo -e "\nERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "ERROR: !!          UPGRADE FAILED            !!"
    echo "ERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo -e "\nERROR: One or more errors occurred. See above output."
    echo -en "\nAny changes made by this script have been rolled back. You "
    echo -en "may run this script again once any issues have been resolved.\n"
    exit 1
}

finish_and_upgrade_postgis() {
    echo ""
    echo "INFO: *********************************************************************"
    echo "INFO: ** The PostgreSQL cluster upgrade has been completed successfully. **"
    echo "INFO: **                                                                 **"

    postgis_error=
    if [[ -n "${is_openfire}" ]]; then
        echo "INFO: ** This is an Openfire database. No PostGIS upgrade is necessary.  **"
        echo "INFO: *********************************************************************"
    else
        echo "INFO: ** This script will now perform the PostGIS upgrade.               **"
        echo "INFO: *********************************************************************"
        stop_when_done=
        "${psql}" --user=$db_admin_user --db=$default_db -c 'select 1 test;' >/dev/null 2>&1 || stop_when_done=1
        if [[ -n "$stop_when_done" ]]; then
            echo -e "\nINFO: Starting edex_postgres service"
            sudo service edex_postgres start
            echo "INFO: Waiting 10 seconds"
            sleep 10
        else
            echo -e  "\nINFO: PostgreSQL is already running"
        fi
        echo -e "\nINFO: Running upgrade_postgis.sh"
        postgis_tmp_log="$(mktemp)"
        "${psql}" --user=$db_admin_user --db=$default_db -c 'select 1 test;' >/dev/null \
            && bash "${here}/upgrade_postgis.sh" "${here}" | tee "${postgis_tmp_log}"
        if grep -q "ERROR:" "${postgis_tmp_log}"; then
            postgis_error=1
            echo -e "\nERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo "ERROR: !!      POSTGIS UPGRADE FAILED        !!"
            echo "ERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo -e "\nERROR: One or more errors occurred. See above output."
            echo -en "You may run this script again once any issues have been resolved.\n"
        else
            echo "INFO: PostGIS upgrade was successful."
        fi
        rm -f "${postgis_tmp_log}"
        if [[ -n "$stop_when_done" ]]; then
            echo INFO: Stopping edex_postgres service
            sudo service edex_postgres stop
            sleep 3
        fi
    fi
    if [[ -z "$postgis_error" ]]; then
        echo -e "\nINFO: ****************************************"
        echo "INFO: **         UPGRADE COMPLETE           **"
        echo "INFO: ****************************************"
    fi
}

if [[ ! -d "${postgres_data_dir}" ]]; then
    echo "ERROR: Specified PostgreSQL data dir ${postgres_data_dir} does not exist."
    echo "ERROR: Cannot continue."
    cleanup_exit
fi

if [[ -d "${postgres_copy_of_old}_done" ]]; then
    echo "INFO: PostgreSQL upgrade has already been completed."
    echo "INFO: Skipping to the PostGIS upgrade."
    finish_and_upgrade_postgis
    exit 0
fi

if [[ ! -d "${postgres_copy_of_old}" ]]; then
    echo -n "ERROR: Did not find the old version of PostgreSQL at "
    echo "${postgres_copy_of_old}. Upgrade might already be complete."
    echo "ERROR: Cannot continue."
    cleanup_exit
fi

if [[ "${postgres_version}" != "${new_ver}" ]]; then
    echo -n "ERROR: Currently installed version of PostgreSQL is "
    echo "${postgres_version}. Expected ${new_ver}. Cannot continue."
    cleanup_exit
fi

if [[ "${psql_version}" != "${new_ver}" ]]; then
    echo -n "ERROR: Currently installed version of psql is "
    echo "${psql_version}. Expected ${new_ver}. Cannot continue."
    cleanup_exit
fi

${postgres_dir}/bin/pg_ctl -D "${postgres_data_dir}" status
if [[ "$?" -eq 0 ]]; then
    echo "ERROR: It looks like PostgreSQL is running. Cannot continue."
    cleanup_exit
fi

# This doesn't appear to work consistently, so we cannot guarantee proper
# cleanup on Ctrl+C or other interruption.
trap cleanup_exit SIGINT SIGTERM

# Identify any tablespaces that need to be moved after running pg_upgrade
tablespaces=()
for ts_link in "${pg_tblspc}"/* ; do
    this_ts=$(readlink "${ts_link}")
    if [[ "${this_ts}" == "${postgres_data_dir}"* ]]; then
        echo "INFO: ${this_ts} is inside the data dir. It will be moved temporarily"
        tablespaces+=("$(basename ${this_ts})")
    fi
done


echo -e "\nINFO: Backing up config files"
for conf in "${config_files[@]}"; do
    if [[ -f "${postgres_data_dir}/${conf}" ]]; then
        cp -v "${postgres_data_dir}/${conf}" "${temp_config_dir}/${conf}" || cleanup_exit
    else
        echo "INFO: $conf does not exist, skipping this one"
    fi
done
echo "INFO: Done backing up config files"


echo -e "\nINFO: Initializing new cluster at ${temp_new_data}"
rm -rf "${temp_new_data}"
"${initdb}" -D "${temp_new_data}" || cleanup_exit


echo -e "\nINFO: Starting new cluster on port ${temp_port}"
"${postgres_dir}/bin/postgres" --port="${temp_port}" -D "${temp_new_data}" 2>&1 &
echo "INFO: Waiting 10 seconds for cluster to start"
sleep 10 || cleanup_exit
"${psql}" --port="${temp_port}" --db=postgres -c 'select 1 test;'
if [[ "$?" != "0" ]]; then
    echo "ERROR: Test query failed"
    cleanup_exit
fi

if [[ -n "${is_openfire}" ]]; then
    echo -e "\nINFO: Setting up awips db user"
    "${PSQL}" --port="${temp_port}" --db=postgres << EOF
        begin;
        alter role awips with password 'awips';
        commit;
EOF
else
    echo -e "\nINFO: Setting up $db_admin_user db user"
    "${psql}" --port="${temp_port}" --db=postgres -c \
        'create role awipstmp with login superuser createdb createrole;'
    "${psql}" --port="${temp_port}" --db=postgres --user=awipstmp << EOF
        begin;
        alter role awips rename to $db_admin_user;
        alter role awipstmp with nologin;
        commit;
EOF
    "${psql}" --port="${temp_port}" --db=postgres --user=$db_admin_user << EOF
        begin;
        alter role $db_admin_user with password 'awips';
        drop role awipstmp;
        commit;
EOF
fi


echo -e "\nINFO: Stopping new cluster"
"${postgres_dir}/bin/pg_ctl" -D "${temp_new_data}" stop
sleep 3 || cleanup_exit


# prevent pg_upgrade from prompting for a password for the old cluster
echo -e "\nINFO: Updating old pg_hba.conf to trust local connections"
grep -E "local\s+all\s+all\s+trust" "${postgres_data_dir}/pg_hba.conf" >/dev/null
if [[ "$?" -ne 0 ]]; then
    echo -e "local\tall\tall\ttrust" >> "${postgres_data_dir}/pg_hba.conf" || cleanup_exit
fi

# Need to symlink psql,
# because pg_upgrade expects $postgres_dir/bin/psql to exist
echo -e "\nINFO: Creating link to ${psql} at ${postgres_dir}/bin/psql"
if [[ ! -x "${postgres_dir}/bin/psql" ]]; then
    ln -fvs "${psql}" "${postgres_dir}/bin/psql" || cleanup_exit
fi

# Symlink libs that are needed for pg_upgrade to work
# This is only necessary because of the simultaneous PostGIS upgrade.
echo -e "\nINFO: Creating PostGIS ${old_postgis_ver} symlinks"
ln -fvs "${postgres_lib}/postgis-${new_postgis_ver}.so" "${postgres_lib}/postgis-${old_postgis_ver}.so" || cleanup_exit
ln -fvs "${postgres_lib}/postgis_topology-${new_postgis_ver}.so" "${postgres_lib}/postgis_topology-${old_postgis_ver}.so" || cleanup_exit
ln -fvs "${postgres_lib}/rtpostgis-${new_postgis_ver}.so" "${postgres_lib}/rtpostgis-${old_postgis_ver}.so" || cleanup_exit
ln -fvs "${postgres_lib}/liblwgeom-${new_postgis_ver}.so.0" "${postgres_lib}/liblwgeom-${old_postgis_ver}.so.0" || cleanup_exit

echo -e "\nINFO: Starting upgrade"

# Run pg_upgrade twice, first time is a read-only compatibility check.
for check in yes no; do
    if [[ "${check}" == "yes" ]]; then
        check="--check"
    else
        check=""
    fi
    LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${postgres_copy_of_old}/lib" \
    "${pg_upgrade}" ${check} --jobs=4 --link \
        --old-port="${temp_port}" \
        --new-port="${temp_port}" \
        --old-bindir="${postgres_copy_of_old}/bin" \
        --new-bindir="${postgres_dir}/bin" \
        --old-datadir="${postgres_data_dir}" \
        --new-datadir="${temp_new_data}" \
        --username="${db_admin_user}" \
        || cleanup_exit
done

# Point of no return, no cleanup/rollback is possible.
trap - SIGINT SIGTERM

# analyze_new_cluster.sh is created by pg_upgrade but is not needed.
# we provide our own script (rebuild_stats.sh) instead
rm -f analyze_new_cluster.sh


echo -e "\nINFO: Copying config files to new data directory"
for conf in "${config_files[@]}"; do
    if [[ -f "${temp_config_dir}/${conf}" ]]; then
        cp -v "${temp_config_dir}/${conf}" "${temp_new_data}/${conf}"
    fi
done


echo -e "\nINFO: Moving tablespaces"
for ts_dir in "${tablespaces[@]}"; do
    mv -v "${postgres_data_dir}/${ts_dir}" "${temp_new_data}"
done
echo "INFO: Done moving tablespaces"


echo -e "\nINFO: Moving new data dir to ${postgres_data_dir}"
rm -rf "${temp_old_data}"
mkdir "${temp_old_data}"
mv "${postgres_data_dir}"/* "${temp_old_data}"
mv "${temp_new_data}"/* "${postgres_data_dir}"


echo -e "\nINFO: Deleting temp directories"
rm -rf "${temp_old_data}"
rm -rf "${temp_new_data}"
rm -rf "${temp_config_dir}"

echo -e "\nINFO: Removing symlinked psql"
if [[ -h "${postgres_dir}/bin/psql" ]]; then
    rm -f "${postgres_dir}/bin/psql"
fi
echo -e "\nINFO: Removing symlinked libs"
rm -f "${postgres_lib}/postgis-${old_postgis_ver}.so"
rm -f "${postgres_lib}/postgis_topology-${old_postgis_ver}.so"
rm -f "${postgres_lib}/rtpostgis-${old_postgis_ver}.so"
rm -f "${postgres_lib}/liblwgeom-${old_postgis_ver}.so.0"

echo -e "\nINFO: Syncing disks"
sync

echo -e "\nINFO: Moving old PostgreSQL install to ${postgres_copy_of_old}_done"
mv "${postgres_copy_of_old}" "${postgres_copy_of_old}_done"

finish_and_upgrade_postgis

