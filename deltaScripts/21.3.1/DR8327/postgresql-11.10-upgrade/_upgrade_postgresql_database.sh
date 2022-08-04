#!/bin/bash

# This script performs the PostgreSQL upgrade. This script should only be
# called by upgrade_postgresql_database.sh and not by itself. If the upgrade
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
    sync
    echo -e "\nERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "ERROR: !!          UPGRADE FAILED            !!"
    echo "ERROR: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo -e "\nERROR: One or more errors occurred. See above output."
    echo -en "\nAny changes made by this script have been rolled back. You "
    echo -en "may run this script again once any issues have been resolved.\n"
    exit 1
}


if [[ ! -d "${postgres_data_dir}" ]]; then
    echo "ERROR: Specified PostgreSQL data dir ${postgres_data_dir} does not exist."
    echo "ERROR: Cannot continue."
    cleanup_exit
fi

if [[ -d "${postgres_copy_of_old}_done" ]]; then
    echo "INFO: PostgreSQL upgrade has already been completed."
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
    "${psql}" --port="${temp_port}" --db=postgres << EOF
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

# These scripts are created by pg_upgrade but are not needed.
# We provide our own script (rebuild_stats_and_hash_indexes.sh) instead.
rm -f analyze_new_cluster.sh
rm -f reindex_hash.sql

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

echo -e "\nINFO: Syncing disks"
sync

echo -e "\nINFO: Moving old PostgreSQL install to ${postgres_copy_of_old}_done"
mv "${postgres_copy_of_old}" "${postgres_copy_of_old}_done"

echo -e "\nINFO: ****************************************"
echo "INFO: **         UPGRADE COMPLETE           **"
echo "INFO: ****************************************"

