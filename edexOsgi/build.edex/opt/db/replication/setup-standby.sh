#!/bin/bash
#
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
#

# Configuration ###############################################################

# Credentials
db_superuser=awips  # awipsadmin on 16.4.1 and later
db_rep_user=replication # for connecting to master
db_rep_password=replication

# Master server info
master_hostname="$1" # from command line
master_port=5432

# Local server info
this_host=$(hostname -s)
local_port=5432
data_dir=/awips2/data

# Location of PostgreSQL install
pg_dir=/awips2/postgresql

# Location of programs
pg_basebackup=${pg_dir}/bin/pg_basebackup
pg_ctl=${pg_dir}/bin/pg_ctl
psql=/awips2/psql/bin/psql

###############################################################################


do_pg_ctl() {
    "${pg_ctl}" -o \"--port=${local_port}\" -D "${data_dir}" $* >/dev/null 2>&1
    return $?
}


stop_server() {
    do_pg_ctl -m fast stop && sleep 1
    do_pg_ctl -s status
    # error code 3 from pg_ctl means server is not running
    return $([[ "$?" -eq 3 ]]);
}


cleanup_exit() {
    echo "ERROR: There were one or more errors; see above."
    echo "INFO: Cleaning up."
    stop_server
    if [[ "$?" -eq 0 ]]; then
        sleep 1
        rm -rf "${data_dir}"/*
    else
        # I don't know if this is possible, but if it is, we don't want to
        # delete data dir while server is running
        echo -n "WARNING: Postgres is still running. "
        echo "See ${data_dir}/pg_log/postgresql-$(date +%A).log for possible errors."
    fi
    if [[ -d "${config_tmpdir}" ]]; then
        if [[ -f "${config_tmpdir}/pg_hba.conf" ]]; then
            mv "${config_tmpdir}/pg_hba.conf" ${data_dir}
        fi
        if [[ -d "${config_tmpdir}/pg_log" ]]; then
            if [[ -d "${data_dir}/pg_log" ]]; then
                logdir="${data_dir}/pg_log-$(date +%F_%H%M%S)"
            else
                logdir="${data_dir}/pg_log"
            fi
            echo "INFO: Moving old logs to ${logdir}"
            mv "${config_tmpdir}/pg_log" "${logdir}"
        fi
        rm -rf "${config_tmpdir}"
    fi
    rm -rf "${data_tmpdir}"
    exit 1
}


# Preliminary checks ##########################################################

# Hostname arg
if [[ -z "${master_hostname}" ]]; then
    echo "Usage: $(basename $0) <master-hostname>"
    exit 1
fi

if [[ "$(id -u)" -ne "$(id -u awips)" ]]; then
    echo "$(basename $0): Must run as user 'awips'."
    exit 1
fi

# Cannot replicate self
if [[ "${master_hostname}" == "${this_host}" ||
      "${master_hostname}" == "localhost" ||
      "${master_hostname}" == "$(hostname)" ]]; then
    echo -n "$(basename $0): ${master_hostname} cannot replicate itself. "
    echo "Choose a different server name."
    exit 1
fi


# Warning prompt
echo "You are about to configure this server (${this_host}) as a PostgreSQL"
echo "standby server."
echo
echo "  Master:   ${master_hostname}:${master_port}"
echo "  Standby:  ${this_host}:${local_port}"
echo "  Data dir: ${data_dir}"
echo
echo "All data at ${data_dir} on this server will be destroyed and this server"
echo "will become a replica of ${master_hostname}."
echo
echo -n "Is this OK? Type YES in all caps: "
read answer

if [[ "${answer}" != 'YES' ]]; then
    echo Canceling.
    exit 1
fi

# Actually do it ##############################################################

stop_server || exit 1
trap 'cleanup_exit' SIGINT

# Backup pg_hba.conf and old logs
config_tmpdir=$(mktemp -d --tmpdir=${data_dir} .tmp.XXXXXX || cleanup_exit)
if [[ -f "${data_dir}/pg_hba.conf" ]]; then
    cp -a "${data_dir}/pg_hba.conf" "${config_tmpdir}" || cleanup_exit
fi
if [[ -d "${data_dir}/pg_log" ]]; then
    cp -a "${data_dir}/pg_log" "${config_tmpdir}" || cleanup_exit
fi

# Prepare data directory
if [[ -d "${data_dir}" ]]; then
    rm -rf "${data_dir}"/*
else
    mkdir -p "${data_dir}" || exit 1
    chmod 700 "${data_dir}" || exit 1
fi


# pg_basebackup will not write to a non-empty directory
# so we have to make a temporary one
data_tmpdir=$(mktemp -d --tmpdir=${data_dir} .tmp.XXXX || cleanup_exit)
# Fetch and install base backup
echo "INFO: Fetching base backup from ${master_hostname}"
echo "Enter the password for the '${db_rep_user}' role now, if prompted."
"${pg_basebackup}" \
    --host="${master_hostname}" \
    --verbose --progress --xlog-method=fetch \
    --username="${db_rep_user}" --format=tar --gzip \
    --port=${master_port} \
    -D "${data_tmpdir}" || cleanup_exit
mv "${data_tmpdir}"/*.tar.gz "${data_dir}" || cleanup_exit

echo "INFO: Installing base backup to ${data_dir}"
pushd "${data_dir}" > /dev/null || cleanup_exit
tar xzf "${data_dir}/base.tar.gz" || cleanup_exit
popd > /dev/null
rm -f "${data_dir}/base.tar.gz"

# Install tablespaces
echo INFO: Unpacking tablespaces
# On Postgres 9.5 and later we need to read tablespace_map and create the
# symlinks ourselves
if [[ -f "${data_dir}/tablespace_map" ]]; then
    while IFS='' read -r line; do
        ts_num="$(echo "$line" | cut -d' ' -f1)"
        ts_path="$(echo "$line" | cut -d' ' -f2-)"
        if [[ -n "${ts_num}" && -n "${ts_path}" ]]; then
            rm -f "${data_dir}/pg_tblspc/${ts_num}"
            ln -sf "${ts_path}" "${data_dir}/pg_tblspc/${ts_num}" || cleanup_exit
        fi
    done < "${data_dir}/tablespace_map"
    rm -f "${data_dir}/tablespace_map"
fi

# Now unpack each tar in the right place
for ts_link in "${data_dir}/pg_tblspc"/*; do
    this_ts=$(readlink "${ts_link}")
    echo -n "  ${this_ts}..."
    tar_name=$(basename "${ts_link}")
    if [[ -d "${this_ts}" ]]; then
        rm -rf "${this_ts}"/*
    else
        mkdir -p "${this_ts}" || cleanup_exit
    fi
    pushd "${this_ts}" > /dev/null
    tar xzf "${data_dir}/${tar_name}.tar.gz" || cleanup_exit
    popd > /dev/null
    rm -f "${data_dir}/${tar_name}.tar.gz"
    echo done.
done

# Write recovery.conf
echo "INFO: Writing ${data_dir}/recovery.conf"
cat > "${data_dir}/recovery.conf" << EOF || cleanup_exit
standby_mode='on'
primary_conninfo='host=${master_hostname} port=${master_port} user=${db_rep_user} password=${db_rep_password}'
recovery_target_timeline='latest'
trigger_file='${data_dir}/promote'
EOF
# Remove recovery.done if it exists
rm -f "${data_dir}/recovery.done"

# Install pg_hba.conf
if [[ -f "${config_tmpdir}/pg_hba.conf" ]]; then
    echo "INFO: Installing ${data_dir}/pg_hba.conf"
    mv "${config_tmpdir}/pg_hba.conf" "${data_dir}"
fi

# Save old pg_logs
if [[ -d "${config_tmpdir}/pg_log" ]]; then
    logdir_ts=$(date +%F_%H%M%S)
    echo "INFO: Moving old logs to ${data_dir}/pg_log-${logdir_ts}"
    mv "${config_tmpdir}/pg_log" "${data_dir}/pg_log-${logdir_ts}"
fi

# Start it up and run test query
echo "INFO: Starting PostgreSQL"
do_pg_ctl start -w || cleanup_exit

echo "INFO: Testing read-only connection to standby"
is_recovery=$("${psql}" \
  -U "${db_superuser}" \
  --port=${local_port} \
  --db=metadata \
  -Aqtc "select pg_is_in_recovery();")

if [[ "${is_recovery}" != "t" ]]; then
    echo "ERROR: It looks like this server failed to start up properly, or is"
    echo "ERROR: not in recovery mode."
    cleanup_exit
fi

rm -rf ${config_tmpdir}
rm -rf ${data_tmpdir}

echo "INFO: Setup is complete. No errors reported."
