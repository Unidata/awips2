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
db_superuser=awips
db_rep_user=replication # for connecting to master

# Master server info
master_hostname="$1" # from command line
master_port=5432

# Local server info
this_host=$(hostname -s)
local_port=5432
data_dir=/awips2/data
ssl_dir=/awips2/database/ssl

# For logging the output of this script
log_dir=/awips2/database/replication/logs
# Keep this many logs, delete old ones
keep_logs=5
log_file="${log_dir}/setup-standby.$(date +%Y%m%d.%H%M%S).log"

# Location of PostgreSQL install
pg_dir=/awips2/postgresql

# Location of programs
pg_basebackup=${pg_dir}/bin/pg_basebackup
pg_ctl=${pg_dir}/bin/pg_ctl
psql=/awips2/psql/bin/psql

log() {
    echo $* | sudo -u awips tee -a "${log_file}"
}

###############################################################################


do_pg_ctl() {
    sudo -u awips "${pg_ctl}" -o \"--port=${local_port}\" -D "${data_dir}" $* >/dev/null 2>&1
    return $?
}


stop_server() {
    do_pg_ctl -m fast stop && sleep 1
    do_pg_ctl -s status
    # error code 3 from pg_ctl means server is not running
    return $([[ "$?" -eq 3 ]]);
}


cleanup_exit() {
    log "ERROR: There were one or more errors; see above."
    log "INFO: Cleaning up."
    stop_server
    if [[ "$?" -eq 0 ]]; then
        if [[ -d "${data_dir}" ]]; then
            rm -rf "${data_dir}"/*
        fi
    else
        # I don't know if this is possible, but if it is, we don't want to
        # delete data dir while server is running
        log -n "WARNING: Postgres is still running. "
        log "See ${data_dir}/pg_log/postgresql-$(date +%A).log for possible errors."
    fi
    if [[ -d "${config_tmpdir}" ]]; then
        if [[ -f "${config_tmpdir}/pg_hba.conf" ]]; then
            sudo -u awips mv "${config_tmpdir}/pg_hba.conf" ${data_dir}
        fi
        if [[ -d "${config_tmpdir}/pg_log" ]]; then
            if [[ -d "${data_dir}/pg_log" ]]; then
                logdir="${data_dir}/pg_log-$(date +%F_%H%M%S)"
            else
                logdir="${data_dir}/pg_log"
            fi
            log "INFO: Moving old logs to ${logdir}"
            sudo -u awips mv "${config_tmpdir}/pg_log" "${logdir}"
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

if [[ "$(id -u)" -ne 0 ]]; then
    echo "$(basename $0): Must run as root."
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

# Make log file for script output
sudo -u awips mkdir -p "${log_dir}" || exit 1
sudo -u awips touch "${log_file}" || exit 1
# Purge old logs
sudo -u awips find "${log_dir}"/*.log -xdev \
    | sort \
    | head -n -${keep_logs} \
    | tr '\n' '\0' \
    | sudo xargs -0r rm

log "INFO: Starting replication setup on ${this_host}:${local_port}"
log "INFO: Will replicate ${master_hostname}:${master_port}"

stop_server || exit 1
trap 'cleanup_exit' SIGINT

# Get certificates from master
master_ssl_dir="${ssl_dir}/replication/${master_hostname}"
sudo -u awips mkdir -p "${master_ssl_dir}"
log "INFO: Downloading SSL certs and keyfile from ${master_hostname}"
# must ssh as root to skip password prompt
rsync --delete-before -av -e ssh \
    "${master_hostname}":"${master_ssl_dir}"/{replication.crt,replication.key,root.crt} \
    "${master_ssl_dir}" || exit 1
chown -R awips:fxalpha "${ssl_dir}"/replication
find "${ssl_dir}"/replication -xdev -type f -exec chmod 600 {} \;
find "${ssl_dir}"/replication -xdev -type d -exec chmod 700 {} \;

# Backup pg_hba.conf and old postgres logs
config_tmpdir=$(sudo -u awips mktemp -d --tmpdir=${data_dir} .tmp.XXXXXX || cleanup_exit)
if [[ -f "${data_dir}/pg_hba.conf" ]]; then
    sudo -u awips cp -a "${data_dir}/pg_hba.conf" "${config_tmpdir}" || cleanup_exit
fi
if [[ -d "${data_dir}/pg_log" ]]; then
    sudo -u awips cp -a "${data_dir}/pg_log" "${config_tmpdir}" || cleanup_exit
fi

# Prepare data directory
log "INFO: Recreating ${data_dir}"
if [[ -d "${data_dir}" ]]; then
    rm -rf "${data_dir}"/*
else
    sudo -u awips mkdir -p "${data_dir}" || exit 1
    sudo -u awips chmod 700 "${data_dir}" || exit 1
fi

# SSL connection string parts
# needed for basebackup and recovery.conf
sslmode_part="sslmode=verify-ca"
sslcert_part="sslcert=${master_ssl_dir}/replication.crt"
sslkey_part="sslkey=${master_ssl_dir}/replication.key"
sslrootcert_part="sslrootcert=${master_ssl_dir}/root.crt"
ssl_part="${sslmode_part} ${sslcert_part} ${sslkey_part} ${sslrootcert_part}"

# pg_basebackup will not write to a non-empty directory
# so we have to make a temporary one
data_tmpdir=$(sudo -u awips mktemp -d --tmpdir=${data_dir} .tmp.XXXX || cleanup_exit)
# Fetch and install base backup
log "INFO: Fetching base backup from ${master_hostname}"
log "Enter the password for the '${db_rep_user}' role now, if prompted."
sudo -u awips "${pg_basebackup}" \
    --host="${master_hostname}" \
    --verbose --progress --xlog-method=fetch \
    --username="${db_rep_user}" --format=tar --gzip \
    --port=${master_port} \
    --db="${ssl_part}" \
    -D "${data_tmpdir}" || cleanup_exit
sudo -u awips mv "${data_tmpdir}"/*.tar.gz "${data_dir}" || cleanup_exit

log "INFO: Installing base backup to ${data_dir}"
pushd "${data_dir}" > /dev/null || cleanup_exit
sudo -u awips tar xzf "${data_dir}/base.tar.gz" || cleanup_exit
popd > /dev/null
rm -f "${data_dir}/base.tar.gz"

# Install tablespaces
log INFO: Unpacking tablespaces
# On Postgres 9.5 and later we need to read tablespace_map and create the
# symlinks ourselves
if [[ -f "${data_dir}/tablespace_map" ]]; then
    while IFS='' read -r line; do
        ts_num="$(echo "$line" | cut -d' ' -f1)"
        ts_path="$(echo "$line" | cut -d' ' -f2-)"
        if [[ -n "${ts_num}" && -n "${ts_path}" ]]; then
            rm -f "${data_dir}/pg_tblspc/${ts_num}"
            sudo -u awips ln -sf "${ts_path}" "${data_dir}/pg_tblspc/${ts_num}" || cleanup_exit
        fi
    done < "${data_dir}/tablespace_map"
    rm -f "${data_dir}/tablespace_map"
fi

# Now unpack each tar in the right place
for ts_link in "${data_dir}/pg_tblspc"/*; do
    this_ts=$(readlink "${ts_link}")
    log -n "  ${this_ts}..."
    tar_name=$(basename "${ts_link}")
    if [[ -d "${this_ts}" ]]; then
        rm -rf "${this_ts}"/*
    else
        sudo -u awips mkdir -p "${this_ts}" || cleanup_exit
    fi
    pushd "${this_ts}" > /dev/null
    sudo -u awips tar xzf "${data_dir}/${tar_name}.tar.gz" || cleanup_exit
    popd > /dev/null
    rm -f "${data_dir}/${tar_name}.tar.gz"
    log done.
done

# Write recovery.conf

host_part="host=${master_hostname}"
port_part="port=${master_port}"
user_part="user=${db_rep_user}"
primary_conninfo="${host_part} ${port_part} ${user_part} ${ssl_part}"

log "INFO: Writing ${data_dir}/recovery.conf"
rm -f "${data_dir}/recovery.conf"
sudo -u awips touch "${data_dir}"/recovery.conf
cat >> "${data_dir}/recovery.conf" << EOF || cleanup_exit
standby_mode='on'
primary_conninfo='${primary_conninfo}' 
recovery_target_timeline='latest'
trigger_file='${data_dir}/promote'
EOF
# Remove recovery.done if it exists
rm -f "${data_dir}/recovery.done"

# Install pg_hba.conf
if [[ -f "${config_tmpdir}/pg_hba.conf" ]]; then
    log "INFO: Installing ${data_dir}/pg_hba.conf"
    sudo -u awips mv "${config_tmpdir}/pg_hba.conf" "${data_dir}"
fi

# Save old pg_logs
if [[ -d "${config_tmpdir}/pg_log" ]]; then
    logdir_ts=$(date +%F_%H%M%S)
    log "INFO: Moving old logs to ${data_dir}/pg_log-${logdir_ts}"
    sudo -u awips mv "${config_tmpdir}/pg_log" "${data_dir}/pg_log-${logdir_ts}"
fi

# Start it up and run test query
log "INFO: Starting PostgreSQL"
do_pg_ctl start -w || cleanup_exit

log "INFO: Testing read-only connection to standby"
is_recovery=$(sudo -u awips "${psql}" \
  -U "${db_superuser}" \
  --port=${local_port} \
  --db=metadata \
  -Aqtc "select pg_is_in_recovery();")

if [[ "${is_recovery}" != "t" ]]; then
    log "ERROR: It looks like this server failed to start up properly, or is"
    log "ERROR: not in recovery mode."
    cleanup_exit
fi

rm -rf ${config_tmpdir}
rm -rf ${data_tmpdir}

log "INFO: Setup is complete. No errors reported."
