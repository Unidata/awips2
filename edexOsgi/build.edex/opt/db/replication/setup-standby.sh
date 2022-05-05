#!/bin/bash
##
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
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Sep 22, 2016 5885       tgurney     Initial creation
# Jan  4, 2017 6056       tgurney     Fix tmp dir cleanup
# Mar 16, 2017 6184       tgurney     Update to fetch and use SSL certificates
# Mar 29, 2017 6184       tgurney     Fix SSL certificate sync from master
# Apr  4, 2017 6184       tgurney     Set correct permissions on cert directory
# Apr  4, 2017 6122       tgurney     Move database to /awips2/database
#                                     + cleanup and fixes for postgres 9.5.x
# Aug  6, 2018 7431       tgurney     Bug fixes. Allow starting without a
#                                     pg_hba.conf on standby server
#
##


# Configuration ###############################################################

# Credentials
db_superuser=awipsadmin
db_rep_user=replication # for connecting to master

# Master server info
master_hostname="$1" # from command line
master_port=5432

# Local server info
this_host=$(hostname -s)
local_port=5432
data_dir=/awips2/database/data
ssl_dir=/awips2/database/ssl
tablespace_dir=/awips2/database/tablespaces

# pg_hba.conf backup location
hba_backup="$(mktemp /tmp/pg_hba.backup.XXXXXX)"
rm -f "${hba_backup}"

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

as_awips='sudo -u awips -i '

log() {
    echo $* | ${as_awips} tee -a "${log_file}"
}

###############################################################################


do_pg_ctl() {
    ${as_awips} "${pg_ctl}" -o \"--port=${local_port}\" -D "${data_dir}" $* >/dev/null 2>&1
    return $?
}


stop_server() {
    do_pg_ctl -m fast stop && sleep 1
    do_pg_ctl -s status
    # error code 3 from pg_ctl means server is not running
    # 4 means there is no database in the data dir, this is okay
    [[ "$?" -eq 3 || "$?" -eq 4 ]]; return $?
}

make_clean_db_dirs() {
    rm -rf "${data_dir}"
    ${as_awips} mkdir -p "${data_dir}"
    ${as_awips} chmod 700 "${data_dir}"
    rm -rf "${tablespace_dir}"
    ${as_awips} mkdir -p "${tablespace_dir}"
    ${as_awips} chmod 700 "${tablespace_dir}"
}

restore_pg_hba() {
    if [[ -f "${hba_backup}" ]]; then
        log "INFO: Restoring backed up pg_hba.conf"
        ${as_awips} cp -a "${hba_backup}" "${data_dir}"/pg_hba.conf
        ${as_awips} chmod 600 "${data_dir}"/pg_hba.conf
    else
	log "WARN: No backed up pg_hba.conf to restore"
    fi
}

cleanup_exit() {
    log "ERROR: There were one or more errors; see above."
    log "INFO: Cleaning up."
    stop_server
    if [[ "$?" -eq 0 ]]; then
        make_clean_db_dirs
    else
        # Do not delete any database files if server is running
        log -n "WARNING: Postgres is still running. "
        log "See ${data_dir}/pg_log/postgresql-$(date +%A).log for possible errors."
    fi
    restore_pg_hba
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
echo "  Tablespace dir: ${tablespace_dir}"
echo
echo "All data at ${data_dir} and ${tablespace_dir} on this server "
echo "will be destroyed and this server will become a replica of "
echo "${master_hostname}."
echo
echo -n "Is this OK? Type YES in all caps: "
read answer

if [[ "${answer}" != 'YES' ]]; then
    echo Canceling.
    exit 1
fi

# Actually do it ##############################################################

# Make log file for script output
${as_awips} mkdir -p "${log_dir}" || exit 1
${as_awips} touch "${log_file}" || exit 1
# Purge old logs
${as_awips} find "${log_dir}"/*.log -xdev \
    | sort \
    | head -n -${keep_logs} \
    | tr '\n' '\0' \
    | sudo xargs -0r rm

log "INFO: Starting replication setup on ${this_host}:${local_port}"
log "INFO: Will replicate ${master_hostname}:${master_port}"

stop_server || exit 1
trap 'cleanup_exit' SIGINT SIGTERM

# Get certificates from master
master_ssl_dir="${ssl_dir}/replication/${master_hostname}"
echo ${as_awips} mkdir -p "${master_ssl_dir}"
${as_awips} mkdir -p "${master_ssl_dir}"
log "INFO: Downloading SSL certs and keyfile from ${master_hostname}"
# must ssh as root to skip password prompt
rsync --delete-before -av -e ssh \
    "${master_hostname}":"${master_ssl_dir}"/{replication.crt,replication.key,root.crt} \
    "${master_ssl_dir}" || exit 1
chown -R awips:fxalpha "${ssl_dir}"/replication
find "${ssl_dir}"/replication -xdev -type f -exec chmod 600 {} \;
find "${ssl_dir}"/replication -xdev -type d -exec chmod 700 {} \;

# Backup pg_hba.conf
if [[ -f "${data_dir}/pg_hba.conf" ]]; then
    ${as_awips} cp -a "${data_dir}/pg_hba.conf" "${hba_backup}" || cleanup_exit
    log "INFO: Backup of pg_hba.conf is at ${hba_backup}"
else
    log "WARN: Cannot backup local ${data_dir}/pg_hba.conf because it does not exist. Continuing anyway."
fi

# Delete all database files
make_clean_db_dirs

# SSL connection string parts
# needed for basebackup and recovery.conf
sslmode_part="sslmode=verify-ca"
sslcert_part="sslcert=${master_ssl_dir}/replication.crt"
sslkey_part="sslkey=${master_ssl_dir}/replication.key"
sslrootcert_part="sslrootcert=${master_ssl_dir}/root.crt"
ssl_part="${sslmode_part} ${sslcert_part} ${sslkey_part} ${sslrootcert_part}"

log "INFO: Retrieving base backup from ${master_hostname}"
log "Enter the password for the '${db_rep_user}' role now, if prompted."
${as_awips} "${pg_basebackup}" \
    --host="${master_hostname}" \
    --verbose --progress \
    --username="${db_rep_user}" \
    --port=${master_port} \
    --db="${ssl_part}" \
    -D "${data_dir}" 2>&1 | tee -a ${log_file}

if [[ "${PIPESTATUS[0]}" != "0" ]]; then
    cleanup_exit
fi


# Write recovery.conf

host_part="host=${master_hostname}"
port_part="port=${master_port}"
user_part="user=${db_rep_user}"
primary_conninfo="${host_part} ${port_part} ${user_part} ${ssl_part}"

log "INFO: Writing ${data_dir}/recovery.conf"
rm -f "${data_dir}/recovery.conf"
${as_awips} touch "${data_dir}"/recovery.conf
cat >> "${data_dir}/recovery.conf" << EOF || cleanup_exit
standby_mode='on'
primary_conninfo='${primary_conninfo}' 
recovery_target_timeline='latest'
trigger_file='${data_dir}/promote'
EOF
# Remove recovery.done if it exists
rm -f "${data_dir}/recovery.done"

# Install backed up pg_hba.conf
restore_pg_hba

# Start it up and run test query
log "INFO: Starting PostgreSQL"
do_pg_ctl start -w || cleanup_exit

log "INFO: Testing read-only connection to standby"
is_recovery=$(${as_awips} "${psql}" \
  -U "${db_superuser}" \
  --port=${local_port} \
  --db=metadata \
  -Aqtc "select pg_is_in_recovery();")

if [[ "${is_recovery}" != "t" ]]; then
    log "ERROR: It looks like this server failed to start up properly, or is"
    log "ERROR: not in recovery mode."
    cleanup_exit
fi

rm -f "${hba_backup}"

log "INFO: Setup is complete. No errors reported."
