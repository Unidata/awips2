#!/bin/bash

# #6507 - This script moves all postgres tablespaces to
# /awips2/data/tablespaces. It will also create a symlink from
# /awips2/database/data to /awips2/data if the latter does not exist already.
#
# Run this script as root on all servers that have /awips2/data.
#
# Author: tgurney

pg_ctl=/awips2/postgresql/bin/pg_ctl
old_data_dir=/awips2/data
new_data_dir=/awips2/database/data
new_tablespace_dir=/awips2/database/tablespaces
pg_tblspc=${new_data_dir}/pg_tblspc

su - awips -c "${pg_ctl} -D ${new_data_dir} status"

if [[ "$?" -eq 0 ]]; then
    echo "ERROR: Postgres is running. Cannot continue."
    exit 1
fi

if [[ -e "${old_data_dir}" && ! -h "${old_data_dir}" ]]; then
    echo "ERROR: ${old_data_dir} exists and is not a symlink. It needs to be"
    echo "moved to ${new_data_dir} first."
    exit 1
fi

echo INFO: Starting.

if [[ ! -h "${old_data_dir}" ]]; then
    echo "INFO: Symlinking ${new_data_dir} to ${old_data_dir}"
    ln -sf "${new_data_dir}" "${old_data_dir}"
    chown awips:fxalpha "${old_data_dir}"
fi

# move tablespaces
mkdir -p ${new_tablespace_dir}
chown -R awips:fxalpha ${new_tablespace_dir}
chmod 700 ${new_tablespace_dir}
for ts_link in "${pg_tblspc}"/* ; do
    if [[ -h ${ts_link} ]]; then
        this_ts=$(readlink "${ts_link}")
        if [[ "$(dirname "${this_ts}")" != "${new_tablespace_dir}" ]]; then
            mv -v "${this_ts}" "${new_tablespace_dir}" && \
              rm -vf "${ts_link}" && \
              ln -sv "${new_tablespace_dir}/$(basename ${this_ts})" ${ts_link}
            chown awips:fxalpha ${ts_link}
        else
            echo "INFO: ${this_ts} already in correct location"
        fi
    fi
done

echo INFO: Done.
