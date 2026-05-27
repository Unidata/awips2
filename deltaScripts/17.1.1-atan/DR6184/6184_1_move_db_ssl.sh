#!/bin/bash

# 6184 - This script moves Postgres SSL files from /awips2/data to
# /awips2/database/ssl and creates symlinks from the old locations to
# the new. Finally it updates postgresql.conf to refer to the new
# location.
#
# Run as root on dx1.
#
# Author: tgurney

ssl_dir=/awips2/database/ssl
ssl_dir_escaped=$(echo "${ssl_dir}" | sed 's/\//\\\//g')
data_dir=/awips2/data
temp_conf=$(mktemp)

echo INFO: Starting move of SSL files

mkdir -pv "${ssl_dir}"
chown -R awips:fxalpha "${ssl_dir}"
chmod 700 "${ssl_dir}"

didstuff=false

for file in "${data_dir}"/*.{crt,key}; do
    if [[ -f "${file}" ]]; then
        mv -v "${file}" "${ssl_dir}"
        ln -fsv "${ssl_dir}"/"$(basename "${file}")"  "${data_dir}"
        didstuff=true
    fi
done

cat "${data_dir}/postgresql.conf" \
  | perl -pe 's/^\#?\s*ssl_cert_file\s*=.*$/ssl_cert_file = '"'${ssl_dir_escaped}"'\/server.crt'"'/g" \
  | perl -pe 's/^\#?\s*ssl_key_file\s*=.*$/ssl_key_file = '"'${ssl_dir_escaped}"'\/server.key'"'/g" \
  | perl -pe 's/^\#?\s*ssl_ca_file\s*=.*$/ssl_ca_file = '"'${ssl_dir_escaped}"'\/root.crt'"'/g" \
  > ${temp_conf}

cmp -s "${data_dir}/postgresql.conf" "${temp_conf}"
if [[ "$?" -eq 0 ]]; then
    echo INFO: No postgresql.conf update needed
else
    echo INFO: Updating postgresql.conf
    now=$(date +%Y%m%d.%H%M%S)
    cp -a "${data_dir}/postgresql.conf" "${data_dir}"/postgresql.conf."${now}" && \
        echo INFO: Old postgresql.conf saved at "${data_dir}"/postgresql.conf."${now}"
    install -T -m 600 -o awips -g fxalpha ${temp_conf} "${data_dir}/postgresql.conf"
fi

echo INFO: Done
