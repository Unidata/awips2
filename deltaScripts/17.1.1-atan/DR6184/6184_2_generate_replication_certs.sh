#!/bin/bash

# 6184 - This script generates the SSL certificate and key for the replication
# DB user on central registry servers.
#
# Run as root on every central registry server.
# Set the ca_dir variable to the location of ca.crt and ca.key before running
# this script.
#
# Author: tgurney

# Path to directory containing ca.crt and ca.key
ca_dir=
#ca_dir=/etc/pki/a2pgca/ca/
ssl_dir=/awips2/database/ssl
replication_ssl_dir="${ssl_dir}"/replication/$(hostname -s)

if [[ "${ca_dir}" == "" ]]; then
    echo ERROR: ca_dir variable is not set.
    echo Open this script file and set ca_dir to the directory containing
    echo ca.key and ca.crt.
    exit 1
fi

if [[ ! -f "${ca_dir}"/ca.crt || ! -f "${ca_dir}"/ca.key ]]; then
    echo "ERROR: ca.crt or ca.key missing from ${ca_dir}"
    echo Open this script file and set ca_dir to the directory containing
    echo ca.key and ca.crt.
    exit 1
fi

sudo -u awips mkdir -m 700 -p "${replication_ssl_dir}"

cd "${replication_ssl_dir}"

openssl req -new -nodes \
    -subj '/O=AWIPS DB Auth/OU=NCF/CN=replication' \
    -out replication.req \
    -keyout replication.key

openssl x509 -req \
    -in replication.req \
    -out replication.crt \
    -days 1825 \
    -CA "${ca_dir}"/ca.crt \
    -CAkey "${ca_dir}"/ca.key \
    -CAcreateserial

cp "${ssl_dir}"/root.crt .
