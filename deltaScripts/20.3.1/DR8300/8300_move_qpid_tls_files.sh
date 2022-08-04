#!/bin/bash

# Delta script for Omaha #8300
# This script moves qpid's TLS certificates and server key from /awips2/qpid
# to /awips2/qpid/tls. This script also updates the paths to those files in
# /awips2/qpid/config.json.
#
# Run on the qpid server
#
# Author: tgurney

echo "$0: starting"
mkdir -pv -m=700 /awips2/qpid/tls
for file in /awips2/qpid/server.crt /awips2/qpid/server.key /awips2/qpid/root.crt; do
    if [[ -f "${file}" ]]; then
        mv -v "${file}" /awips2/qpid/tls/
    fi
done
sudo chown -vR awips:fxalpha /awips2/qpid/tls
echo "$0: updating config.json"
sed -i 's/${qpid.home_dir}${file.separator}server.crt/${qpid.home_dir}${file.separator}tls${file.separator}server.crt/g' /awips2/qpid/config.json
sed -i 's/${qpid.home_dir}${file.separator}server.key/${qpid.home_dir}${file.separator}tls${file.separator}server.key/g' /awips2/qpid/config.json
sed -i 's/${qpid.home_dir}${file.separator}root.crt/${qpid.home_dir}${file.separator}tls${file.separator}root.crt/g' /awips2/qpid/config.json
echo "$0: finished"

