#!/bin/bash

# Omaha #7878 upgrade AvnFPS to Python 3
#
# This script updates all AvnFPS monitor configuration files to remove any
# duplicate config options, which are no longer allowed in Python 3.

echo INFO: Starting $0

script=$(mktemp || exit 1)
cleanup_exit() {
    rm -f "${script}"
    exit 0
}
trap cleanup_exit SIGTERM
trap cleanup_exit SIGINT

cat > "${script}" << 'EOF'
#!/awips2/python/bin/python3
import configparser
import sys

file = sys.argv[1]
config = configparser.SafeConfigParser(strict=False)
config.read(file)
with open(file, 'w') as f:
    config.write(f)
EOF

shopt -s nullglob

for file in /awips2/edex/data/utility/common_static/{site,user}/*/aviation/config/tafs/*/{ccfp,ltg,rltg,llws,mtrs,grids}.cfg; do
    bakfile="${file}.DR7878.bak"
    if [[ ! -f "${bakfile}" ]]; then
        cp -a "${file}" "${bakfile}" || cleanup_exit
    fi

    python3 "${script}" "${file}"

    if diff -q "${file}" "${bakfile}" >/dev/null; then
        echo INFO: ${file} was not modified
        rm -f "${bakfile}"
    else
        echo "INFO: "${file}" was modified, backup is at ${bakfile}"
    fi
done

echo INFO: Finished $0
cleanup_exit
