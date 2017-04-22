#!/bin/bash

# DR #5801 - This script drop the afos_to_awips table and creates a site
# override for afos2awips.txt if needed.
#
# afoslookup table is also dropped since it is no longer used anywhere.

# Run this script only on dx1 as root.

# get site identifier from dx3
ENV_TMP=$(mktemp || exit 1)
ssh dx3 -- 'cat /awips2/edex/bin/setup.env | grep AW_SITE_IDENTIFIER' > "${ENV_TMP}"
source "${ENV_TMP}"
rm -f "${ENV_TMP}"

if [[ -z "${AW_SITE_IDENTIFIER}" ]]; then
    echo "ERROR: could not determine the current site identifier"
    exit 1
fi

if [[ $(id -u) -ne 0 ]]; then
    echo "ERROR: you need to be root."
    exit 1
fi

cleanup_exit() {
    # remove temp files
    for file in "${FROM_DB}" "${FROM_BASE}" "${DIFF_FILE}" "${TMP_SITE_FILE}" \
                "${DB_DUMP_FILE}" "${SITE_FILE}~" "${SITE_BLACKLIST_FILE}~" \
                "${REMOVE_FILE}"; do
        if [[ ! -z "${file}" ]]; then
            rm -f "${file}"
        fi
    done
    exit $1
}

PSQL="/awips2/psql/bin/psql"
LOCALIZATION_ROOT="/awips2/edex/data/utility/common_static"
SITE_DIR="${LOCALIZATION_ROOT}/site/${AW_SITE_IDENTIFIER}/afos2awips"
SITE_FILE="${SITE_DIR}/afos2awips.txt"
SITE_BLACKLIST_FILE="${SITE_DIR}/afos2awips.blacklist.txt"
BASE_FILE="/awips2/edex/data/utility/common_static/base/afos2awips/afos2awips.txt"
DB_DUMP_FILE=$(mktemp) || cleanup_exit 1
FROM_DB=$(mktemp) || cleanup_exit 1
FROM_BASE=$(mktemp) || cleanup_exit 1
DIFF_FILE=$(mktemp) || cleanup_exit 1
REMOVE_FILE=$(mktemp) || cleanup_exit 1
TMP_SITE_FILE=$(mktemp) || cleanup_exit 1
TMP_SITE_BLACKLIST_FILE=$(mktemp) || cleanup_exit 1
DB_BACKUP="/tmp/afos2awips.txt.$(date +%Y%m%d)"

TABLE_EXISTS=$("${PSQL}" --user=awipsadmin --db=fxatext -tA -c "
    select 1 from information_schema.tables where table_catalog = 'fxatext' and table_name = 'afos_to_awips';
")

if [[ "${TABLE_EXISTS}" -ne 1 ]]; then
    echo "WARN: afos_to_awips table does not exist; nothing to do."
    cleanup_exit 0
fi

# Dump afos_to_awips table to a file
echo "INFO: Dumping afos_to_awips_table to ${DB_DUMP_FILE}"
"${PSQL}" -tA -U awips -d fxatext -c "
    select afosid, wmottaaii, wmocccc
    from afos_to_awips
    order by afosid, wmottaaii, wmocccc;
" > "${DB_DUMP_FILE}" || cleanup_exit 1

cat "${DB_DUMP_FILE}" | sed 's/|/ /g' | sed 's/\s\s*/ /g' | sed 's/\s\s*$//g' | sort > "${FROM_DB}"
sort "${BASE_FILE}" | sed 's/\s\s*$//g' > "${FROM_BASE}"

# get only the records in database that are not in the base file
comm -23 "${FROM_DB}" "${FROM_BASE}" > "${DIFF_FILE}"

# get records in base file that are not in database
comm -13 "${FROM_DB}" "${FROM_BASE}" > "${REMOVE_FILE}"

if [[ -f "${SITE_FILE}" ]]; then
    cat "${SITE_FILE}" "${DIFF_FILE}" | sort | uniq > "${TMP_SITE_FILE}"
else
    cat "${DIFF_FILE}" | sort | uniq > "${TMP_SITE_FILE}"
fi

if [[ -f "${SITE_BLACKLIST_FILE}" ]]; then
    cat "${SITE_BLACKLIST_FILE}" "${REMOVE_FILE}" | sort | uniq > "${TMP_SITE_BLACKLIST_FILE}"
else
    cat "${REMOVE_FILE}" | sort | uniq > "${TMP_SITE_BLACKLIST_FILE}"
fi

echo "INFO: Copying new site override to ${SITE_FILE}"
mkdir -pv "${SITE_DIR}" || cleanup_exit 1
chown awips:fxalpha "${SITE_DIR}"
chmod 775 "${SITE_DIR}"
install -bv -o awips -g fxalpha -m 664 "${TMP_SITE_FILE}" "${SITE_FILE}" || cleanup_exit 1
install -bv -o awips -g fxalpha -m 664 "${TMP_SITE_BLACKLIST_FILE}" "${SITE_BLACKLIST_FILE}" || cleanup_exit 1

echo "INFO: Removing afos_to_awips table"

"${PSQL}" -tA -U awipsadmin -d fxatext -c "drop table if exists afos_to_awips;"

echo "INFO: Removing afoslookup table"

"${PSQL}" -tA -U awipsadmin -d fxatext -c "drop table if exists afoslookup;"

cp "${FROM_DB}" "${DB_BACKUP}"

echo "INFO: Contents of afos_to_awips table were saved to ${DB_BACKUP}"
echo "INFO: Done."
cleanup_exit 0
