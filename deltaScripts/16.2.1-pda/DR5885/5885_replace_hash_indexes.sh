#!/bin/bash

# DR #5885: This script will drop all hash indexes that are on a primary
# key, and replace all other hash indexes with B-tree indexes.

PSQL="/awips2/psql/bin/psql"
DB_SUPERUSER=awips
INDEXES_FILE=$(mktemp || exit 1)
SCRIPT_FILE=$(mktemp || exit 1)

cleanup_exit() {
    echo "Cleaning up."
    if [[ -f "${INDEXES_FILE}" ]]; then
        rm -f "${INDEXES_FILE}"
    fi
    if [[ -f "${SCRIPT_FILE}" ]]; then
        rm -f "${SCRIPT_FILE}"
    fi
    exit $1
}

DATABASES=$("${PSQL}" --db metadata -U ${DB_SUPERUSER} -Aqtc "
    select datname
    from pg_database
    where datistemplate = false
    and datname not in ('awips', 'postgres')
    ") || cleanup_exit 1

for DBNAME in ${DATABASES}; do
    "${PSQL}" --user="${DB_SUPERUSER}" --db=${DBNAME} -Aqt << EOF \
      | sort > "${INDEXES_FILE}" || cleanup_exit 1
        select schemaname || '."' || indexname || E'"\\t' || indexdef
        from pg_indexes
        where schemaname <> 'pg_catalog'
        and indexdef ilike '%using hash%'
        and indexdef not ilike 'create unique index%';
EOF
    # Write script to temp file
    echo "BEGIN;" > "${SCRIPT_FILE}" || cleanup_exit 1

    # add DROP INDEX statements
    cut -f1 "${INDEXES_FILE}" \
      | awk '{print "DROP INDEX IF EXISTS " $0 ";"}' >> "${SCRIPT_FILE}" \
      || cleanup_exit 1

    # add CREATE INDEX statements
    # we do not want to create any indexes on (id) since they will already
    # have a unique index
    cut -f2 "${INDEXES_FILE}" \
      | grep -iv "using hash (id)" \
      | perl -pe 's/stringvalue_index ON ebxml.value (using|USING) hash/stringvalue_index ON ebxml.value using spgist/g' \
      | perl -pe 's/(USING|using) hash \(/USING btree \(/i' \
      | awk '{print $0 ";"}' >> "${SCRIPT_FILE}" \
      || cleanup_exit 1

    echo "COMMIT;" >> "${SCRIPT_FILE}" || cleanup_exit 1

    # Run script
    if [[ "$(wc -l ${SCRIPT_FILE} | cut -d' ' -f1)" -gt 2 ]]; then
        echo INFO: Updating ${DBNAME}.
        "${PSQL}" --user="${DB_SUPERUSER}" --db=${DBNAME} -f "${SCRIPT_FILE}"
    else
        echo INFO: Nothing to do for ${DBNAME}.
    fi
done
