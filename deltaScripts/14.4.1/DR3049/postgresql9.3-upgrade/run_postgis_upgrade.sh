#!/bin/bash

source settings.sh

POSTGIS_UPGRADE=${POSTGIS_CONTRIB}/postgis_upgrade_20_minor.sql
RTPOSTGIS_UPGRADE=${POSTGIS_CONTRIB}/rtpostgis_upgrade_20_minor.sql
TOPOLOGY_UPGRADE=${POSTGIS_CONTRIB}/topology_upgrade_20_minor.sql

echo "=== postgis upgrade ===" >> errors.txt
${PSQL} -U ${POSTGRESQL_USER} -f ${POSTGIS_UPGRADE} -d ${1} >> errors.txt 2>&1

echo "=== rtpostgis upgrade ===" >> errors.txt
${PSQL} -U ${POSTGRESQL_USER} -f ${RTPOSTGIS_UPGRADE} -d ${1} >> errors.txt 2>&1

echo "=== topology upgrade ===" >> errors.txt
${PSQL} -U ${POSTGRESQL_USER} -f ${TOPOLOGY_UPGRADE} -d ${1} >> errors.txt 2>&1

exit 0
