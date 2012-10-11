#!/bin/bash
# This script will add register the gridcoverage plugin, which was previously part of grib
#
# This needs to be performed with build ????
#

PSQL="/awips2/psql/bin/psql"
SQL_COMMAND_CHECK="select * FROM gridcoverage LIMIT 1;"
SQL_COMMAND_REGISTER="insert into plugin_info (name, database, initialized, tablename) VALUES('gridcoverage', 'metadata', TRUE, 'gridcoverage');"
SQL_COMMAND_SEQ="CREATE SEQUENCE gridcoverage_seq INCREMENT 1 MINVALUE 1 MAXVALUE 9223372036854775807 START 1 CACHE 1; ALTER TABLE gridcoverage_seq OWNER TO awips;"
SQL_COMMAND_UPDATE_ID="update gridcoverage set id=nextval('gridcoverage_seq');"
SQL_COMMAND_ALTER_NAME_DESC="ALTER TABLE gridcoverage DROP COLUMN description, ALTER COLUMN name TYPE character varying(255);"
SQL_COMMAND_UPDATE_NAME="update gridcoverage g1 set name = 'Subgrid-' || g2.id from gridcoverage g2 where g1.name like '%-SubGrid-%' and g2.name = split_part(g1.name, '-',1);"

if [ ! -f ${PSQL} ]; then
echo "ERROR: The PSQL executable does not exist - ${PSQL}."
echo "FATAL: Update Failed!"
exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_CHECK}" > /dev/null
if [ $? -ne 0 ]; then
echo "WARN: gridcoverage table does not exist so we are not registering the plugin"
exit 0
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_REGISTER}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_SEQ}"
if [ $? -ne 0 ]; then
echo "FATAL: unable to create gridcoverage_seq"
exit 1
fi

FK=`${PSQL} -U awips -d metadata -c "\d grib_models" | grep gridcoverage | awk -F"\"" '{print $2}'`
if [ -z "$FK" ]; then
echo "FATAL: unable to find foreign key constraint on grib_models"
exit 1
fi

${PSQL} -U awips -d metadata -c "ALTER TABLE grib_models DROP CONSTRAINT ${FK}, ADD CONSTRAINT ${FK} FOREIGN KEY (location_id) REFERENCES gridcoverage (id) MATCH SIMPLE ON UPDATE CASCADE ON DELETE NO ACTION;"
if [ $? -ne 0 ]; then
echo "FATAL: unable to modify foreign key constraint on grib_models"
exit 1
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_UPDATE_ID}"
if [ $? -ne 0 ]; then
echo "FATAL: unable to update gridcoverage ids"
exit 1
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_ALTER_NAME_DESC}"
if [ $? -ne 0 ]; then
echo "WARN: unable to remove description column from gridcoverage table"
fi

echo "INFO: The update was successfully applied."
exit 0
