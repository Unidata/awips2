#!/bin/bash

DUMP_FILE="/tmp/dump.sql"
SCHEMA_DEFINITION_SCRIPT="ebxmlSchemaDefinition.sql"
TABLE_NAME_UPDATE_SCRIPT="tableNameUpdate.sql"

# ensure that the schema definition script is present
if [ ! -f ${SCHEMA_DEFINITION_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${SCHEMA_DEFINITION_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

# ensure that the table name update script is present
if [ ! -f ${TABLE_NAME_UPDATE_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${TABLE_NAME_UPDATE_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

echo -n "Modifying table names to conform to new schema..."
psql -U awips -d ebxml -f $TABLE_NAME_UPDATE_SCRIPT > /dev/null
echo "Done!"

echo -n "Dumping existing ebxml database contents..."
pg_dump --port 5432 --username "awips" --role "awips" --no-password  --format plain --data-only --inserts --column-inserts --file  $DUMP_FILE "ebxml"
if [ $? -ne 0 ]; then
   echo "FATAL: Failed to dump existing database contents!"
   echo "FATAL: the update has failed!"
   exit 1
fi
echo "Done!"

echo -n "Setting search path in dump file to be ebxml..."
sed -i 's/SET search_path =.*/SET search_path TO ebxml, pg_catalog;/g' $DUMP_FILE
if [ $? -ne 0 ]; then
   echo "FATAL: Failed to reset search path in dump file!"
   echo "FATAL: the update has failed!"
   exit 1
fi
echo "Done!"

echo -n "Removing references to versioninfo table..."
sed -i "s/INSERT INTO versioninfo/--INSERT INTO versioninfo/g" $DUMP_FILE
if [ $? -ne 0 ]; then
   echo "FATAL: Removing references to versioninfo table has failed!"
   echo "FATAL: the update has failed!"
   exit 1
fi
echo "Done!"

echo -n "Updating column names for version info columns..."
sed -i "s/versioninfo_versionname/versionname/g" $DUMP_FILE
if [ $? -ne 0 ]; then
   echo "FATAL: Updating version info column names has failed!"
   echo "FATAL: the update has failed!"
   exit 1
fi

sed -i "s/versioninfo_userversionname/userversionname/g" $DUMP_FILE
if [ $? -ne 0 ]; then
   echo "FATAL: Updating version info column names has failed!"
   echo "FATAL: the update has failed!"
   exit 1
fi
echo "Done!"

#Redirect standard out to null, but keep standard error for the following psql commands
echo -n "Adding tables to ebxml schema..."
psql -U awips -d metadata -f $SCHEMA_DEFINITION_SCRIPT > /dev/null
if [ $? -ne 0 ]; then
   echo "FATAL: Exporting new ebxml schema has failed!"
   echo "FATAL: the update has failed!"
   exit 1
fi
echo "Done!"

echo -n "Getting current hibernate_sequence value to update new sequences..."
sequenceValue=$(psql -t -U awips -d ebxml -c "SELECT nextval('hibernate_sequence');")
echo "Done!"

echo "Current hibernate_sequence value is: $sequenceValue"

for seq in 'action' 'deliveryinfo' 'emailaddress' 'internationalstring' 'localizedstring' 'map' 'objectreflist' 'parameter' 'postaladdress' 'queryexpression' 'registryobjectlist' 'simplelink' 'slot' 'telephonenumber' 'value'
do
	echo -n "Updating sequence ebxml.${seq}_sequence with value $sequenceValue to avoid key violations.."
	psql -U awips -d metadata -c "SELECT pg_catalog.setval('ebxml.${seq}_sequence', $sequenceValue, true);" > /dev/null
	if [ $? -ne 0 ]; then
   		echo "FATAL: Updating sequence ${seq} has failed!"
   		echo "FATAL: The update has failed!"
   		exit 1
       	fi
	echo "Done!"
done


echo -n "Removing references to hibernate_sequence..."
sed -i "s/SELECT pg_catalog.setval('hibernate_sequence'/--SELECT pg_catalog.setval('hibernate_sequence'/g" $DUMP_FILE
if [ $? -ne 0 ]; then
   echo "FATAL: Removal of references to hibernate_sequence has failed!"
   echo "FATAL: the update has failed!"
   exit 1
fi
echo "Done!"

echo -n "Populating ebxml schema with existing database contents..."
psql -U awips -d metadata -f $DUMP_FILE > /dev/null
if [ $? -ne 0 ]; then
   echo "FATAL: Populating ebxml schema with existing data has failed!"
   echo "FATAL: the update has failed!"
   exit 1
fi
echo "Done!"

echo -n "Removing dump file: $DUMP_FILE..."
rm -f $DUMP_FILE
if [ $? -ne 0 ]; then
   echo "Warn: File $DUMP_FILE has not been removed. Clean up manually. Update still successful."

else
   echo "Done!"
fi


echo "Ebxml database schema update successful!"
exit 0

