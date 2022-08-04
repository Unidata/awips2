#!/bin/bash
# DR #3702 Copy text plugin_info to the fxatext database.

PSQL="/awips2/psql/bin/psql"

echo "INFO: Copying text plugin_info to fxatext database."

initialized=`$PSQL -t -U awips -d metadata -c "SELECT initialized FROM plugin_info WHERE name = 'text';"`
if [ -n "$initialized" ]; then
  $PSQL -U awips -d fxatext -c "CREATE TABLE IF NOT EXISTS plugin_info(name character varying(255) NOT NULL, initialized boolean,tablename character varying(255),CONSTRAINT plugin_info_pkey PRIMARY KEY (name)); ALTER TABLE plugin_info OWNER TO awips;"
  if [ $? -ne 0 ]; then
    echo "ERROR: Failed to create plugin_info table in fxatext database"
    echo "FATAL: The update has failed."
    exit 1
  fi
  $PSQL -U awips -d fxatext -c "INSERT INTO plugin_info (name,initialized) VALUES ('text','$initialized');"
  if [ $? -ne 0 ]; then
    echo "ERROR: Failed to register text plugin in fxatext database."
    echo "FATAL: The update has failed."
    exit 1
  fi
  $PSQL -t -U awips -d metadata -c "DELETE FROM plugin_info WHERE name = 'text';"
  
else
  echo "INFO: Nothing to do."
fi

${PSQL} -U awips -d metadata -c "ALTER TABLE plugin_info DROP COLUMN IF EXISTS database;"

echo "INFO: Done copying text plugin_info to fxatext database."