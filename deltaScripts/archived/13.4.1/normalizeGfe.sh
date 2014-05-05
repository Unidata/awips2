#!/bin/bash
# Main script for updating GFE database structure

PSQL="/awips2/psql/bin/psql"
PYTHON="/awips2/python/bin/python"

SQL_SCRIPT="createNewGfeTables.sql"

# ensure that the sql script is present
if [ ! -f ${SQL_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${SQL_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi


echo "Creating new GFE tables"
${PSQL} -U awips -d metadata -f ${SQL_SCRIPT}
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

echo 
echo "Querying GFE parmIds"
RETRIEVE_PARMIDS_SQL="SELECT distinct parmId FROM gfe order by parmID"
_parmid_list_txt=parmIdList.txt

${PSQL} -U awips -d metadata -c "${RETRIEVE_PARMIDS_SQL}" -t -o ${_parmid_list_txt}
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to retrieve the list of parm ids."
   echo "FATAL: The update has failed."
   exit 1
fi

echo 
echo "Parsing parmIds for insertion into new tables"
PYTHON_PARSE_SCRIPT="parseParmIds.py"
if [ ! -f ${PYTHON_PARSE_SCRIPT} ]; then
   echo "ERROR: the required python script - ${PYTHON_PARSE_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi
${PYTHON} ${PYTHON_PARSE_SCRIPT} ${_parmid_list_txt}
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to parse parm ids."
   echo "FATAL: The update has failed."
   exit 1
fi

echo 
echo "Inserting db ids"
# dbIdInserts.sql generated from parseParmIds.py
${PSQL} -U awips -d metadata -q -f dbIdInserts.sql
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to insert database ids."
   echo "FATAL: The update has failed."
   exit 1
fi

echo 
echo "Inserting parm ids"
# parmIdInserts.sql generated from parseParmIds.py
${PSQL} -U awips -d metadata -q -f parmIdInserts.sql
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to insert parm ids."
   echo "FATAL: The update has failed."
   exit 1
fi

echo 
echo "Add gfe record reference to parm id table"
# gfeToParmIdUpdates.sql generated from parseParmIds.py
${PSQL} -U awips -d metadata -q -f gfeToParmIdUpdates.sql
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to add gfe to parm id mapping."
   echo "FATAL: The update has failed."
   exit 1
fi

echo 
echo "Updating constraints and indexes on gfe"
SQL_SCRIPT="updateGfeConstraintsAndIndexes.sql"
${PSQL} -U awips -d metadata -f ${SQL_SCRIPT}
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to update constraints and indexes."
   echo "FATAL: The update has failed."
   exit 1
fi


echo 
echo "Updating dataURIs for gfe"
UPDATE_DATAURIS_SQL="UPDATE gfe SET dataURI =regexp_replace(dataURI, '(/gfe/[^/]+)/([^_]+)_([^:]+):([^_]+)_GRID_([^_]*)_([^_]+)_(\\d{8}_\\d{4})/[^/]+', '\\1/\\4/\\6/\\7/\\5/\\2/\\3') where dataURI ~ '/gfe/[^/]+/[^/]+/[^/]+';"
${PSQL} -U awips -d metadata -c "${UPDATE_DATAURIS_SQL}"
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to retrieve the list of parm ids."
   echo "FATAL: The update has failed."
   exit 1
fi

echo 
echo "Running full vacuum for gfe"
${PSQL} -U awips -d metadata -c "VACUUM FULL VERBOSE ANALYZE gfe"
