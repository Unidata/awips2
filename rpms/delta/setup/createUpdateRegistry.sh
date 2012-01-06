#!/bin/bash

# Schema
# ------
# awips2_delta_registry
# {
#    'deltaBuild'  : varchar(10)
#    'deltaID'     : varchar(20)
#    'deltaDesc'   : varchar(255)
#    'dateApplied' : int
#    'timeApplied' : int
# }

# Variables
CONST_CREATE_SQL="CREATE TABLE awips2_delta_registry("
CONST_CREATE_SQL="${CONST_CREATE_SQL} deltaBuild varchar(10), "
CONST_CREATE_SQL="${CONST_CREATE_SQL} deltaID varchar(20), "
CONST_CREATE_SQL="${CONST_CREATE_SQL} deltaDesc varchar(255), "
CONST_CREATE_SQL="${CONST_CREATE_SQL} dateApplied int, "
CONST_CREATE_SQL="${CONST_CREATE_SQL} timeApplied int);"

# Create The Directory If Necessary.
if [ ! -d ${CONST_REGISTRY_DIR} ]; then
   mkdir -p ${CONST_REGISTRY_DIR}
fi

# Create Our Empty DB File.
touch ${CONST_REGISTRY_DIR}/${CONST_REGISTRY_DB_FILE}

# Create The Table.
echo ${CONST_CREATE_SQL} | sqlite3 ${CONST_REGISTRY_DIR}/${CONST_REGISTRY_DB_FILE}
