#!/bin/bash

# This script adds the updatetime column to the ebxml schema tables
# that inherit from RegistryObjectType class.
# Run as root on Postgres server. Postgres must be running for this to
# work. 
#
#
# Author: skabasele 


echo INFO: Adding updateTime column

has_column() {
    table_name=${1}
    column_name=${2}
    result=$( /awips2/psql/bin/psql --user=awipsadmin --db=metadata -Aqtc "
		select 1  
		FROM  information_schema.columns 
		WHERE table_schema='ebxml' AND table_name='${table_name}' AND column_name='${column_name}'; " )
	
    [[ ${result} == "1" ]]
    return $?   
}




add_updatetime_column() {
    table="$1"
    column="updatetime"
    has_column $table $column   
    if [[ $? != 0 ]]; then
       echo INFO: Adding column $column to the ebxml.$table table
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
    		ALTER TABLE ebxml.$table ADD COLUMN $column TIMESTAMP without time zone DEFAULT NOW() NOT NULL;
EOF
    else
         echo INFO: $column column already exists in  the ebxml.$table table
    fi
}


add_updatetime_column registryobject
add_updatetime_column association
add_updatetime_column auditableevent
add_updatetime_column classification
add_updatetime_column classificationnode
add_updatetime_column classificationscheme
add_updatetime_column comment
add_updatetime_column externalidentifier

add_updatetime_column externallink
add_updatetime_column extrinsicobject
add_updatetime_column federation
add_updatetime_column notification
add_updatetime_column organization
add_updatetime_column person
add_updatetime_column querydefinition
add_updatetime_column registrypackage

add_updatetime_column registry
add_updatetime_column role
add_updatetime_column servicebinding
add_updatetime_column serviceendpoint
add_updatetime_column serviceinterface
add_updatetime_column service 
add_updatetime_column subscription
add_updatetime_column workflowaction

