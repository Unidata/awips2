#!/bin/bash

# This script updates the id column of the ebxml registry schema tables that inherit from ExtensibleObjectType class
# to be VarChars. 
#
# NOTE: This script should ONLY be ran if you want to revert the changes of the other deltascript from this ticket 
#       that updates the ids to be BigInts. This does NOT need to be ran as part of a normal upgrade. 
#       Only run the update_Registry_IdColumn_ToBigInt.sh
#
# Run as root on Postgres server. Postgres must be running for this to work.
#
# Author: Tim Jensen

#global variables
get_foreignKey_name_returnValue=""
get_uniqueKey_name_returnValue=""

# return the foreign key name corresponding to the tables and columns passed
get_foreignKey_name() {
    table_name=${1}
    fk_column=${2}
    get_foreignKey_name_returnValue=$( /awips2/psql/bin/psql --user=awipsadmin --db=metadata -Aqtc "
        select kcu.constraint_name
        from information_schema.table_constraints tco
        join information_schema.key_column_usage kcu
        on tco.constraint_schema = kcu.constraint_schema
        and tco.constraint_name = kcu.constraint_name
        join information_schema.referential_constraints rco
        on tco.constraint_schema = rco.constraint_schema
        and tco.constraint_name = rco.constraint_name
        join information_schema.key_column_usage rel_kcu
        on rco.unique_constraint_schema = rel_kcu.constraint_schema
        and rco.unique_constraint_name = rel_kcu.constraint_name
        and kcu.ordinal_position = rel_kcu.ordinal_position
        where tco.constraint_type = 'FOREIGN KEY'  and kcu.column_name = '${fk_column}'
        and  kcu.table_name = '${table_name}' and kcu.table_schema = 'ebxml'; " )
}


# return the unique key name corresponding to the table passed
get_uniqueKey_name() {
    table_name=${1}
    get_uniqueKey_name_returnValue=$( /awips2/psql/bin/psql --user=awipsadmin --db=metadata -Aqtc "
        SELECT conname
        FROM pg_constraint
        WHERE conrelid =
            (SELECT oid
             FROM pg_class
             WHERE relname LIKE '${table_name}') and conname like 'uk_%'; " )
}

#update the current table foreign key id values to the id value of the foreign key table
update_foreignKey_value() {
    current_table=${1}
    current_id=${2}
    current_bid=${3}
    foreignKey_table=${4}
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        update  ebxml.$current_table  set $current_id = ebxml.${foreignKey_table}.id  from  ebxml.${foreignKey_table}  where ${foreignKey_table}.bid = ${current_table}.${current_bid};
EOF
}

# Remove orphaned records from specific join tables.
# The orphaned record are the values that existed in the join table
# but for which the corresponding value didn't exist in the referenced tables
#Important to remove them  when the joint table has a composite primary key to rebuild
remove_orphaned_records(){
    table=${1}
    first_column=${2}
    second_column=${3}
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        delete from ebxml.$table where $first_column is null or $second_column is null;
EOF
}

# drop and recreate the foreign key operations
rebuildForeignKey() {
    fk_table=${1}
    fk_column=${2}
    fk_constraint_name=${3}
    fk_column_bid=${4}
    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_${table}_${fk_column}
    fi
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table drop constraint if exists $fk_constraint_name;
        alter table ebxml.$table drop column  if exists $fk_column;
        alter table ebxml.$table add column $fk_column character varying(1024);
        alter table ebxml.$table add constraint $fk_constraint_name FOREIGN KEY ($fk_column)  REFERENCES ebxml.${fk_table} (id);
EOF
       update_foreignKey_value "$table" "$fk_column" "$fk_column_bid" "$fk_table"
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
    \set ON_ERROR_STOP on
        alter table ebxml.$table drop column $fk_column_bid;
EOF
}

#check column in information schema for specified type
checkColumnforType(){
    table_name=${1}
    dataType=${2}
    column=${3}
    result=$( /awips2/psql/bin/psql --user=awipsadmin --db=metadata -Aqtc "
        select trim(data_type)
        FROM  information_schema.columns
        WHERE table_schema='ebxml' AND table_name='${table_name}' AND column_name='${column}'; " )

    [[ "${dataType}" == "${result}" ]]
    return $?
}

# check if the id column is still of a varchar datatype
has_varChar_id_column() {
    table_name=${1}
    column="id"
    datatype="character varying"
    return_value=$(checkColumnforType $table_name "$datatype" $column)
    return $return_value
}

#check if id column is bigInt
has_bigint_id_column() {
    table_name=${1}
    column="id"
    datatype="bigint"
    return_value=$(checkColumnforType "$table_name" $datatype $column)
    return $return_value
}

# check if the specified column exists
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

#  basic operations needed to update id column when only a primary key exist
update_id_baseOperations() {
    table=${1}
    sequence=ebxml.extensibleObject_seq
    if has_bigint_id_column $table;
    then
        echo INFO: Ebxml.$table being updated to varchar...
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            update ebxml.$table set bid=id;
            alter table ebxml.$table drop constraint if exists ${table}_pkey cascade;
            alter table ebxml.$table drop column if exists id;
            alter table ebxml.$table add column id character varying(1024) not null default nextval('${sequence}');
            alter table ebxml.$table add constraint ${table}_pkey primary key (id);
EOF
    else
        echo INFO: Ebxml.$table id column already updated to varchar. Skipping...
    fi
    if has_column $table "lid";
    then
        echo INFO: Ebxml."$table" copying lids to ids
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on    
             update ebxml.$table set id=lid;
EOF
    fi
    
}

# Update id column of the registryObject table or its descendant
# that only has a primary key constraint (no foreign Key constraint) to update
update_id_of_registryObject_child_modify_pk_only() {
    table="$1"
    if ! has_column $table "bid";
    then
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            alter table ebxml.$table add column bid bigint;
EOF
    fi
    update_id_baseOperations $table
}

# basic operations needed to update id column when a foreign key exist.
update_id_baseOperations_with_fk() {
    table=${1}
    fk_table=${2}
    fk_column=${3}
    fk_column_bid=${fk_table}_bid
    sequence=ebxml.extensibleObject_seq
    get_foreignKey_name "$table" "$fk_column"
    fk_constraint_name=$get_foreignKey_name_returnValue

    if has_bigint_id_column "$table";
    then
        echo INFO: Ebxml."$table" being updated to varchar
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            alter table ebxml.$table add column bid bigint;
            alter table ebxml.$table add column $fk_column_bid bigint;
            update ebxml.$table set $fk_column_bid=$fk_column;
            update ebxml.$table set bid=id;
            alter table ebxml.$table drop constraint if exists ${table}_pkey cascade;
            alter table ebxml.$table drop column if exists id;
            alter table ebxml.$table add column id character varying(1024) not null default nextval('${sequence}');
            alter table ebxml.$table add constraint ${table}_pkey primary key (id);

EOF
        rebuildForeignKey "$fk_table" "$fk_column" "$fk_constraint_name" "$fk_column_bid"
    else
        echo INFO: Ebxml."$table" id column already updated to varchar. Skipping...
    fi
    
    if has_column $table "lid";
    then
        echo INFO: Ebxml."$table" copying lids to ids
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on    
             update ebxml.$table set id=lid;
EOF
    fi
}

# Update id column of the registryObject table or its descendant
# that has both a primary key constraint and a foreign Key constraint to update
update_id_of_registryObject_child_has_pk_and_fk() {
    table="$1"
    fk_table="$2"
    fk_column="$3"
    update_id_baseOperations_with_fk "$table"  "$fk_table" "$fk_column"
}


# Update id column of the ExtensibleObject table or its descendant
# that has both a primary key constraint and a foreign Key constraint to update
update_id_of_extensibleObject_child_has_pk_and_fk() {
    table="$1"
    fk_table="$2"
    fk_column="$3"
    has_column $table "bid"
    if ! has_column $table "bid";
    then
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table add column bid character varying(1024);
EOF
    fi
    update_id_baseOperations_with_fk "$table"  "$fk_table" "$fk_column"

}

# common operations needed to update hyphenated tables
update_id_hypenatesTables_basic_operations(){
    table="$1"
    first_table="$2"
    first_column="$3"
    other_table="$4"
    other_column="$5"
    first_column_bid=${first_table}_bid_1
    other_column_bid=${other_table}_bid_2
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table add column $first_column_bid bigint;
        alter table ebxml.$table add column $other_column_bid bigint;
        update ebxml.$table set $first_column_bid=$first_column;
        update ebxml.$table set $other_column_bid=$other_column;
        alter table ebxml.$table drop column if exists $first_column;
        alter table ebxml.$table drop column if exists $other_column;
        alter table ebxml.$table add column $first_column character varying(1024);
        alter table ebxml.$table add column $other_column character varying(1024);
EOF
    update_foreignKey_value "$table"  "$first_column" "$first_column_bid" "$first_table"
    update_foreignKey_value "$table"  "$other_column" "$other_column_bid" "$other_table"
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table drop column if exists $first_column_bid;
        alter table ebxml.$table drop column if exists $other_column_bid;
EOF
}

# Updates the id of hyphenated tables that has a unique key,composite primary key and foreign key
update_id_hyphenatedTables_with_uk_compositePk_fk() {
    table="$1"
    fk_table="$2"
    fk_column="$3"
    other_table="$4"
    other_column="$5"
    uk_column="$6"

    get_foreignKey_name $table "$fk_column"
    fk_constraint_name=$get_foreignKey_name_returnValue
    get_uniqueKey_name $table
    uk_constraint_name=$get_uniqueKey_name_returnValue
    pk_constraint_name=${table}_pkey

    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_${table}_${fk_column}
    fi

    if [[ "${uk_constraint_name}" == "" ]]; then
        uk_constraint_name=uk_${table}_${uk_column}
    fi
    echo INFO: Ebxml."$table" being updated
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table drop constraint if exists $pk_constraint_name cascade;
        alter table ebxml.$table drop constraint if exists $fk_constraint_name;
        alter table ebxml.$table drop constraint if exists $uk_constraint_name;
EOF
    update_id_hypenatesTables_basic_operations "$table" "$fk_table" "$fk_column" "$other_table" "$other_column"
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table add constraint $fk_constraint_name FOREIGN KEY ($fk_column)  REFERENCES ebxml.$fk_table (id);
        alter table ebxml.$table add UNIQUE ($uk_column);
EOF

}

# update id for hyphenated tables that has one unique key and 2 foreign keys
update_id_hyphenatedTables_with_uk_2fk() {
    table="$1"
    first_fk_table="$2"
    first_fk_column="$3"
    second_fk_table="$4"
    second_fk_column="$5"
    uk_column="$6"

    get_foreignKey_name $table $first_fk_column
    first_fk_constraint_name=$get_foreignKey_name_returnValue
    get_foreignKey_name $table $second_fk_column
    second_fk_constraint_name=$get_foreignKey_name_returnValue
    get_uniqueKey_name $table
    uk_constraint_name=$get_uniqueKey_name_returnValue

    if [[ "${first_fk_constraint_name}" == "" ]]; then
        first_fk_constraint_name=fk_${table}_${first_fk_column}
    fi

    if [[ "${second_fk_constraint_name}" == "" ]]; then
        second_fk_constraint_name=fk_${table}_${second_fk_column}
    fi

    if [[ "${uk_constraint_name}" == "" ]]; then
        uk_constraint_name=uk_${table}_${uk_column}
    fi

    echo INFO: Ebxml."$table" being updated
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table drop constraint if exists $first_fk_constraint_name;
        alter table ebxml.$table drop constraint if exists $second_fk_constraint_name;
        alter table ebxml.$table drop constraint if exists $uk_constraint_name;
EOF
    update_id_hypenatesTables_basic_operations "$table" "$first_fk_table" "$first_fk_column" "$second_fk_table" "$second_fk_column"
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table add constraint $first_fk_constraint_name FOREIGN KEY ($first_fk_column)  REFERENCES ebxml.$first_fk_table (id);
        alter table ebxml.$table add constraint $second_fk_constraint_name FOREIGN KEY ($second_fk_column)  REFERENCES ebxml.$second_fk_table (id);
        alter table ebxml.$table add constraint $uk_constraint_name UNIQUE ($uk_column);
EOF
}

# update id of hyphenated tables that has a primary key and foreign key
update_id_hyphenatedTables_with_pk_fk() {
    table="$1"
    fk_table="$2"
    fk_column="$3"
    other_table="$4"
    other_column="$5"

    get_foreignKey_name $table "$fk_column"
    fk_constraint_name=$get_foreignKey_name_returnValue
    pk_constraint_name=${table}_pkey

    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_${table}_${fk_column}
    fi

    echo INFO: Updating table ebxml."$table"
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table drop constraint if exists $pk_constraint_name cascade;
        alter table ebxml.$table drop constraint if exists $fk_constraint_name;
EOF
    update_id_hypenatesTables_basic_operations "$table" "$fk_table" "$fk_column" "$other_table" "$other_column"
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table add constraint $fk_constraint_name FOREIGN KEY ($fk_column)  REFERENCES ebxml.$fk_table (id);
EOF
}

# updating registryobjectlist_registryobject table
update_registryobjectlist_registryobject_table() {

    get_foreignKey_name registryobjectlist_registryobject query_id
    fk_constraint_name=$get_foreignKey_name_returnValue

    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_registryobjectlist_registryobject_query_id
    fi

    echo INFO: Updating  registryobject_id of ebxml.registryobjectlist_registryobject
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            alter table ebxml.registryobjectlist_registryobject add column registryobject_bid bigint;
            update ebxml.registryobjectlist_registryobject set registryobject_bid=registryobject_id;
            alter table ebxml.registryobjectlist_registryobject drop column if exists registryobject_id;
            alter table ebxml.registryobjectlist_registryobject add column registryobject_id character varying(1024);

EOF
    update_foreignKey_value "registryobjectlist_registryobject" "registryobject_id" "registryobject_bid" "registryobject"
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.registryobjectlist_registryobject drop column if exists registryobject_bid;
EOF

}

#basic common operations needed for tables with composite primary keys
update_compositePkeyTable_basic_operations(){
    table="$1"
    fk_table="$2"
    fk_column="$3"
    other_column="$4"
    fk_column_bid="$5"
    other_column_bid="$6"
    pk_constraint_name=${table}_pkey

    get_foreignKey_name $table $fk_column
    fk_constraint_name=$get_foreignKey_name_returnValue

    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_${table}_${fk_column}
    fi


    echo INFO: Updating table ebxml.$table

    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table drop constraint if exists $pk_constraint_name cascade;
        alter table ebxml.$table drop constraint if exists $fk_constraint_name;
        alter table ebxml.$table add column $first_column_bid bigint;
        alter table ebxml.$table add column $other_column_bid bigint;
        update ebxml.$table set $first_column_bid=$first_column;
        update ebxml.$table set $other_column_bid=$other_column;
        alter table ebxml.$table drop column if exists $first_column;
        alter table ebxml.$table drop column if exists $other_column;
        alter table ebxml.$table add column $first_column character varying(1024);
        alter table ebxml.$table add column $other_column character varying(1024);
EOF

}

# rebuild the primary key for composite tables
update_compositePkeyTable_rebuildPK(){
    table="$1"
    first_column="$2"
    other_column="$3"
    first_column_bid="$4"
    other_column_bid="$5"
    fk_table="$6"

    get_foreignKey_name $table $first_column
    fk_constraint_name=$get_foreignKey_name_returnValue

    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_${table}_${first_column}
    fi

    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.$table drop column if exists $first_column_bid;
        alter table ebxml.$table drop column if exists $other_column_bid;
        alter table ebxml.$table add constraint ${table}_pkey primary key ($first_column , $other_column);
        alter table ebxml.$table add constraint $fk_constraint_name FOREIGN KEY ($first_column)  REFERENCES ebxml.$fk_table (id);
EOF


}

#update the taxonomyelementtype_classificationnode table
update_id_taxonomyelementtype_classificationnode(){
    table="taxonomyelementtype_classificationnode"
    first_column="classificationnode_id"
    first_column_bid="classificationnode_bid"
    other_column="taxonomyelementtype_id"
    other_column_bid="taxonomyelementtype_bid"
    pk_constraint_name=${table}_pkey
    fk_column=$first_column
    fk_table="classificationnode"

    update_compositePkeyTable_basic_operations "$table" "$fk_table" "$fk_column" "$other_column"  "$first_column_bid" "$other_column_bid"
    update_foreignKey_value $table $first_column $first_column_bid $fk_table
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        update  ebxml.$table  set $other_column = ebxml.classificationnode.id from  ebxml.classificationnode  where classificationnode.bid = ${table}.taxonomyelementtype_bid;
        update  ebxml.$table  set $other_column = ebxml.classificationscheme.id  from  ebxml.classificationscheme  where classificationscheme.bid = ${table}.taxonomyelementtype_bid;
EOF
    remove_orphaned_records $table $first_column $other_column
    update_compositePkeyTable_rebuildPK $table $first_column $other_column $first_column_bid $other_column_bid $fk_table

}

#update the registryobject_externallink table
update_id_registryobject_externallink(){
    table="registryobject_externallink"
    first_column="externallink_id"
    first_column_bid="externallink_bid"
    other_column="registryobject_id"
    other_column_bid="registryobject_bid"
    pk_constraint_name=${table}_pkey
    fk_column=$first_column
    fk_table="externallink"
    other_table="registryobject"

    update_compositePkeyTable_basic_operations "$table" "$fk_table" "$fk_column" "$other_column" "$first_column_bid" "$other_column_bid"
    update_foreignKey_value $table $first_column $first_column_bid $fk_table
    update_foreignKey_value $table $other_column $other_column_bid $other_table
    remove_orphaned_records $table $first_column $other_column
    update_compositePkeyTable_rebuildPK $table $first_column $other_column $first_column_bid $other_column_bid $fk_table

}

#check if slot parent id column is bigInt
has_bigint_slot_parentid_column() {
    table_name="slot"
    column="parent_id"
    datatype="bigint"
    return_value=$(checkColumnforType $table_name "$datatype" $column)
    return $return_value
}

# updating the parent_id  column of slot table to be varchar
update_slot_table_parentId_column(){
    has_bigint_slot_parentid_column
    if [[ $? == 0 ]];
    then
        echo INFO: Updating slot table parent_id column to varchar
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.slot add column parent_bid_tempColumn bigint;
        update ebxml.slot set parent_bid_tempColumn=parent_id;
        alter table ebxml.slot drop column parent_id;
        alter table ebxml.slot add column parent_id character varying(1024);
EOF
    else
        echo INFO: Parent_id column of the ebxml.slot is already a varchar. Skipping...
    fi

}

# updates the slot parent id from the specified id of the specified parent table
update_slot_table_parentId_values_from_specified_table(){
    table="$1"
    has_varChar_id_column $table
    if [[ $? == 0 ]]; then
        echo INFO: Getting parent_id value from  ebxml.$table
        update_foreignKey_value "slot" "parent_id" "parent_bid_tempColumn" $table

    else
        echo INFO: id column of the ebxml.$table is not yet updated to bigInt or table ebxml.$table does not exist anymore.Skipping...
    fi
}

# Updating the querydefinition table queryexpression_id column to be varchar
update_querydefinition_table_queryexpressionId_column(){
echo INFO: Updating column queryexpression_id  of  ebxml.querydefinition
      /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            alter table ebxml.querydefinition add column queryexpression_bid bigint;
            update ebxml.querydefinition set queryexpression_bid=queryexpression_id;
            alter table ebxml.querydefinition drop column if exists queryexpression_id;
            alter table ebxml.querydefinition add column queryexpression_id character varying(1024);

EOF
            update_foreignKey_value "querydefinition" "queryexpression_id" "queryexpression_bid" "queryexpression"

    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            alter table ebxml.querydefinition drop column if exists queryexpression_bid;
EOF

}

#updating selector_id of subscription table
update_subscription_table_selector_id_column() {

    get_foreignKey_name subscription selector_id
    fk_constraint_name=$get_foreignKey_name_returnValue

    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_subscription_selector_id
    fi

    echo INFO: Updating selector_id of  ebxml.subscription
          /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
             alter table ebxml.subscription add column selector_bid bigint;
            update ebxml.subscription set selector_bid=selector_id;
            alter table ebxml.subscription drop constraint if exists $fk_constraint_name;
            alter table ebxml.subscription drop column if exists selector_id;
            alter table ebxml.subscription add column selector_id character varying(1024);
            alter table ebxml.subscription add constraint $fk_constraint_name FOREIGN KEY (selector_id)  REFERENCES ebxml.query (id);

EOF
            update_foreignKey_value "subscription" "selector_id" "selector_bid" "query"

    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            alter table ebxml.subscription drop column if exists selector_bid;
EOF
}

#updating query_id of dynamicobjectref table
update_dynamicobjectref_table() {

    get_foreignKey_name dynamicobjectref query_id
    fk_constraint_name=$get_foreignKey_name_returnValue

    if [[ "${fk_constraint_name}" == "" ]]; then
        fk_constraint_name=fk_dynamicobjectref_query_id
    fi

    echo INFO: Updating query_id of  ebxml.dynamicobjectref
      /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
                 alter table ebxml.dynamicobjectref add column query_bid bigint;
            update ebxml.dynamicobjectref set query_bid=query_id;
            alter table ebxml.dynamicobjectref drop constraint if exists $fk_constraint_name;
            alter table ebxml.dynamicobjectref drop column if exists query_id;
            alter table ebxml.dynamicobjectref add column query_id character varying(1024);
            alter table ebxml.dynamicobjectref add constraint $fk_constraint_name FOREIGN KEY (query_id)  REFERENCES ebxml.query (id);

EOF
    update_foreignKey_value "dynamicobjectref" "query_id" "query_bid" "query"

    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            alter table ebxml.dynamicobjectref drop column if exists query_bid;
EOF
}

#drop the temporary column parent_bid_tempColumn
dropSlotParentBid(){
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table ebxml.slot drop column parent_bid_tempColumn;
EOF
}

#recreating indexes.
recreating_indexes() {
    echo INFO: Recreating indexes
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 <<EOF
            \set ON_ERROR_STOP on
            CREATE INDEX IF NOT EXISTS organization_id_address_idx
                ON ebxml.organization_postaladdress
                USING btree
                (organization_id);
            CREATE INDEX IF NOT EXISTS organization_id_telephone_idx
                ON ebxml.organization_telephonenumber
                USING btree
                (organization_id);
            CREATE INDEX IF NOT EXISTS person_id_telephone_idx
                ON ebxml.person_telephonenumber
                USING btree
                (person_id);
            CREATE INDEX IF NOT EXISTS querydefinition_id_idx
                ON ebxml.querydefinition_parameter
                USING btree
                (querydefinition_id);
            CREATE INDEX IF NOT EXISTS registryobject_id_externallink_idx
                ON ebxml.registryobject_externallink
                USING btree
                (registryobject_id);
            CREATE INDEX IF NOT EXISTS taxonomyelement_id_idx
                ON ebxml.taxonomyelementtype_classificationnode
                USING btree
                (taxonomyelementtype_id);
EOF
}

# Creating the sequence
create_sequence() {
    echo INFO: creating remaining sequences
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 <<EOF
            \set ON_ERROR_STOP on
            create sequence if not exists ebxml.extensibleObject_seq;
EOF
}

# run all update process
run_all_updates(){
    create_sequence
    # update Id for Tables related to registryObject and extensibleObject tables
    # that only have primary keys ( no foreign Keys) to update
    update_id_of_registryObject_child_modify_pk_only registryobject
    update_id_of_registryObject_child_modify_pk_only association
    update_id_of_registryObject_child_modify_pk_only classification
    update_id_of_registryObject_child_modify_pk_only classificationnode
    update_id_of_registryObject_child_modify_pk_only classificationscheme
    update_id_of_registryObject_child_modify_pk_only comment
    update_id_of_registryObject_child_modify_pk_only externalidentifier
    update_id_of_registryObject_child_modify_pk_only externallink
    update_id_of_registryObject_child_modify_pk_only extrinsicobject
    update_id_of_registryObject_child_modify_pk_only federation
    update_id_of_registryObject_child_modify_pk_only organization
    update_id_of_registryObject_child_modify_pk_only querydefinition
    update_id_of_registryObject_child_modify_pk_only registrypackage
    update_id_of_registryObject_child_modify_pk_only registry
    update_id_of_registryObject_child_modify_pk_only role
    update_id_of_registryObject_child_modify_pk_only serviceendpoint
    update_id_of_registryObject_child_modify_pk_only service
    update_id_of_registryObject_child_modify_pk_only subscription
    update_id_of_registryObject_child_modify_pk_only deliveryinfo
    update_id_of_registryObject_child_modify_pk_only emailaddress
    update_id_of_registryObject_child_modify_pk_only parameter
    update_id_of_registryObject_child_modify_pk_only personname
    update_id_of_registryObject_child_modify_pk_only postaladdress
    update_id_of_registryObject_child_modify_pk_only query
    update_id_of_registryObject_child_modify_pk_only queryexpression
    update_id_of_registryObject_child_modify_pk_only telephonenumber

    # update tables with foreign keys
    update_id_of_registryObject_child_has_pk_and_fk person  personname  personname_id
    update_id_hyphenatedTables_with_uk_compositePk_fk registryobject_externallink externallink externallink_id registryobject registryobject_id externallink_id
    update_id_hyphenatedTables_with_uk_compositePk_fk registryobject_externalidentifier externalidentifier externalidentifier_id registryobject registryobject_id externalidentifier_id
    update_id_hyphenatedTables_with_uk_2fk organization_emailaddress organization organization_id emailaddress emailaddress_id emailaddress_id
    update_id_hyphenatedTables_with_uk_2fk organization_organization organization org_id organization org_id2 org_id2
    update_id_hyphenatedTables_with_uk_2fk organization_postaladdress organization organization_id postaladdress postaladdress_id postaladdress_id
    update_id_hyphenatedTables_with_uk_2fk organization_telephonenumber organization organization_id telephonenumber telephonenumber_id telephonenumber_id
    update_id_hyphenatedTables_with_uk_2fk person_emailaddress person person_id emailaddress emailaddress_id emailaddress_id
    update_id_hyphenatedTables_with_uk_2fk person_postaladdress person person_id postaladdress postaladdress_id postaladdress_id
    update_id_hyphenatedTables_with_uk_2fk person_telephonenumber person person_id telephonenumber telephonenumber_id telephonenumber_id
    update_id_hyphenatedTables_with_uk_2fk querydefinition_parameter querydefinition querydefinition_id parameter parameter_id parameter_id
    update_id_hyphenatedTables_with_uk_2fk service_serviceendpoint service service_id serviceendpoint serviceendpoint_id serviceendpoint_id
    update_id_hyphenatedTables_with_uk_2fk subscription_deliveryinfo subscription subscription_id deliveryinfo deliveryinfo_id deliveryinfo_id
    update_id_hyphenatedTables_with_pk_fk registryobject_classification classification classification_id registryobject registryobject_id

    # update the slot parent id
    update_slot_table_parentId_column
    update_slot_table_parentId_values_from_specified_table association
    update_slot_table_parentId_values_from_specified_table auditableevent
    update_slot_table_parentId_values_from_specified_table cataloging
    update_slot_table_parentId_values_from_specified_table classification
    update_slot_table_parentId_values_from_specified_table classificationnode
    update_slot_table_parentId_values_from_specified_table classificationscheme
    update_slot_table_parentId_values_from_specified_table comment
    update_slot_table_parentId_values_from_specified_table deliveryinfo
    update_slot_table_parentId_values_from_specified_table emailaddress
    update_slot_table_parentId_values_from_specified_table externalidentifier
    update_slot_table_parentId_values_from_specified_table externallink
    update_slot_table_parentId_values_from_specified_table extrinsicobject
    update_slot_table_parentId_values_from_specified_table federation
    update_slot_table_parentId_values_from_specified_table notification
    update_slot_table_parentId_values_from_specified_table organization
    update_slot_table_parentId_values_from_specified_table parameter
    update_slot_table_parentId_values_from_specified_table person
    update_slot_table_parentId_values_from_specified_table personname
    update_slot_table_parentId_values_from_specified_table postaladdress
    update_slot_table_parentId_values_from_specified_table query
    update_slot_table_parentId_values_from_specified_table querydefinition
    update_slot_table_parentId_values_from_specified_table queryexpression
    update_slot_table_parentId_values_from_specified_table registry
    update_slot_table_parentId_values_from_specified_table registryobject
    update_slot_table_parentId_values_from_specified_table registrypackage
    update_slot_table_parentId_values_from_specified_table role
    update_slot_table_parentId_values_from_specified_table service
    update_slot_table_parentId_values_from_specified_table serviceendpoint
    update_slot_table_parentId_values_from_specified_table subscription
    update_slot_table_parentId_values_from_specified_table telephonenumber

    # recreating indexes
    recreating_indexes

    # update specific tables with unique patterns
    update_dynamicobjectref_table
    update_registryobjectlist_registryobject_table
    update_id_taxonomyelementtype_classificationnode
    update_id_registryobject_externallink
    update_querydefinition_table_queryexpressionId_column
    update_subscription_table_selector_id_column

    #drop slot parent_bid_tempColumn column
    dropSlotParentBid

    echo INFO:  Script has completed running !

}


# run the script
run_script(){
    dumpfile=/awips2/edex/data/5847_dump.sql

    # If a dumpfile already exists, move it to backup
    if test -f "$dumpfile"; then
       mv $dumpfile ${dumpfile}.bak
    fi
        
    # Create a dump of the ebxml schema as a backup in case these changes need undone.
    echo INFO "Dumping existing ebxml schema content to location $dumpfile..."
    pg_dump --port 5432 -c -U awipsadmin --schema ebxml -d metadata > $dumpfile
    echo INFO "Done dumping ebxml schema content to location $dumpfile !"
          
    # Perform all the id updates
    run_all_updates
}


run_script
