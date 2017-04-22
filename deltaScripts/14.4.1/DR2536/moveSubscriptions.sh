#!/bin/bash

# moves subscriptions tables from metadata db to fxatext db

function rowcount {
    psql -U awips -d $1 -c "select count(*) from $2"
}

function getSeqStart {
    CURR_ID=$(psql -U awips metadata -Aqzt0 -c "select max(id) from $1")
    if [[ -z $CURR_ID ]]
    then
        echo 1
    else
        echo $(( $CURR_ID + 1 ))
    fi
}

BACKUPFILE=sub_dump.bak
MODIFIED_BACKUP=sub_modified.bak

echo "Moving subscriptions tables from metadata to fxatext"

OLD_SUB_COUNT=$(rowcount metadata subscription.subscriptions)
OLD_REP_COUNT=$(rowcount metadata subscription.replacements)

SUB_SEQ_START=$(getSeqStart subscription.subscriptions)
REQ_SEQ_START=$(getSeqStart subscription.replacements)

pg_dump -U awips -t subscription.subscriptions -t subscription.replacements metadata > $BACKUPFILE
if [[ $? != 0 ]]
then
    echo "subscription tables backup failed, aborting"
    exit 1
fi

sed 's/\(\(TABLE\s\+\)\|\(Schema:\s\+\)\|=\s\+\)subscription\([^s]\)/\1public\4/' $BACKUPFILE > $MODIFIED_BACKUP
if [[ $? != 0 ]] 
then
    echo "subscription tables backup editing failed, aborting"
    exit 1
fi

psql -U awips fxatext < $MODIFIED_BACKUP
if [[ $? != 0 ]]
then
    echo "Subscription tables restore failed, backup located at $BACKUPFILE"
    exit 1
fi

NEW_SUB_COUNT=$(rowcount fxatext public.subscriptions)
NEW_REP_COUNT=$(rowcount fxatext public.replacements)

if [[ $OLD_SUB_COUNT != $NEW_SUB_COUNT || $OLD_REP_COUNT != $NEW_REP_COUNT ]]
then
    echo "Row counts do not match before and after table move"
    echo "Subscriptions before: \n$OLD_SUB_COUNT"
    echo "Subscriptions after: \n$NEW_SUB_COUNT"
    echo "Replacements before: \n$OLD_REP_COUNT"
    echo "Replacements after: \n$NEW_REP_COUNT"
    echo "skipping old table cleanup, backup exists at $BACKUPFILE"
    exit 1
fi

echo "Creating sequences"
psql -U awips -d fxatext -c "CREATE SEQUENCE subscriptionseq START WITH $SUB_SEQ_START"
psql -U awips -d fxatext -c "CREATE SEQUENCE replacementseq START WITH $REQ_SEQ_START"

echo "Cleaning up old tables"
psql -U awips -d metadata -c 'DROP SCHEMA subscription CASCADE'
psql -U awips -d metadata -c "DELETE from awips.plugin_info WHERE name = 'com.raytheon.edex.autobldsrv'"

rm $MODIFIED_BACKUP
rm $BACKUPFILE
echo "Done moving subscription tables"
