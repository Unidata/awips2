#!/bin/bash
#
# Changes the tcm.bullmessage column in the metadata database to type "text",
# so that it is actually unlimited in length.
#
# Run on dx1. To do a dry run, run with the '-d' option.
#
# Author: tgurney

echo "INFO: $0 started."
dtype=$(psql -Aqt --user=awips --db=metadata -c \
       "select data_type || '(' || coalesce(character_maximum_length,0) || ')'
        from information_schema.columns
        where table_name = 'tcm'
        and column_name='bullmessage';"
       )

finish="commit"
if [[ "$1" == "-d" ]]; then
    echo "INFO: -d was specified, will not actually change anything."
    finish="rollback"
fi

if [[ "$dtype" != "text(0)" ]]; then
    echo "INFO: tcm.bullmessage is of type '$dtype'. Changing to 'text'"
    psql --user=awipsadmin --db=metadata << EOF
    begin;
    alter table if exists tcm alter column bullmessage type text;
    $finish;
EOF
else
    echo "INFO: tcm.bullmessage is of type 'text'. No change neccessary"
fi
echo "INFO: $0 finished."
