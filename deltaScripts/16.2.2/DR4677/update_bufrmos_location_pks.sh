#!/bin/bash
# DR #4677 - This script replaces all PKs in bufrmos_location with
#            sequence-generated values, and cascades the update to all
#            referencing tables.

PSQL="/awips2/psql/bin/psql"

# Given table name as argument, return the name of the FK constraint
# referencing bufrmos_location.
get_constraint_name() {
    ${PSQL} -U awips -Aqt -d metadata << EOF
    SELECT tc.constraint_name
    FROM information_schema.table_constraints AS tc 
    JOIN information_schema.key_column_usage AS kcu
    ON tc.constraint_name = kcu.constraint_name
    JOIN information_schema.constraint_column_usage AS ccu
    ON ccu.constraint_name = tc.constraint_name
    WHERE constraint_type = 'FOREIGN KEY'
    AND tc.table_name='$1'
    and ccu.table_name = 'bufrmos_location';
EOF
}

# Check for existence of bufrmos_locationseq
sequence_exists() {
    ${PSQL} -U awips -Aqt -d metadata << EOF
    select 0
    from information_schema.sequences
    where sequence_name = 'bufrmos_locationseq'
EOF
}

get_min_pk() {
    ${PSQL} -U awips -Aqt -d metadata << EOF
    select min(id) id
    from bufrmos_location;
EOF
}

get_max_pk() {
    ${PSQL} -U awips -Aqt -d metadata << EOF
    select max(id) id
    from bufrmos_location;
EOF
}

last_bufrmos_locationseq_value() { 
    ${PSQL} -U awips -Aqt -d metadata -c \
        "select last_value from bufrmos_locationseq;"
}


if [[ "$(sequence_exists)" != "0" ]]; then
    echo "INFO: bufrmos_locationseq does not exist in the database"
    echo "INFO: Attempting to create bufrmos_locationseq"
    ${PSQL} -U awips -d metadata -c \
        "create sequence bufrmos_locationseq increment 1 start 1;"
    if [[ "$?" != "0" || "$(sequence_exists)" != "0" ]]; then
        echo "ERROR: Failed to create bufrmos_locationseq"
        exit 1
    else
        echo "INFO Successfully created bufrmos_locationseq"
    fi
fi

min_pk="$(get_min_pk)"
max_pk="$(get_max_pk)"

if [[ ("$min_pk" -gt 0) && ("$max_pk" -le "$(last_bufrmos_locationseq_value)") ]]; then
    echo "WARN: Looks like PKs were already updated."
    echo "WARN: (smallest PK found: ${min_pk})"
    echo "WARN: (largest PK found: ${max_pk})"
    echo "WARN: Doing nothing."
    exit 0
fi

fk_bufrmosavn=`get_constraint_name bufrmosavn`
fk_bufrmoseta=`get_constraint_name bufrmoseta`
fk_bufrmosgfs=`get_constraint_name bufrmosgfs`
fk_bufrmoshpc=`get_constraint_name bufrmoshpc`
fk_bufrmoslamp=`get_constraint_name bufrmoslamp`
fk_bufrmosmrf=`get_constraint_name bufrmosmrf`
fk_bufrmosngm=`get_constraint_name bufrmosngm`

echo "INFO: Updating all PKs of bufrmos_location"
echo "INFO: This may take a few minutes..."

${PSQL} -U awips -d metadata << EOF
begin transaction;
alter table bufrmosavn
drop constraint ${fk_bufrmosavn},
add constraint ${fk_bufrmosavn}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;

alter table bufrmoseta
drop constraint ${fk_bufrmoseta},
add constraint ${fk_bufrmoseta}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;

alter table bufrmosgfs
drop constraint ${fk_bufrmosgfs},
add constraint ${fk_bufrmosgfs}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;

alter table bufrmoshpc
drop constraint ${fk_bufrmoshpc},
add constraint ${fk_bufrmoshpc}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;

alter table bufrmoslamp
drop constraint ${fk_bufrmoslamp},
add constraint ${fk_bufrmoslamp}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;

alter table bufrmosmrf
drop constraint ${fk_bufrmosmrf},
add constraint ${fk_bufrmosmrf}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;

alter table bufrmosngm
drop constraint ${fk_bufrmosngm},
add constraint ${fk_bufrmosngm}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;

UPDATE bufrmos_location
   SET id=nextval('bufrmos_locationseq');
 
alter table bufrmosavn
drop constraint ${fk_bufrmosavn},
add constraint ${fk_bufrmosavn}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;

alter table bufrmoseta
drop constraint ${fk_bufrmoseta},
add constraint ${fk_bufrmoseta}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;

alter table bufrmosgfs
drop constraint ${fk_bufrmosgfs},
add constraint ${fk_bufrmosgfs}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;

alter table bufrmoshpc
drop constraint ${fk_bufrmoshpc},
add constraint ${fk_bufrmoshpc}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;

alter table bufrmoslamp
drop constraint ${fk_bufrmoslamp},
add constraint ${fk_bufrmoslamp}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;

alter table bufrmosmrf
drop constraint ${fk_bufrmosmrf},
add constraint ${fk_bufrmosmrf}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;

alter table bufrmosngm
drop constraint ${fk_bufrmosngm},
add constraint ${fk_bufrmosngm}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;
commit transaction;
EOF

echo "INFO: Done."
