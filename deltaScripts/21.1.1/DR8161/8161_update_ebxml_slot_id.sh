#!/bin/bash

# This script should be run on the database server and switches the id column of the ebxml.slot table from varchar to bigint.


echo INFO: Updating id column of ebxml.slots

/awips2/psql/bin/psql -U awipsadmin -d metadata -c "
begin transaction;
create sequence if not exists ebxml.slot_sequence;
alter sequence if exists ebxml.slot_sequence increment 50;
alter table if exists ebxml.slot drop constraint slot_pkey;
alter table if exists ebxml.slot drop column if exists type;
alter table if exists ebxml.slot drop column id;
alter table if exists ebxml.slot add column id bigint not null default nextval('ebxml.slot_sequence');
alter table if exists ebxml.slot add constraint slot_pkey primary key (id);
commit;
"

/awips2/psql/bin/psql -U awipsadmin -d metadata -c "vacuum full analyze ebxml.slot;"

echo INFO: Finished updating id column of ebxml.slots

