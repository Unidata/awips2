#!/bin/ksh

#Author: Varalakshmi Rajaram
#Date : May 10th 2006
#Initialize rivermonlocation and rivermongroup tables

# This allows you to run this script from outside of ./bin
RUN_FROM_DIR=`dirname $0`

# set up SOME environment variables for WHFS applications
. $RUN_FROM_DIR/../../set_hydro_env
export DB_NAME=$(get_apps_defaults db_name)

###################### function
fn(){
 psql $DB_NAME -e << END
 \copy rpffcstgroup (group_id, group_name, ordinal) to './input.grp' with delimiter '|'
 \copy rivermongroup(group_id, group_name, ordinal) from './input.grp' with delimiter '|'
 insert into rivermongroup values('DEFAULT', 'DEFAULT GROUP', 1);
 update rivermongroup set hsa=(select hsa from admin) where hsa is null;
 \copy rpffcstpoint (lid, group_id, ordinal) to './input.lid1' with delimiter '|'
  create table dummytable(lid varchar(8));
  insert into dummytable select lid from location where lid not in(select lid from rpffcstpoint);
 \copy dummytable (lid) to './input.lid2' with delimiter '|'
 \copy rivermonlocation(lid, group_id, ordinal) from './input.lid1' with delimiter '|'
 \copy rivermonlocation(lid) from './input.lid2' with delimiter '|'
  update rivermonlocation set ordinal=1 where ordinal is null;
  update rivermonlocation set group_id='DEFAULT' where group_id is null;
  drop table dummytable;
END

 rm -f ./input.grp ./input.lid1 ./input.lid2
}

fn 
