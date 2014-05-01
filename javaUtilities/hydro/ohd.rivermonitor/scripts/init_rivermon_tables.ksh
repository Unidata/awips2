#!/bin/ksh

#Authors: Chip Gobs / Varalakshmi Rajaram
#Date : Sept 22, 2006
#Initialize rivermonlocation and rivermongroup tables

# This allows you to run this script from outside of ./bin
RUN_FROM_DIR=`dirname $0`

# set up SOME environment variables for WHFS applications
. $RUN_FROM_DIR/../../set_hydro_env

###################### function
fill_rivermon_group_and_location(){
 $POSTGRESQLBINDIR/psql $DB_NAME -e << END
 
 
-- create a table to hold the group information, 
-- we will later load this table into RiverMonGroup
  
   create table group_temp(group_id varchar(8),
     				     group_name varchar(32),
				     ordinal integer,
				     hsa varchar(3));

-- fill up the groups initially with what is in RpfFcstGroup

  insert into group_temp select * from RpfFcstGroup;
 
 
-- add in the standard default group
-- this is being done to prevent unknown side effects in its absence
    insert into group_temp values('DEFAULT', 'DEFAULT GROUP', 1, (select hsa from admin limit 1) );


-- add in a default group per HSA
    create table default_group_temp(group_id varchar(8),
     				     group_name varchar(32),
				     ordinal integer,
				     hsa varchar(3));
				     
    insert into default_group_temp  ( hsa )
            (select distinct(hsa) from location l, riverstat r
   	        where l.lid = r.lid);
		
    update default_group_temp 
           set group_id = 'DEF_'||hsa,
	       group_name = hsa||' DEFAULT GROUP',
	       ordinal = 1;

    insert into group_temp
           select group_id, group_name, ordinal, hsa from default_group_temp;


    select * from group_temp;



-- add all locations to location_temp according to location and riverstat data
    create table location_temp(lid varchar(8),
                                group_id varchar(8),
			        ordinal integer,
			        hsa varchar(3));
						
    insert into location_temp(lid, hsa) 
                     select lid, hsa  from location 
		           where (type <> 'I' or type is null) and
			         lid in(select lid from riverstat);
	
-- update locations according to RpfFcstPoint data				 
    update location_temp set 
           group_id=(select group_id from rpffcstpoint where lid=location_temp.lid);
    update location_temp set 
           ordinal=(select ordinal from rpffcstpoint where lid=location_temp.lid);
     
     
-- for the non-grouped river data points, assign the default group information, based on the HSA of the point
    update location_temp set 
           group_id=(select group_id from default_group_temp g 
	               where hsa = location_temp.hsa) where group_id is null;	       
    update location_temp set 
           ordinal=1 where ordinal is null;

     select * from location_temp;
     

 
--  update the hsa for the groups, based on their points
    update group_temp set
      hsa=(select hsa from location_temp where group_id=group_temp.group_id order by lid limit 1);
 
-- for the case of an empty group, give it the admin.hsa 
    update group_temp set hsa=(select hsa from admin) where hsa is NULL;


    select * from group_temp;


-- copy the temp table information to the real tables
  delete from rivermonlocation;
  delete from rivermongroup;

  insert into RiverMonGroup 
      select * from group_temp;
  insert into RiverMonLocation
      select lid, group_id, ordinal from location_temp;


-- clean up the temp tables
 drop table default_group_temp;
 drop table location_temp;     
 drop table group_temp;
     
END

}


#fn 

fill_rivermon_group_and_location
