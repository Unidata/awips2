#!/bin/bash

# This script creates sequences for the tables in the events schema

STATS_MAX_VAL=$(psql -U awips -d metadata -t -c "select max(id)+1 from events.stats;")
#Since events.notification will only exist at sites running datadelivery, check for table first to avoid ERRORs
if [ `psql -U awips -d metadata -tAc "select exists (select 1 from information_schema.tables where table_schema='events' and table_name='notification');"` = 't' ]; then
   NOTIFICATION_MAX_VAL=$(psql -U awips -d metadata -t -c "select max(id)+1 from events.notification;")
else
   NOTIFICATION_MAX_VAL=0
fi
AGGREGATE_MAX_VAL=$(psql -U awips -d metadata -t -c "select max(id)+1 from events.aggregate;")

if [ -z $STATS_MAX_VAL ]
then
	STATS_MAX_VAL=1
fi

if [ -z $NOTIFICATION_MAX_VAL ]
then
	NOTIFICATION_MAX_VAL=1
fi

if [ -z $AGGREGATE_MAX_VAL ]
then
	AGGREGATE_MAX_VAL=1
fi

psql -U awips -d metadata -c \
"CREATE SEQUENCE stats_seq START WITH $STATS_MAX_VAL; \
 CREATE SEQUENCE aggregate_seq START WITH $AGGREGATE_MAX_VAL;"

if [ $NOTIFICATION_MAX_VAL != 0 ]; then
   psql -U awips -d metadata -c "CREATE SEQUENCE notification_seq START WITH $NOTIFICATION_MAX_VAL;"
fi
