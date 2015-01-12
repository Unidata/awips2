#!/bin/bash

# This script creates sequences for the tables in the events schema

STATS_MAX_VAL=$(psql -U awips -d metadata -t -c "select max(id)+1 from events.stats;")
NOTIFICATION_MAX_VAL=$(psql -U awips -d metadata -t -c "select max(id)+1 from events.notification;")
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
 CREATE SEQUENCE notification_seq START WITH $NOTIFICATION_MAX_VAL; \
 CREATE SEQUENCE aggregate_seq START WITH $AGGREGATE_MAX_VAL;"

