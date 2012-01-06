#!/bin/sh

# dumps selected tables in the IHFS data base.
# created for ticket 1575 -- mwf
#
# usage:
#    dump-ihfs.sh awips hd_ob83oax ihfs-data.sql 5433 awips-devdb
# arguments
# $1 = user
# $2 = database name
# $3 = output file
# $4 = port
# $5 = host

USER=$1
DATABASE=$2
OUT_FILE=$3
PORT=$4
HOST=$5
if [ -z $PORT ]; then
   PORT=5432
fi

if [ -f $OUT_FILE ]; then
   rm -f $OUT_FILE
fi
TABLES_TO_DUMP='adjustfactor agricultural alertalarmval arealfcst arealobs commentvalue '\
'contingencyvalue dailypp dhradapt dhrradar discharge dpaadapt dparadar dspadapt '\
'dspradar evaporation fcstdischarge fcstheight fcstother fcstprecip fcstptdeterm '\
'fcstptesp fcstptwatsup fcsttemperature fishcount fpprevprod fpprevprodpractice '\
'gatedam ground height hourlypc hourlypp ice lake lightning locimage lwstmt moisture '\
'monthlyvalues officenotes ofsdatatrans ofsstntrans pairedvalue perflog power pressure '\
'procvalue productlink pseudogageval radiation ratingshift rawpc rawpother rawpp rejecteddata '\
'rescap rwbiasdyn rwradarresult rwresult s3postanalprefs sacsmaparams sacsmastate snow '\
'temperature unkstn unkstnvalue vtecpractice waterquality weather wind yunique'
 
TEMP_FILE=/tmp/temp.sql
# get the location of the postgres bin directory -- assumes postgres is running...
POSTMASTER_STR=`ps ax -o user,args | grep postmaster | grep -v grep`
PG_DAEMON=`echo ${POSTMASTER_STR} | cut -d ' ' -f 2`
PG_BIN_DIR=`dirname ${PG_DAEMON}`
echo "using PostgreSQL in ${PG_BIN_DIR}" 
PG_BASE_DIR=`dirname ${PG_BIN_DIR}`
DB_BASE_DIR=`dirname ${PG_BASE_DIR}`
echo "setting LD_LIBRARY_PATH = ${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"
export LD_LIBRARY_PATH="${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"

for table in ${TABLES_TO_DUMP}; do
   echo exporting DDL for $table
   if [ -z $HOST ]; then
      ${PG_BIN_DIR}/pg_dump -a -U $USER -D $DATABASE -p $PORT -t $table > $TEMP_FILE
   else
      ${PG_BIN_DIR}/pg_dump -a -U $USER -D $DATABASE -h $HOST -p $PORT -t $table > $TEMP_FILE
   fi
   COUNT=`grep -c INSERT $TEMP_FILE`
   if [ $COUNT -eq 0 ]; then
      echo "   no data exported from '${table}'"
   else
      echo "   exported ${COUNT} rows from '${table}'"
      cat $TEMP_FILE >> $OUT_FILE
   fi
done
