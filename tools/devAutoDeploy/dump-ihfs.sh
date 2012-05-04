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
TEMP_FILE=/tmp/temp.sql

for table in adjustfactor agricultural alertalarmval arealfcst arealobs commentvalue\
 contingencyvalue dailypp dhradapt dhrradar discharge dpaadapt dparadar dspadapt\
 dspradar evaporation fcstdischarge fcstheight fcstother fcstprecip fcstptdeterm\
 fcstptesp fcstptwatsup fcsttemperature fishcount fpprevprod fpprevprodpractice\
 gatedam ground height hourlypc hourlypp ice lake lightning locimage lwstmt moisture\
 monthlyvalues officenotes ofsdatatrans ofsstntrans pairedvalue perflog power pressure\
 procvalue productlink pseudogageval radiation ratingshift rawpc rawpother rawpp rejecteddata\
 rescap rwbiasdyn rwradarresult rwresult s3postanalprefs sacsmaparams sacsmastate snow\
 temperature unkstn unkstnvalue vtecpractice waterquality weather wind yunique; do
   echo exporting DDL for $table
   if [ -z $HOST ]; then
      pg_dump -a -U $USER -D $DATABASE -p $PORT -t $table > $TEMP_FILE
   else
      pg_dump -a -U $USER -D $DATABASE -h $HOST -p $PORT -t $table > $TEMP_FILE
   fi
   COUNT=`grep -c INSERT $TEMP_FILE`
   if [ $COUNT -ne 0 ]; then
      cat $TEMP_FILE >> $OUT_FILE
   fi
done
