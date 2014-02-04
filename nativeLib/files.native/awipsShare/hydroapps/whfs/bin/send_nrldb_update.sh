#!/bin/sh
###############################################################################
# This script is run at the field office to send ad-hoc updates to the NRLDB
# server, then on to the AHPS CMS.  It can be run at any time.  It is designed 
# to send small, time-sensitive updates to the CMS.  It takes two argument 
# lists:-table table names (comma-separated) and -lid lid names
# (comma-separated).  It parses the arguments, selects the updated data from 
# the database and builds an SQL formatted text file for use on the nrldb and
# CMS databases.  The SQL file contains a delete staement that deletes the
# pre-existing data for the lid/table combinations, before running the inserts
#
# Usage: send_nrldb_update.sh -table <table1>,<table2>,... -lid <lid1>,<lid2>,...
# Example: send_nrldb_update.sh -table rating,floodstmt -lid BRKM2,CBEM2
#
if [ $# -ne 4 ]
then
	echo "Incorrect number of arguments entered: $#"
	echo "Correct Arguments are:"
	echo "send_nrldb_update.sh -table table1,table2 -lid lid1,lid2"
	echo "Any number of tables and lids may be specified, but they need to be in a comma separated list with no spaces between commas and table/lid names"
	exit 0
fi 
# set up SOME environment variables for NRLDB applications

# get the nrldb host and wfo from the nrldb.conf file/database
nrldb_host=`grep nrldb_host $NRLDB_CONFIG/nrldb.conf | cut -d= -f2 | sed 's/"//g' | sed 's/ //g'`
echo "DB NAME: $db_name"
wfo=`psql -h $PGHOST -d $db_name -c "select hsa from admin;" | tail -3 | head -1 | sed -e 's/ //g'`
echo `date`
echo "WFO $wfo"

# create the final SQL file that will be sent to the NRLDB host
timestamp=`date +%Y%m%d%H%N`
sql_file="${wfo}_update_${timestamp}.sql"
if [ -f $sql_file ]
then
	rm $sql_file
fi

# build the list of tables/lids to send 
lid_list="XXXXX"
table_list="XXXXX"
while [ $# -gt 0 ]
do
        case "$1" in
                -lid) lid_list="$2,";shift;;
                -table) table_list="$2,";shift;;
                *) break;;
        esac
        shift
done

# set the last update information for update_nrldb.pl to use
echo `date` > ${NRLDB_LOG}/last_nrldb_update.txt
up_lid_list=`echo $lid_list | sed 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`
echo "lid list: $up_lid_list" >> ${NRLDB_LOG}/last_nrldb_update.txt 
echo "table_list: $table_list" >> ${NRLDB_LOG}/last_nrldb_update.txt

#loop through the tables/lids
if [ $table_list != "XXXXX" ]
then
        pos=1
        table="XXXXX"
        ltable=`echo $table | wc -m`
        while [ $ltable -gt 4 ]
        do
                table=`echo $table_list | cut -d"," -f$pos`
                pos=`expr $pos + 1`
                ltable=`echo $table | wc -m`
                if [ $ltable -gt 4 ]
                then
        		lid="XXXXX"
			lpos=1
			llid=`echo $lid | wc -m`
        		while [ $llid -gt 3 ]
        		do
                		lid=`echo $up_lid_list | cut -d"," -f$lpos`
                		lpos=`expr $lpos + 1`
                		llid=`echo $lid | wc -m`
                		if [ $llid -gt 3 ]
                		then
					# fetch the values from the DB and edit them
					export PGUSER=awips
					touch $NRLDB_TMP/update.txt
					chmod ugo+rw $NRLDB_TMP/update.txt
					ls -l $NRLDB_TMP/update.txt
					psql -h $PGHOST -d $db_name -c "copy (select * from $table where lid = '$lid') to '$NRLDB_TMP/update.txt' with delimiter '|';"  
					cp $NRLDB_TMP/update.txt ${NRLDB_DATA}/update.txt 
					sed -f ${NRLDB_CONFIG}/sed_script.txt ${NRLDB_TMP}/update.txt > ${NRLDB_DATA}/update11.txt
					sed -e "s/|/'|'/g" ${NRLDB_DATA}/update11.txt > ${NRLDB_DATA}/update1.txt
					sed -e "s/^/insert into $table values('/g" ${NRLDB_DATA}/update1.txt > ${NRLDB_DATA}/update2.txt
					sed -e "s/$/');/g" ${NRLDB_DATA}/update2.txt > ${NRLDB_DATA}/update3.txt
					sed -e "s/|/,/g" ${NRLDB_DATA}/update3.txt > ${NRLDB_DATA}/update4.txt
					if [ -f "${NRLDB_DATA}/update.txt" ]
					then
						update_lines=`wc -l "${NRLDB_DATA}/update.txt" | cut -d" " -f1`
					else
						echo "No update file found".
						update_lines=0
					fi
					if [ $update_lines -gt 0 ]
					then
						if [ $table != "location" -a $table != "riverstat" ]
						then
							echo "delete from $table where lid = '$lid';" >> ${NRLDB_DATA}/$sql_file
						fi
						cat ${NRLDB_DATA}/update4.txt >> ${NRLDB_DATA}/$sql_file
					fi
					# location and riverstat require a special forecast since they have dependent tables via foreign keys
					if [ $table = "location" ]
					then
						sql_stmt="update location set lid = '$lid'"
						for col in county coe cpm detail elev hdatum hsa hu lat lon lremark lrevise name network rb rfc sbd sn state waro wfo wsfo type des det post stntype tzone
						do
							psql -h $PGHOST -d  $db_name -c "select $col from location where lid = '$lid' and $col is not null;" > ${NRLDB_DATA}/update.txt
							ct_zero=`grep -c "0 row" ${NRLDB_DATA}/update.txt`
							if [ $ct_zero -eq 0 ]
							then	
								export val=`cat ${NRLDB_DATA}/update.txt | head -3 | tail -1 | cut -c2-80`
								new_val=`echo "$val" | sed -f ${NRLDB_CONFIG}/sed_script.txt`
								sql_stmt="$sql_stmt, $col = '$new_val'"
							fi
						done 
						sql_stmt="$sql_stmt where lid = '$lid';" 
						echo $sql_stmt >> ${NRLDB_DATA}/$sql_file
						
					elif [ $table = "riverstat" ]
					then
						sql_stmt="update riverstat set lid = '$lid'"
						for col in primary_pe bf cb da response_time threshold_runoff fq fs gsno level mile pool por rated lat lon remark rrevise rsource stream tide backwater vdatum action_flow wstg zd ratedat usgs_ratenum uhgdur use_latest_fcst
                                                do
							psql -h $PGHOST -d  $db_name -c "select $col from riverstat where lid = '$lid' and $col is not null;" > ${NRLDB_DATA}/update.txt
							ct_zero=`grep -c "0 row" ${NRLDB_DATA}/update.txt`
							if [ $ct_zero -eq 0 ]
							then	
								export val=`cat ${NRLDB_DATA}/update.txt | head -3 | tail -1 | cut -c2-80`
								new_val=`echo "$val" | sed -f ${NRLDB_CONFIG}/sed_script.txt`
								sql_stmt="$sql_stmt, $col = '$new_val'"
							fi
                                                done
						sql_stmt="$sql_stmt where lid = '$lid';"
                                                echo $sql_stmt >> ${NRLDB_DATA}/$sql_file
					fi
                		fi
			done
                fi

        done

	# send the SQL file to the NRLDB server
	if [ -f ${NRLDB_DATA}/$sql_file ]
	then
		rsync -av ${NRLDB_DATA}/$sql_file ${nrldb_host}\::nrldb_update/
		echo "SQL file: $sql_file  created for lids: $up_lid_list and tables: $table_list"
	else
		echo "No SQL file created.  Database contained no entries for lids: $up_lid_list and tables: $table_list"
	fi
fi

# remove the temp files to keep the directory clean
for temp_file in ${NRLDB_DATA}/update.txt ${NRLDB_DATA}/update11.txt ${NRLDB_DATA}/update1.txt ${NRLDB_DATA}/update2.txt ${NRLDB_DATA}/update3.txt ${NRLDB_DATA}/update4.txt
do
	if [ -f $temp_file ]
	then
		rm $temp_file
	fi	
done
