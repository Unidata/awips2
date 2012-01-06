#!/bin/bash
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# $1 == install directory
# $2 == port number
# $3 == database location directory
export LD_LIBRARY_PATH="${1}/lib"

/bin/chmod a+x ${1}/bin/*

PGDATA=${3}
PGHOME=${1}/share/sql
PGDATABASE=metadata
METADATA=${PGDATA}/metadata
IHFS=${PGDATA}/pgdata_ihfs
MAPS=${PGDATA}/maps
DAMCAT=${PGDATA}/damcat
HMDB=${PGDATA}/hmdb

USER=`whoami`

# Check to see if an instance of PostgreSQL is running on the port specified

PGCHECK=`/bin/netstat -l | /bin/grep -c "PGSQL.$2"`
CNT=0
while [ "$PGCHECK" -eq "1" ]
do
  if [ "$CNT" -eq "0" ]
  then
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "!!                 WARNING!"
    echo "!!"
    echo "!! Another PostgreSQL process is running on"
    echo "!! port $2.  Please shut down the other"
    echo "!! instance of PostgreSQL to continue install."
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    CNT=`expr $CNT + 1`
  fi
  PGCHECK=`/bin/netstat -l | /bin/grep -c "PGSQL.$2"`
done

#start posgresql with the initdb command
echo **Initialize the database and start the service
mkdir -p $PGDATA

###cp ./conf/postgresql /etc/sysconfig/pgsql
###chmod 755 /etc/sysconfig/pgsql/postgresql
#copy non-default postgresql parameters to /etc/sysconfig/pgsql

${1}/bin/initdb --auth=trust --locale=en_US.UTF-8 --pgdata=${PGDATA} --lc-collate=en_US.UTF-8 --lc-ctype=en_US.UTF-8

###rm -f ~postgres/.bash_profile
###cp ./conf/bash_profile ~postgres/.bash_profile
###chown postgres ~postgres/.bash_profile
###chgrp postgres ~postgres/.bash_profile
###/etc/init.d/postgresql start

#copy postgresql configuration for non-IPv6 kernels
#copy setup.sql to postgres dir
echo **Copy configuration files and setup sql
rm -f $PGDATA/postgresql.conf
cp $PGHOME/postgresql.conf $PGDATA

#create a metadata tablespace directory
echo **Create a directory for the metadata tablespace
mkdir $METADATA

#create a maps tablespace directory
echo **Create a directory for the maps tablespace
mkdir $MAPS

#create a ihfs tablespace directory
echo **Create a directory for the ihfs tablespace
mkdir $IHFS

#create a damcat tablespace directory
echo **Create a directory for the damcat tablespace
mkdir $DAMCAT

#create an hmdb tablespace directory
echo **Create a directory for the hmdb tablespace
mkdir $HMDB

#restart postgresql service 
echo **Starting PostgreSQL configuration

${1}/bin/pg_ctl start -D ${PGDATA} -o "-p ${2}" -w

#create the tablespace and awips objects
echo **Run the initial SQL script
${1}/bin/psql postgres -U $USER -q -p $2 -f ${PGHOME}/initial_setup_developer.sql > $PGHOME/sql_install.log 2>&1

#create the postgis objects
echo **Create the GIS database
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${1}/share/lwpostgis.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${1}/share/spatial_ref_sys.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/permissions.sql >> $PGHOME/sql_install.log 2>&1
echo -----------------------------------------------------
echo \| Creating station table...
echo -----------------------------------------------------
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/create_subscription_tables.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/fxatext.sql >> $PGHOME/sql_install.log 2>&1
echo -----------------------------------------------------
echo \| Creating maps database...
echo -----------------------------------------------------
#${1}/bin/psql -d postgres -U $USER -q -p $2 -f ${PGHOME}/createMapsDb.sql >> $PGHOME/sql_install.log 2>&1
#${1}/bin/psql -d maps -U $USER -q -p $2 -f ${1}/share/lwpostgis.sql >> $PGHOME/sql_install.log 2>&1
#${1}/bin/psql -d maps -U $USER -q -p $2 -f ${1}/share/spatial_ref_sys.sql >> $PGHOME/sql_install.log 2>&1
#${1}/bin/pg_restore -d maps -U $USER -p $2 -n mapdata ${PGHOME}/maps.db >> $PGHOME/sql_install.log 2>&1
#${1}/bin/pg_restore -d maps -U $USER -n public -t geometry_columns -a ${PGHOME}/maps.db >> $PGHOME/sql_install.log 2>&1
${PGHOME}/initializeMapsDb.sh $1 $USER $2 >> $PGHOME/sql_install.log 2>&1
echo -----------------------------------------------------
echo \| Creating shef tables...
echo -----------------------------------------------------
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/hd_ob83oax.sql >> $PGHOME/sql_install.log 2>&1
echo -----------------------------------------------------
echo \| Creating damcat tables...
echo -----------------------------------------------------
${1}/bin/psql -d postgres -U $USER -q -p $2 -f ${PGHOME}/createDamcat.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d dc_ob7oax -U $USER -q -p $2 -f ${PGHOME}/dcob7oax.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d dc_ob7oax -U $USER -q -p $2 -f ${PGHOME}/populateDamcatDatabase.sql >> $PGHOME/sql_install.log 2>&1
echo -----------------------------------------------------
echo \| Creating HMDB database...
echo -----------------------------------------------------
${PGHOME}/createHMDB.sh ${1} ${2} ${USER} ${PGHOME} ${PGHOME}/sql_install.log
echo -----------------------------------------------------
echo \| Creating VTEC tables...
echo -----------------------------------------------------
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/vtec_initial_setup.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/create_p_vtec_tables.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/populate_p_vtec_tables.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/populate_vtec_events_table.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/populate_vtec_afos_product_table.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/populate_event_product_index.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/populate_vtec_event_tracking_table.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/create_h_vtec_tables.sql >> $PGHOME/sql_install.log 2>&1
${1}/bin/psql -d metadata -U $USER -q -p $2 -f ${PGHOME}/populate_h_vtec_tables.sql >> $PGHOME/sql_install.log 2>&1

${1}/bin/pg_ctl stop -D ${PGDATA} -o "-p ${2}" -w


#set the connections and restrict logons
echo **Copy additional configuration files 
rm -f $PGDATA/pg_hba.conf
cp ${PGHOME}/pg_hba.conf $PGDATA/pg_hba.conf

echo Database creation is complete.
