#!/bin/sh
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
#
# This is an off-line utility used to populate a new maps database from 
# /awips2/edex/data/utility/edex_static/base/shapefiles
# and create the database snapshot used for clean installs.
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# 02/11/2016   5348       randerso    Do a full vacuum after creating the maps database
# 01/23/2017   6097       randerso    Restored incorrectly deleted file.
#                                     Updated for changes to importShapeFile.sh
# 04/11/2018   7140       tgurney     Use a2dbauth
# 04/05/2021   21179      smoorthy    Remove reference to legacy.sql. Additional code cleanups.
# 07/01/2021   8544       tgurney     Remove PGBINDIR and PSQLBINDIR, not
#                                     needed anymore
# 03/28/2025   2037812    smoorthy    Define pghost appropriately for db calls.


# Set PGHOST appropriately:
# - If we're already on "dv1", set it to dv1.
# - If we're on any other host with postgres running, set it 
#   to "localhost".
# - If we're on any other host without postgres running, set it
#   to "dv1".

export PGHOST='localhost'

host=$(hostname)

#extract first part of hostname of the form dv1-xxx.xxx
IFS="-"
hostTemp=''
for part in $host
do
    hostTemp=$part
    break
done
unset IFS


# if we're on dv1, just set "PGHOST" to dv1
if [ $hostTemp == 'dv1' ] ; then
    export PGHOST=$hostTemp
fi


#check if postgres is running on this host. Should be many 
#processes with "postgres" in the name.
numPostgres=$(ps -ef | grep postgres | wc -l)

#choosing 3 for safety, since the "grep" is atleast 1
if [ $numPostgres -lt 3 ] ; then
   export PGHOST='dv1'
fi



DATABASEDIR=/awips2/database/sqlScripts/share/sql/maps
PGUSER=awipsadmin
PGPORT=5432

psql="a2dbauth psql"
vacuumdb="a2dbauth vacuumdb"
pg_restore="a2dbauth pg_restore"

${psql} -d postgres -U $PGUSER -q -p $PGPORT -f ${DATABASEDIR}/createMapsDb.sql
${psql} -d maps -U $PGUSER -q -p $PGPORT -c "CREATE EXTENSION postgis;"
${psql} -d maps -U $PGUSER -q -p $PGPORT -c "CREATE EXTENSION postgis_topology;"
if [ -f ${DATABASEDIR}/maps.db ] ; then
    ${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "DROP TABLE IF EXISTS mapdata.map_version"
    ${pg_restore} -d maps -U $PGUSER -p $PGPORT -n mapdata ${DATABASEDIR}/maps.db
    ${pg_restore} -d maps -U $PGUSER -p $PGPORT -n public -t geometry_columns -a ${DATABASEDIR}/maps.db
    ${vacuumdb} -d maps -U ${PGUSER} -p ${PGPORT} -vz
else
    for shp in `find /awips2/edex/data/utility/edex_static/base/shapefiles -name "*.shp"` ; do
        base=`basename \`dirname $shp\``
        echo Importing $base...
        ${DATABASEDIR}/importShapeFile.sh $shp $base
    done

    for file in `find /awips2/edex/data/utility/edex_static/base/infofiles -type f`; do
        base=`basename \`dirname $file\``
        echo Importing `basename $file` into $base...
        ${DATABASEDIR}/importPointsInfo.sh $file mapdata $base $PGUSER $PGPORT /awips2
    done
    ${vacuumdb} -d maps -U ${PGUSER} -p ${PGPORT} -vfz
    ${DATABASEDIR}/createMapsDbSnapshot.sh ${DATABASEDIR}/maps.db
fi
