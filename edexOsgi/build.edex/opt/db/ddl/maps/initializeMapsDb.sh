#!/bin/sh
##
#
# This is an off-line utility used to populate a new maps database from 
# /awips2/edex/data/utility/edex_static/base/shapefiles
# and create the database snapshot used for clean installs.
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# 02/11/2016    #5348     randerso    Do a full vacuum after creating the maps database
# 01/23/2017    #6097     randerso    Restored incorrectly deleted file.
#                                     Updated for changes to importShapeFile.sh
#
##

DATABASEDIR=/awips2/database/sqlScripts/share/sql/maps
POSTGIS_CONTRIB=/awips2/postgresql/share/contrib/postgis-2.2
PGBINDIR=/awips2/postgresql/bin
PSQLBINDIR=/awips2/psql/bin
PGUSER=awipsadmin
PGPORT=5432

${PSQLBINDIR}/psql -d postgres -U $PGUSER -q -p $PGPORT -f ${DATABASEDIR}/createMapsDb.sql
${PSQLBINDIR}/psql -d maps -U $PGUSER -q -p $PGPORT -c "CREATE EXTENSION postgis;"
${PSQLBINDIR}/psql -d maps -U $PGUSER -q -p $PGPORT -c "CREATE EXTENSION postgis_topology;"
${PSQLBINDIR}/psql -d maps -U $PGUSER -q -p $PGPORT -f ${POSTGIS_CONTRIB}/legacy.sql
if [ -f ${DATABASEDIR}/maps.db ] ; then
    ${PSQLBINDIR}/psql -d maps -U ${PGUSER} -q -p ${PGPORT} -c "DROP TABLE IF EXISTS mapdata.map_version"
    ${PGBINDIR}/pg_restore -d maps -U $PGUSER -p $PGPORT -n mapdata ${DATABASEDIR}/maps.db
    ${PGBINDIR}/pg_restore -d maps -U $PGUSER -p $PGPORT -n public -t geometry_columns -a ${DATABASEDIR}/maps.db
    ${PGBINDIR}/vacuumdb -d maps -U ${PGUSER} -p ${PGPORT} -vz
else
    for shp in `find /awips2/edex/data/utility/edex_static/base/shapefiles -name "*.shp"` ; do
        base=`basename \`dirname $shp\``
        echo Importing $base...
        ${DATABASEDIR}/importShapeFile.sh $shp $base
    done

    for file in `find /awips2/edex/data/utility/edex_static/base/infofiles -type f`; do
        base=`basename \`dirname $file\``
        echo Importing `basename $file` into $base...
        ${DATABASEDIR}/importPointsInfo.sh $file mapdata $base $PGUSER $PGPORT $1
    done
    ${PGBINDIR}/vacuumdb -d maps -U ${PGUSER} -p ${PGPORT} -vfz
    ${DATABASEDIR}/createMapsDbSnapshot.sh ${DATABASEDIR}/maps.db
fi
