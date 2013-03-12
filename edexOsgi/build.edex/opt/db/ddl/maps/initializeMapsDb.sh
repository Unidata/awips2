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
##

if [ $# -lt 1 ] ; then
    echo
    echo usage: `basename $0` installDir [dbUser [dbPort]]
    echo "       installDir- directory path to awips installation"
    echo "       dbUser    - optional database user id"
    echo "       dbPort    - optional database port number"
    echo "example: `basename $0` /awips2 awips 5432"
    exit -1
fi
STATIC_DATA_DIR="database/sqlScripts/share/sql/maps"
SHARED_SQL_DIR="${1}/database/sqlScripts/share"
DATABASEDIR=${1}/${STATIC_DATA_DIR}
POSTGRESDIR=${1}/postgresql
POSTGIS_CONTRIB=${POSTGRESDIR}/share/contrib/postgis-2.0
PGBINDIR=${1}/postgresql/bin
PSQLBINDIR=${1}/psql/bin

if [ -z $2 ] ; then
    PGUSER=awips
else
    PGUSER=${2}
fi

if [ -z $3 ] ; then
    PGPORT=5432
else
    PGPORT=${3}
fi

${PSQLBINDIR}/psql -d postgres -U $PGUSER -q -p $PGPORT -f ${DATABASEDIR}/createMapsDb.sql
${PSQLBINDIR}/psql -d maps -U $PGUSER -q -p $PGPORT -f ${POSTGIS_CONTRIB}/postgis.sql
${PSQLBINDIR}/psql -d maps -U $PGUSER -q -p $PGPORT -f ${POSTGIS_CONTRIB}/spatial_ref_sys.sql
if [ -f ${DATABASEDIR}/maps.db ] ; then
    ${PSQLBINDIR}/psql -d maps -U ${PGUSER} -q -p ${PGPORT} -c "DROP TABLE IF EXISTS mapdata.map_version"
    ${PGBINDIR}/pg_restore -d maps -U $PGUSER -p $PGPORT -n mapdata ${DATABASEDIR}/maps.db
    ${PGBINDIR}/pg_restore -d maps -U $PGUSER -p $PGPORT -n public -t geometry_columns -a ${DATABASEDIR}/maps.db
    ${PGBINDIR}/vacuumdb -d maps -U ${PGUSER} -p ${PGPORT} -vz
else
    for shp in `find ${1}/edex/data/utility/edex_static/base/shapefiles -name "*.shp"` ; do
        base=`basename \`dirname $shp\``
        echo Importing $base...
        SIMPLEV=
        if [[ $base == 'AllRivers' \
           || $base == 'Basins' \
           || $base == 'Canada' \
           || $base == 'County' \
           || $base == 'CWA' \
           || $base == 'FFMP_Basins' \
           || $base == 'FFMP_Streams' \
           || $base == 'FireWxAOR' \
           || $base == 'FireWxZones' \
           || $base == 'HighSea' \
           || $base == 'Highway' \
           || $base == 'Interstate' \
           || $base == 'Lake' \
           || $base == 'MajorRivers' \
           || $base == 'MarineZones' \
           || $base == 'Mexico' \
           || $base == 'Railroad' \
           || $base == 'RFC' \
           || $base == 'States' \
           || $base == 'Zone' \
           ]]
        then
            SIMPLEV='0.064,0.016,0.004,0.001'
        fi
        
        ${DATABASEDIR}/importShapeFile.sh $shp mapdata $base "$SIMPLEV" $PGUSER $PGPORT $1
    done

    for file in `find ${1}/edex/data/utility/edex_static/base/infofiles -type f`; do
        base=`basename \`dirname $file\``
        echo Importing `basename $file` into $base...
        ${DATABASEDIR}/importPointsInfo.sh $file mapdata $base $PGUSER $PGPORT $1
    done
    ${PGBINDIR}/vacuumdb -d maps -U ${PGUSER} -p ${PGPORT} -vz
    ${DATABASEDIR}/createMapsDbSnapshot.sh ${1} $PGUSER $PGPORT ${DATABASEDIR}/maps.db
fi
