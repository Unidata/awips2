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
if [ $# -lt 3 ] ; then
    echo
    echo usage: `basename $0` shapefile schema table [simplev [dbUser [dbPort [installDir]]]]
    echo "where: shapefile - pathname of the shape file to be imported"
    echo "       schema    - database schema where the shape file is to be imported"
    echo "       table     - database table where the shape file is to be imported"
    echo "       simplev   - optional list of geometry simplification levels to be created"
    echo "       dbUser    - optional database user id"
    echo "       dbPort    - optional database port number"
    echo "       installDir- optional directory path to awips installation"
    echo "example: `basename $0` uscounties.shp mapdata County 0.064,0.016,0.004,0.001 awips 5432 /awips2"
    exit -1
fi

SHAPEFILEPATH=${1}
SCHEMA=`echo "${2}" | tr '[:upper:]' '[:lower:]'`
TABLE=`echo "${3}" | tr '[:upper:]' '[:lower:]'`
SIMPLEVS=${4}

if [ -z $5 ] ; then
    PGUSER=awips
else
    PGUSER=${5}
fi

if [ -z $6 ] ; then
    PGPORT=5432
else
    PGPORT=${6}
fi

if [ -z $7 ] ; then
    PGBINDIR=''
    PSQLBINDIR=''
else
    PGBINDIR=${7}/postgresql/bin/
    PSQLBINDIR=${7}/psql/bin/
fi

SHAPEFILENAME=`basename ${SHAPEFILEPATH}`
echo "  Importing ${SHAPEFILENAME} into ${SCHEMA}.${TABLE} ..."
${PSQLBINDIR}psql -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    DELETE FROM public.geometry_columns WHERE f_table_schema = '${SCHEMA}' AND f_table_name = '${TABLE}';
    DELETE from ${SCHEMA}.map_version WHERE table_name='${TABLE}';
    DROP TABLE IF EXISTS ${SCHEMA}.${TABLE}
"
${PGBINDIR}shp2pgsql -W LATIN1 -s 4326 -g the_geom -I ${SHAPEFILEPATH} ${SCHEMA}.${TABLE} | ${PSQLBINDIR}psql -d maps -U ${PGUSER} -q -p ${PGPORT} -f -
${PSQLBINDIR}psql -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    INSERT INTO ${SCHEMA}.map_version (table_name, filename) values ('${TABLE}','${SHAPEFILENAME}');
    SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom_0','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='${SCHEMA}' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
    UPDATE ${SCHEMA}.${TABLE} SET the_geom_0=ST_Segmentize(the_geom,0.1);
    CREATE INDEX ${TABLE}_the_geom_0_gist ON ${SCHEMA}.${TABLE} USING gist(the_geom_0);
"

if [ -n "$SIMPLEVS" ] ; then
    echo "  Creating simplification levels ${SIMPLEVS}..."
    IFS=",	 "
    for LEV in $SIMPLEVS ; do
        echo "    Creating simplified geometry level $LEV ..."
        IFS="."
        SUFFIX=
        for x in $LEV ; do SUFFIX=${SUFFIX}_${x} ; done
        ${PSQLBINDIR}psql -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
        SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom${SUFFIX}','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='${SCHEMA}' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
        UPDATE ${SCHEMA}.${TABLE} SET the_geom${SUFFIX}=ST_Segmentize(ST_Multi(ST_SimplifyPreserveTopology(the_geom,${LEV})),0.1);
        CREATE INDEX ${TABLE}_the_geom${SUFFIX}_gist ON ${SCHEMA}.${TABLE} USING gist(the_geom${SUFFIX});"
    done
fi
${PGBINDIR}vacuumdb -d maps -t ${SCHEMA}.${TABLE} -U ${PGUSER} -p ${PGPORT} -qz
