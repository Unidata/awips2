#!/bin/sh 

if [ $# -lt 3 ] ; then
    echo
    echo usage: `basename $0` shapefile schema table [simplev [dbUser [dbPort [installDir [logFile]]]]]
    echo "where: shapefile - pathname of the shape file to be imported"
    echo "       schema    - database schema where the shape file is to be imported"
    echo "       table     - database table where the shape file is to be imported"
    echo "       simplev   - optional list of geometry simplification levels to be created"
    echo "       dbUser    - optional database user id"
    echo "       dbPort    - optional database port number"
    echo "       installDir- optional directory path to awips installation"
    echo "       logFile   - optional log file for output"
    echo "example: `basename $0` uscounties.shp mapdata County 0.08,0.04,0.02,0.01 awips 5432 /awips"
    exit -1
fi

SHAPEFILE=${1}
SCHEMA=${2}
TABLE=`echo "${3}" | tr '[:upper:]' '[:lower:]'`
SIMPLEVS=${4}
LOGFILE=${8}

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
else
    PGBINDIR=${7}/bin/
fi

echo "  Importing `basename ${SHAPEFILE}` into ${SCHEMA}.${TABLE} ..."
if [ -n "$LOGFILE" ] ; then
    ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -c "
        DELETE FROM public.geometry_columns WHERE f_table_schema = '${SCHEMA}' AND f_table_name = '${TABLE}';
        DROP TABLE IF EXISTS ${SCHEMA}.${TABLE}
    " >> $LOGFILE 2>&1
    ${PGBINDIR}shp2pgsql -s 4326 -I ${SHAPEFILE} ${SCHEMA}.${TABLE} 2>> $LOGFILE | ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -f -  >> $LOGFILE 2>&1
    ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -c "
        SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom_0','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='${SCHEMA}' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
        UPDATE ${SCHEMA}.${TABLE} SET the_geom_0=ST_Segmentize(the_geom,0.1);
        CREATE INDEX ${TABLE}_the_geom_0_gist ON ${SCHEMA}.${TABLE} USING gist(the_geom_0);
    " >> $LOGFILE 2>&1
else
    ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -c "
        DELETE FROM public.geometry_columns WHERE f_table_schema = '${SCHEMA}' AND f_table_name = '${TABLE}';
        DROP TABLE IF EXISTS ${SCHEMA}.${TABLE}
    "
    ${PGBINDIR}shp2pgsql -s 4326 -I ${SHAPEFILE} ${SCHEMA}.${TABLE} | ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -f -
    ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -c "
        SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom_0','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='${SCHEMA}' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
        UPDATE ${SCHEMA}.${TABLE} SET the_geom_0=ST_Segmentize(the_geom,0.1);
        CREATE INDEX ${TABLE}_the_geom_0_gist ON ${SCHEMA}.${TABLE} USING gist(the_geom_0);
    "
fi 

if [ -n "$SIMPLEVS" ] ; then
    echo "  Creating simplification levels ${SIMPLEVS}..."
    IFS=",	 "
    for LEV in $SIMPLEVS ; do
	    echo "    Creating simplified geometry level $LEV ..."
        IFS="."
        SUFFIX=
        for x in $LEV ; do SUFFIX=${SUFFIX}_${x} ; done
        if [ -n "$LOGFILE" ] ; then
	        ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -c "
                SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom${SUFFIX}','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='${SCHEMA}' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
                UPDATE ${SCHEMA}.${TABLE} SET the_geom${SUFFIX}=ST_Segmentize(ST_Multi(ST_SimplifyPreserveTopology(the_geom,${LEV})),0.1);
                CREATE INDEX ${TABLE}_the_geom${SUFFIX}_gist ON ${SCHEMA}.${TABLE} USING gist(the_geom${SUFFIX});" >> $LOGFILE 2>&1
        else
	        ${PGBINDIR}psql -d ncep -U ${PGUSER} -q -p ${PGPORT} -c "
                SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom${SUFFIX}','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='${SCHEMA}' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
                UPDATE ${SCHEMA}.${TABLE} SET the_geom${SUFFIX}=ST_Segmentize(ST_Multi(ST_SimplifyPreserveTopology(the_geom,${LEV})),0.1);
                CREATE INDEX ${TABLE}_the_geom${SUFFIX}_gist ON ${SCHEMA}.${TABLE} USING gist(the_geom${SUFFIX});"
        fi
    done
fi
if [ -n "$LOGFILE" ] ; then
    ${PGBINDIR}vacuumdb -d ncep -t ${SCHEMA}.${TABLE} -U ${PGUSER} -p ${PGPORT} -qz >> $LOGFILE 2>&1
else
    ${PGBINDIR}vacuumdb -d ncep -t ${SCHEMA}.${TABLE} -U ${PGUSER} -p ${PGPORT} -qz
fi
