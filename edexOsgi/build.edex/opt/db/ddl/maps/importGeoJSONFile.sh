#!/bin/bash 
##
#
# For permanent static GeoJSON file, use this script to import into maps database
# Adding a MapDescriptor and other installation steps will be same as Shapefiles
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Jun 10, 2016 17912      pwang       Added support for importing GeoJSON files
# Apr 11, 2018 7140       tgurney     Use a2dbauth
# Jul  1, 2021 8544       tgurney     Remove PGBINDIR and PSQLBINDIR, not
#                                     needed anymore. Update GDAL data path
# Mar 28, 2025 2037812    smoorthy    Define pghost appropriately for db calls.
##

function usage()
{
    echo
echo usage: `basename $0` filePath fileName schema tableName [AWIPS2_install_dir [dbUser [dbPort [simplev]]]]
    echo "where: filePath  - pathname of the GeoJSON file to be imported"
    echo "       fileName  - the file name of the GeoJSON file to be imported"
    echo "       schema    - database schema, usually mapdata, where the GeoJSON file is to be imported"
    echo "       tableName - database table name where the GeoJSON file is to be imported"
    echo "       AWIPS2_install_dir    - ignored (exists for backward compatibility only) "
    echo "       dbUser    - optional database user id, default to awipsadmin"
    echo "       dbPort    - optional database port number"
    echo "       simplev   - optional list of geometry simplification levels to be created"
}

if [ $# -lt 4 ] ; then
    usage
    exit -1
fi

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


GJS_PATH=`readlink -f ${1}`
GJS_NAME=${2}  # GeoJSON file name with extension
SCHEMA=`echo "${3}" | tr '[:upper:]' '[:lower:]'`
TABLE=`echo "${4}" | tr '[:upper:]' '[:lower:]'`


if [ -z "${DB_HOST}" ] ; then
    DBHOST=localhost
else
    DBHOST="${DB_HOST}"
fi

# $5 is awips2_install_dir, ignored

if [ -z "${6}" ] ; then
    PGUSER=awipsadmin
else
    PGUSER=${6}
fi

if [ -z "${7}" ] ; then
    PGPORT=5432
else
    PGPORT=${7}
fi


if [ -z "${8}" ] ; then
    SIMPLEVS=''
else
    SIMPLEVS=${8}
fi

psql="a2dbauth psql"

# set env
export GDAL_DATA=/awips2/python/share/gdal

echo "${GJS_PATH}  ${GJS_NAME}" 
echo "  Importing ${GJS_NAME} into ${SCHEMA}.${TABLE} ..."
${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    DELETE FROM public.geometry_columns WHERE f_table_schema = '${SCHEMA}' AND f_table_name = '${TABLE}';
    DELETE FROM ${SCHEMA}.map_version WHERE table_name='${TABLE}';
    DROP TABLE IF EXISTS ${SCHEMA}.${TABLE}
"

a2dbauth -f ogr2ogr \
    -f PostgreSQL PG:"host=$DBHOST user=${PGUSER} port=${PGPORT} dbname=maps password=awips" \
    ${GJS_PATH}/${GJS_NAME} \
    -nln ${SCHEMA}.${TABLE} \
    -lco GEOMETRY_NAME=the_geom \
    -lco FID=gid \
    -t_srs "EPSG:4326";

${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    INSERT INTO ${SCHEMA}.map_version (table_name, filename) values ('${TABLE}','${GJS_NAME}');
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
        ${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
        SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom${SUFFIX}','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='${SCHEMA}' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
        UPDATE ${SCHEMA}.${TABLE} SET the_geom${SUFFIX}=ST_Segmentize(ST_Multi(ST_SimplifyPreserveTopology(the_geom,${LEV})),0.1);
        CREATE INDEX ${TABLE}_the_geom${SUFFIX}_gist ON ${SCHEMA}.${TABLE} USING gist(the_geom${SUFFIX});"
    done
fi
a2dbauth vacuumdb -d maps -t ${SCHEMA}.${TABLE} -U ${PGUSER} -p ${PGPORT} -vfz
