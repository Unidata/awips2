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
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# 03/25/2014    #2664     randerso    Added support for importing non-WGS84 shape files
# 10/23/2014    #3685     randerso    Fixed bug where .prj was not recognized when shape file
#                                     was in the current directory (no directory specified)
# 02/11/2016    #5348     randerso    Add code to create a county_names view into the county table
# 01/23/2017    #6097     randerso    Removed unnecessary command line parameters. Use ogr2ogr to 
#                                     to convert shapefile to WGS84 (EPSG:4326).
# 08/02/2017    #6362     randerso    Add code to create alaska_marine view     
# 08/28/2017    #6097     randerso    Made command line backward compatible with warning 
#                                     message if extra arguments are present 
# 04/11/2018    #7140     tgurney     Use a2dbauth
# 03/25/2021    #8398     randerso    Added PROJ_LIB for PostGIS 2.4
#     
##

function usage()
{
    echo
    echo usage: `basename $0` shapefile table
    echo "where: shapefile - pathname of the shape file to be imported"
    echo "       table     - database table where the shape file is to be imported"
    echo "example: `basename $0` ~/Downloads/c_11au16.shp County"
}

if [ $# -lt 2 ] ; then
    usage
    exit -1
fi

if [ $# -gt 2 ] ; then
    echo
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "!!!                                                                                  !!!"
    echo "!!! WARNING: importShapeFile.sh HAS BEEN CHANGED. OBSOLETE ARGUMENTS WILL BE IGNORED !!!"
    echo "!!! See updated usage information below for new command syntax.                      !!!"
    echo "!!!                                                                                  !!!"
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" 
    usage
    echo
fi

SHP_PATH=`readlink -f ${1}`
SHP_DIR="${SHP_PATH%/*}"    # shape file dir
SHP_NAME="${SHP_PATH##*/}"  # shape file name with extension
SHP_BASE="${SHP_NAME%.*}"   # shape file name without extension
SHP_EXT="${SHP_NAME##*.}"   # shape file extension
PRJ_PATH="${SHP_DIR}/${SHP_BASE}.prj"

TABLE=`echo "${2}" | tr '[:upper:]' '[:lower:]'`
if [[ "${TABLE}" == "mapdata" && $# -gt 2 ]] ; then
    TABLE=`echo "${3}" | tr '[:upper:]' '[:lower:]'`
fi

PGUSER=awipsadmin
PGPORT=5432
PGBINDIR=/awips2/postgresql/bin/
PSQLBINDIR=/awips2/psql/bin/
SIMPLEVS=( "0.064" "0.016" "0.004" "0.001" )

psql="a2dbauth ${PSQLBINDIR}psql"
vacuumdb="a2dbauth ${PGBINDIR}vacuumdb"


if [ ! -r ${SHP_PATH} ]; then
    echo
    echo "ERROR, file not found or is not readable: ${SHP_PATH}"
    exit -1
fi

SRC_SRID=""
if [ ! -f $PRJ_PATH ] ; then
    echo
    echo "WARNING, no projection file (.prj) found. Assuming source projection is WGS84."
    SRC_SRID="-s_srs EPSG:4326"
fi

echo "  Importing ${SHP_NAME} into mapdata.${TABLE} ..."

export GDAL_DATA=/awips2/postgresql/share/gdal
export PROJ_LIB=/awips2/postgresql/share/proj

#
# If updating county table drop the county names view
#
if [ "county" == "${TABLE}" ] ; then
    ${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
        DROP VIEW IF EXISTS mapdata.county_names;
    "
#
# If updating marine or offshore zones drop Alaska marine view
#
elif [ "marinezones" == "${TABLE}" ] || [ "offshore" == "${TABLE}" ] ; then
    ${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
        DROP VIEW IF EXISTS mapdata.alaska_marine;
    "
fi

${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    DELETE FROM public.geometry_columns WHERE f_table_schema = 'mapdata' AND f_table_name = '${TABLE}';
    DELETE FROM mapdata.map_version WHERE table_name='${TABLE}';
    DROP TABLE IF EXISTS mapdata.${TABLE}
"
TEMPDIR=`mktemp --directory`
a2dbauth -f ${PGBINDIR}ogr2ogr -f "ESRI Shapefile" -overwrite ${TEMPDIR} ${SHP_PATH} ${SRC_SRID} -t_srs EPSG:4326
a2dbauth -f ${PGBINDIR}shp2pgsql -W LATIN1 -s 4326 -g the_geom -I ${TEMPDIR}/${SHP_NAME} mapdata.${TABLE} | ${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -f -
${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    INSERT INTO mapdata.map_version (table_name, filename) values ('${TABLE}','${SHP_NAME}');
    SELECT AddGeometryColumn('mapdata','${TABLE}','the_geom_0','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='mapdata' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
    UPDATE mapdata.${TABLE} SET the_geom_0=ST_Segmentize(the_geom,0.1);
    CREATE INDEX ${TABLE}_the_geom_0_gist ON mapdata.${TABLE} USING gist(the_geom_0);
"
rm -rf ${TEMPDIR}

#
# Create simplification levels for non-point data
#
TYPE=`${psql} -d maps -U ${PGUSER} -qt -p ${PGPORT} -c "SELECT type FROM public.geometry_columns WHERE f_table_schema='mapdata' and f_table_name='${TABLE}' and f_geometry_column='the_geom';"`
if [[ $TYPE != *"POINT"* ]] ; then
    for LEV in "${SIMPLEVS[@]}" ; do
        echo "    Creating simplified geometry level $LEV ..."
        SUFFIX=${LEV/./_}
        ${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
        SELECT AddGeometryColumn('mapdata','${TABLE}','the_geom_${SUFFIX}','4326',(SELECT type FROM public.geometry_columns WHERE f_table_schema='mapdata' and f_table_name='${TABLE}' and f_geometry_column='the_geom'),2);
        UPDATE mapdata.${TABLE} SET the_geom_${SUFFIX}=ST_Segmentize(ST_Multi(ST_SimplifyPreserveTopology(the_geom,${LEV})),0.1);
        CREATE INDEX ${TABLE}_the_geom_${SUFFIX}_gist ON mapdata.${TABLE} USING gist(the_geom_${SUFFIX});"
    done
fi

#
# If updating county table recreate the county names view
#
if [ "county" == "${TABLE}" ] ; then
${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
        CREATE OR REPLACE VIEW mapdata.county_names AS
        SELECT countyname as name, ST_SetSRID(ST_Point(lon,lat), 4326)::geometry(Point, 4326) as the_geom
        FROM mapdata.county;"

#
# If updating marine or offshore zones recreate the Alaska marine view
#
elif [ "marinezones" == "${TABLE}" ] || [ "offshore" == "${TABLE}" ] ; then
${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    CREATE OR REPLACE VIEW mapdata.alaska_marine AS
        SELECT CAST(ROW_NUMBER() OVER(ORDER BY id) AS INT) GID, * FROM (
            SELECT id, wfo, name, lat, lon, 
                the_geom, the_geom_0, the_geom_0_064, the_geom_0_016, the_geom_0_004, the_geom_0_001 
            FROM mapdata.marinezones WHERE wfo IN ('AFC', 'AFG', 'AJK') 
            UNION 
            SELECT id, wfo, name, lat, lon, 
                the_geom, the_geom_0, the_geom_0_064, the_geom_0_016, the_geom_0_004, the_geom_0_001 
            FROM mapdata.offshore WHERE wfo IN ('AFC', 'AFG', 'AJK')
        ) a;"
fi

${vacuumdb} -d maps -t mapdata.${TABLE} -U ${PGUSER} -p ${PGPORT} -vfz
