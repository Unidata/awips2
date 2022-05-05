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
    echo usage: `basename $0` pointsInfo schema table [dbUser [dbPort [installDir]]]
    echo "where: pointsInfo- pathname of the pointsInfo file to be imported"
    echo "       schema    - database schema where the shape file is to be imported"
    echo "       table     - database table where the shape file is to be imported"
    echo "       dbUser    - optional database user id"
    echo "       dbPort    - optional database port number"
    echo "       installDir- optional directory path to awips installation"
    echo "example: `basename $0` LocalCitiesInfo.txt mapdata localcities awips 5432 /awips2"
    exit -1
fi

FILEPATH=${1}
SCHEMA=${2}
TABLE=`echo "${3}" | tr '[:upper:]' '[:lower:]'`

if [ -z $4 ] ; then
    PGUSER=awipsadmin
else
    PGUSER=${4}
fi

if [ -z $5 ] ; then
    PGPORT=5432
else
    PGPORT=${5}
fi

if [ -z $6 ] ; then
    PGBINDIR=''
    PSQLBINDIR=''
else
    PGBINDIR=${6}/postgresql/bin/
    PSQLBINDIR=${6}/psql/bin/
fi

psql="a2dbauth ${PSQLBINDIR}psql"

FILENAME=`basename ${FILEPATH}`
echo "  Importing ${FILENAME} into ${SCHEMA}.${TABLE} ..."
${psql} -d maps -U ${PGUSER} -q -p ${PGPORT} -c "
    DELETE FROM public.geometry_columns WHERE f_table_schema = '${SCHEMA}' AND f_table_name = '${TABLE}';
    DROP TABLE IF EXISTS ${SCHEMA}.${TABLE};
    CREATE TABLE "${SCHEMA}"."${TABLE}" (gid serial PRIMARY KEY,
        "st" character varying(3),
        "name" character varying(50),
        "prog_disc" bigint,
        "warngenlev" character varying(14));
    DELETE from ${SCHEMA}.map_version WHERE table_name='${TABLE}';
    INSERT INTO ${SCHEMA}.map_version (table_name, filename) values ('${TABLE}','${FILENAME}');
    SELECT AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom','4326','POINT',2);
    CREATE INDEX "${TABLE}_the_geom_gist"
      ON "${SCHEMA}"."${TABLE}"
      USING gist
      (the_geom);
"
sed -e "s/'/''/g;s/\([-\.[:digit:]]*\)[[:blank:]]\{1,\}\([-\.[:digit:]]*\)[[:blank:]]\{1,\}\([[:digit:]]*\)[[:blank:]]\{1,\}\([[:upper:]]*\)[[:blank:]]\{1,\}\([^|]*\)|\([[:digit:]]*\).*/INSERT INTO \"${SCHEMA}\".\"${TABLE}\"(st,name,prog_disc,warngenlev,the_geom) VALUES('\4','\5',\3,\6,ST_GeomFromText('POINT(\2 \1)',4326));/" $FILEPATH | \
    ${psql} -d maps -U ${PGUSER} -q -p ${PGPORT}
    
if [ -d ${PGBINDIR} ]; then
a2dbauth ${PGBINDIR}vacuumdb -d maps -t ${SCHEMA}.${TABLE} -U ${PGUSER} -p ${PGPORT} -qz
fi
