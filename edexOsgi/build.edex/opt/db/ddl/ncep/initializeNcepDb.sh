#!/bin/sh
if [ $# -lt 1 ] ; then
    echo
    echo usage: `basename $0` installDir [dbUser [dbPort [logFile]]]
    echo "       installDir- directory path to awips installation"
    echo "       dbUser    - optional database user id"
    echo "       dbPort    - optional database port number"
    echo "       logFile   - optional log file for output"
    echo "example: `basename $0` /awips awips 5432"
    exit -1
fi

PGHOME=${4}
PGBINDIR=${1}/bin
LOGFILE=${5}

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

# in /awips2/database/sqlScripts/share/sql/ncep/shapefiles/
for shp in `find ${4}/shapefiles -name "*.shp"` ; do
    base=`basename \`dirname $shp\``
    SIMPLEV=
    if [[ $base == 'Adjcstlbnds' \
       || $base == 'Airmetcstlbnds' \
       || $base == 'Akpsabnds' \
       || $base == 'Ascairways' \
       || $base == 'Ascarrfa' \
       || $base == 'Aschifiwo' \
       || $base == 'Asctropfirs' \
       || $base == 'Asctweb' \
       || $base == 'Ascwrzones' \
       || $base == 'Awcccfcan' \
       || $base == 'Atlbasin' \
       || $base == 'Bwus_bnd' \
       || $base == 'Bwus_label' \
       || $base == 'Bwx1224' \
       || $base == 'Countybnds' \
       || $base == 'Cpcus_bnd' \
       || $base == 'Cwabnds' \
       || $base == 'Elev_NAM1000' \
       || $base == 'Enh_area' \
       || $base == 'FA_Area' \
       || $base == 'FA_AreaX' \
       || $base == 'FA_Region' \
       || $base == 'Firbnds' \
       || $base == 'Firebnds' \
       || $base == 'FireWxAOR' \
       || $base == 'G2t_atl_bnd' \
       || $base == 'G2t_nwc' \
       || $base == 'G2t_pac_bnd' \
       || $base == 'G2t_tpc_bnd' \
       || $base == 'Gfa_conus' \
       || $base == 'Greatlakesbnds' \
       || $base == 'Hcnbnds' \
       || $base == 'Hpcsfc' \
       || $base == 'Lakesbnds' \
       || $base == 'Mzcntybnds' \
       || $base == 'Npsabnds' \
       || $base == 'Opcbnds' \
       || $base == 'PacBasin' \
       || $base == 'Pfzbnds' \
       || $base == 'Rfcbnds' \
       || $base == 'SPC_outlook_area' \
       || $base == 'Statebnds' \
       || $base == 'Tzbnds' \
       || $base == 'Us_ak' \
       ]]
    then
        SIMPLEV='0.064,0.016,0.004,0.001'
        ${PGHOME}/importNcepShapeFile.sh $shp bounds $base "$SIMPLEV" $PGUSER $PGPORT $1 $LOGFILE 2>&1
    fi
    
done

if [ -n "$LOGFILE" ] ; then
    ${1}/bin/vacuumdb -d ncep -U ${PGUSER} -p ${PGPORT} -vz >> $LOGFILE 2>&1
else
    ${1}/bin/vacuumdb -d ncep -U ${PGUSER} -p ${PGPORT} -vz
fi
