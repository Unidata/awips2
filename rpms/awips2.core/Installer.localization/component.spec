#
# AWIPS II Localization Spec File
#
Name: %{_component_name}
Summary: AWIPS II Localization Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: %{_component_name}
Requires: awips2-edex, awips2-edex-radar, awips2-edex-upc
Requires: awips2-edex-shapefiles
Obsoletes: awips2-localization-OAX < 16.1.4

%description
AWIPS II Site Localization.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

%build
# Build all WFO site localization Map Scales (Regional.xml and WFO.xml)
BUILD_DIR=%{_baseline_workspace}/rpms/awips2.core/Installer.localization/
UTIL=%{_baseline_workspace}/localization/utility
#file=$BUILD_DIR/wfo.dat
file=$BUILD_DIR/coords.dat
regional=$BUILD_DIR/coords_regional.dat
#<gridGeometry rangeX="LOWX HIGHX" rangeY="LOWY HIGHY" envelopeMinX="MINX" envelopeMaxX="MAXX" envelopeMinY="MINY" envelopeMaxY="MAXY">

for site in $(cat $file |cut -c -3)
do
   lat=$(cat $file   |grep $site | cut -d"," -f2  | tr -d '[[:space:]]')
   lon=$(cat $file   |grep $site | cut -d"," -f3  | tr -d '[[:space:]]')

   # <gridGeometry rangeX="LOWX HIGHX" rangeY="LOWY HIGHY" envelopeMinX="MINX" envelopeMaxX="MAXX" envelopeMinY="MINY" envelopeMaxY="MAXY">
   lowx=$(cat $file  |grep $site | cut -d"," -f4  | tr -d '[[:space:]]')
   highx=$(cat $file |grep $site | cut -d"," -f5  | tr -d '[[:space:]]')
   lowy=$(cat $file  |grep $site | cut -d"," -f6  | tr -d '[[:space:]]')
   highy=$(cat $file |grep $site | cut -d"," -f7  | tr -d '[[:space:]]')
   minx=$(cat $file  |grep $site | cut -d"," -f8  | tr -d '[[:space:]]')
   maxx=$(cat $file  |grep $site | cut -d"," -f9  | tr -d '[[:space:]]')
   miny=$(cat $file  |grep $site | cut -d"," -f10 | tr -d '[[:space:]]')
   maxy=$(cat $file  |grep $site | cut -d"," -f11 | tr -d '[[:space:]]')

   # CAVE
   CAVE_DIR=$UTIL/cave_static/site/$site
   mkdir -p $CAVE_DIR
   cp -R $BUILD_DIR/utility/cave_static/* $CAVE_DIR

   mkdir -p ~/awips2-builds/localization/localization/utility/cave_static/site/$site
   cp -R $BUILD_DIR/utility/cave_static/* ~/awips2-builds/localization/localization/utility/cave_static/site/$site

   grep -rl 'LOWX'  $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/LOWX/'$lowx'/g'
   grep -rl 'HIGHX' $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/HIGHX/'$highx'/g'
   grep -rl 'LOWY'  $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/LOWY/'$lowy'/g'
   grep -rl 'HIGHY' $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/HIGHY/'$highy'/g'
   grep -rl 'MINX'  $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/MINX/'$minx'/g'
   grep -rl 'MAXX'  $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/MAXX/'$maxx'/g'
   grep -rl 'MINY'  $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/MINY/'$miny'/g'
   grep -rl 'MAXY'  $CAVE_DIR/bundles/scales/WFO.xml | xargs sed -i 's/MAXY/'$maxy'/g'

   cp $CAVE_DIR/bundles/scales/WFO.xml ~/awips2-core/viz/com.raytheon.uf.viz.core.maps/localization/bundles/scales/WFO/$site.xml

   lowx=$(cat $regional  |grep $site | cut -d"," -f4  | tr -d '[[:space:]]')
   highx=$(cat $regional |grep $site | cut -d"," -f5  | tr -d '[[:space:]]')
   lowy=$(cat $regional  |grep $site | cut -d"," -f6  | tr -d '[[:space:]]')
   highy=$(cat $regional |grep $site | cut -d"," -f7  | tr -d '[[:space:]]')
   minx=$(cat $regional  |grep $site | cut -d"," -f8  | tr -d '[[:space:]]')
   maxx=$(cat $regional  |grep $site | cut -d"," -f9  | tr -d '[[:space:]]')
   miny=$(cat $regional  |grep $site | cut -d"," -f10 | tr -d '[[:space:]]')
   maxy=$(cat $regional  |grep $site | cut -d"," -f11 | tr -d '[[:space:]]')

   grep -rl 'LOWX'  $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/LOWX/'$lowx'/g'
   grep -rl 'HIGHX' $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/HIGHX/'$highx'/g'
   grep -rl 'LOWY'  $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/LOWY/'$lowy'/g'
   grep -rl 'HIGHY' $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/HIGHY/'$highy'/g'
   grep -rl 'MINX'  $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/MINX/'$minx'/g'
   grep -rl 'MAXX'  $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/MAXX/'$maxx'/g'
   grep -rl 'MINY'  $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/MINY/'$miny'/g'
   grep -rl 'MAXY'  $CAVE_DIR/bundles/scales/Regional.xml | xargs sed -i 's/MAXY/'$maxy'/g'

   grep -rl 'XXX' $CAVE_DIR | xargs sed -i 's/XXX/'$site'/g'
   grep -rl 'LATITUDE' $CAVE_DIR | xargs sed -i 's/LATITUDE/'$lat'/g'
   grep -rl 'LONGITUDE' $CAVE_DIR | xargs sed -i 's/LONGITUDE/'$lon'/g'
   # EDEX
   EDEX_DIR=$UTIL/edex_static/site/$site
   mkdir -p $EDEX_DIR
   cp -R $BUILD_DIR/utility/edex_static/* $EDEX_DIR/
   grep -rl 'XXX' $EDEX_DIR | xargs sed -i 's/XXX/'$site'/g'
done

# COMMON
COMMON_DIR=$UTIL/common_static
mkdir -p $COMMON_DIR
cp -R $BUILD_DIR/utility/common_static/* $COMMON_DIR/

%install
if [ ! -d %{_baseline_workspace}/%{_localization_directory} ]; then
   echo "ERROR: The specified localization directory does not exist - %{_localization_directory}."
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/utility
if [ $? -ne 0 ]; then
   exit 1
fi

# Copy the localization files
cp -rv %{_baseline_workspace}/localization/utility/* \
   ${RPM_BUILD_ROOT}/awips2/edex/data/utility
if [ $? -ne 0 ]; then
   exit 1
fi

%pre

%post
# verify the following exists:
if [ ! -d /awips2/data/maps ] ||
   [ ! -f /awips2/postgresql/bin/postmaster ] ||
   [ ! -f /awips2/postgresql/bin/pg_ctl ] ||
   [ ! -f /awips2/psql/bin/psql ] ||
   [ ! -f /awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh ] ||
   [ ! -f /awips2/postgresql/bin/pg_restore ]; then
   # we are missing a file or directory, exit
   exit 0
fi

localization_db_log="localization_db.log"
log_file="/awips2/database/sqlScripts/share/sql/${localization_db_log}"
if [ -f ${log_file} ]; then
   /bin/rm -f ${log_file}
fi
/bin/touch ${log_file}
chmod 666 ${log_file}

edex_utility="/awips2/edex/data/utility"
I_STARTED_POSTGRESQL="NO"
POSTGRESQL_RUNNING="NO"
a2_postmaster="/awips2/postgresql/bin/postmaster"
a2_pg_ctl="/awips2/postgresql/bin/pg_ctl"
DB_OWNER=`ls -l /awips2/ | grep -w 'data' | awk '{print $3}'`

echo "Determining if PostgreSQL is running ..." >> ${log_file}
su ${DB_OWNER} -c \
   "${a2_pg_ctl} status -D /awips2/data >> ${log_file} 2>&1"
RC=$?
echo "" >> ${log_file}

# start PostgreSQL if it is not running as the user that owns data
if [ ${RC} -eq 0 ]; then
   echo "INFO: PostgreSQL is running." >> ${log_file}
else
   echo "Starting PostgreSQL as User: ${DB_OWNER} ..." >> ${log_file}
   su ${DB_OWNER} -c \
      "${a2_postmaster} -D /awips2/data >> ${log_file} 2>&1 &"
   if [ $? -ne 0 ]; then
      echo "FATAL: Failed to start PostgreSQL." >> ${log_file}
      return 0
   fi
   # give PostgreSQL time to start.
   /bin/sleep 10
   I_STARTED_POSTGRESQL="YES"
fi
POSTGRESQL_RUNNING="YES"

static_shp_directory="${edex_utility}/edex_static/base/shapefiles"
a2_shp_script="/awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh"
  
/bin/bash ${a2_shp_script} \
   ${static_shp_directory}/NHAdomain/NHAdomain.shp mapdata nhadomain >> ${log_file} 2>&1 
if [ $? -ne 0 ]; then
   echo "FATAL: failed to import NHAdomain." >> ${log_file}
   return 0
fi
/bin/bash ${a2_shp_script} \
   ${static_shp_directory}/StormSurgeWW/StormSurgeWW.shp mapdata stormsurgeww >> ${log_file} 2>&1 
if [ $? -ne 0 ]; then
   echo "FATAL: failed to import StormSurgeWW." >> ${log_file}
   return 0
fi
   
echo "INFO: NHAdomain and StormSurgeWW shapefiles were successfully imported." >> ${log_file}

PSQL="/awips2/psql/bin/psql"
echo "Updating metadata.radar_spatial from common_static/base/radar/radarSpatial.sql"
${PSQL} -U awips -d metadata -q -f /awips2/edex/data/utility/common_static/base/radar/radarSpatial.sql >> ${log_file} 2>&1

if [ "${I_STARTED_POSTGRESQL}" = "YES" ]; then
   echo "" >> ${log_file}
   su ${DB_OWNER} -c \
      "${a2_pg_ctl} stop -D /awips2/data" >> ${log_file}
   if [ $? -ne 0 ]; then
      echo "WARNING: Failed to shutdown PostgreSQL." >> ${log_file}
      echo "         PostgreSQL will need to manually be shutdown." >> ${log_file}
   else
      # Give PostgreSQL time to shutdown.
      /bin/sleep 10
   fi
fi

exit 0

%preun

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(755,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/data
%dir /awips2/edex/data/utility
/awips2/edex/data/utility/*
