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
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
Provides: %{_component_name}
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
# Build all WFO site localization Map Scales (States.xml, WFO.xml)
BUILD_DIR=%{_baseline_workspace}/rpms/awips2.core/Installer.localization/
UTIL=%{_baseline_workspace}/localization/utility
file=$BUILD_DIR/wfo.dat
for site in $(cat $file |cut -c -3)
do
   lat=$(cat $file |grep $site | cut -f9)
   lon=$(cat $file |grep $site | cut -f10)
   # CAVE
   CAVE_DIR=$UTIL/cave_static/site/$site
   mkdir -p $CAVE_DIR
   cp -R $BUILD_DIR/utility/cave_static/* $CAVE_DIR
   grep -rl 'XXX' $CAVE_DIR | xargs sed -i 's/XXX/'$site'/g'
   grep -rl 'LATITUDE' $CAVE_DIR | xargs sed -i 's/LATITUDE/'$lat'/g'
   grep -rl 'LONGITUDE' $CAVE_DIR | xargs sed -i 's/LONGITUDE/'$lon'/g'
   # EDEX
   EDEX_DIR=$UTIL/edex_static/site/$site
   mkdir -p $EDEX_DIR
   cp -R $BUILD_DIR/utility/edex_static/* $EDEX_DIR/
   grep -rl 'XXX' $EDEX_DIR | xargs sed -i 's/XXX/'$site'/g'
done

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
# only import the shapefiles and/or hydro databases, if we are on 
# the same machine as the db.
# verify the following exists:
#   1) /awips2/data/maps
#   2) /awips2/postgresql/bin/postmaster
#   3) /awips2/postgresql/bin/pg_ctl
#   4) /awips2/psql/bin/psql
#   5) /awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh
#   6) /awips2/postgresql/bin/pg_restore
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

function prepare()
{
   if [ "${POSTGRESQL_RUNNING}" = "YES" ]; then
      return 0
   fi
   
   local a2_postmaster="/awips2/postgresql/bin/postmaster"
   local a2_pg_ctl="/awips2/postgresql/bin/pg_ctl"
   
   # retrieve the owner of the database
   DB_OWNER=`ls -l /awips2/ | grep -w 'data' | awk '{print $3}'`
   
   # determine if PostgreSQL is running
   I_STARTED_POSTGRESQL="NO"
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
   
   return 0  
}

function restartPostgreSQL()
{
   if [ "${POSTGRESQL_RUNNING}" = "NO" ]; then
      return 0
   fi
   
   local a2_pg_ctl="/awips2/postgresql/bin/pg_ctl"
   
   # retrieve the owner of the database
   DB_OWNER=`ls -l /awips2/ | grep -w 'data' | awk '{print $3}'`
   
   echo "Restarting PostgreSQL ..." >> ${log_file}
   su ${DB_OWNER} -c \
      "${a2_pg_ctl} restart -D /awips2/data" >> ${log_file}
   sleep 20
   echo "PostgreSQL restart complete ..." >> ${log_file}
}

function importShapefiles()
{   
   local site_directory="${edex_utility}/edex_static/site/OAX"
   
   # determine if we include shapefiles
   local ffmp_shp_directory="${site_directory}/shapefiles/FFMP"
   local wg_shp_directory="${site_directory}/shapefiles/warngen"
   
   # if we do not, halt
   if [ ! -d ${ffmp_shp_directory} ]; then
      echo "${ffmp_shp_directory} does not exist, returning ..." >> ${log_file}   
      return 0
   fi
   
   # shapefiles exist
   
   prepare
   
   # verify the both the basins and streams shapefile are present.
   if [ ! -f ${ffmp_shp_directory}/FFMP_aggr_basins.shp ] ||
      [ ! -f ${ffmp_shp_directory}/FFMP_ref_sl.shp ]; then
      # if they are not, exit
      echo "ffmp_shp_directory files do not exist, returning ..." >> ${log_file}   
      return 0
   fi
  
   # verify the warngen location shapefile exists
   if [ ! -f ${wg_shp_directory}/wg23fe15.shp ]; then
      # if it does not, exit
      echo "warngenloc files do not exist, returning ..." >> ${log_file}   
      return 0
   fi
   
   # verify that the files the streams and basins shapefile depend on
   # are present.
   if [ ! -f ${ffmp_shp_directory}/FFMP_aggr_basins.dbf ] ||
      [ ! -f ${ffmp_shp_directory}/FFMP_aggr_basins.shx ] ||
      [ ! -f ${ffmp_shp_directory}/FFMP_ref_sl.dbf ] ||
      [ ! -f ${ffmp_shp_directory}/FFMP_ref_sl.shx ]; then
      # if they are not, exit
      echo "ffmp basin files do not exist, returning ..." >> ${log_file}   
      return 0
   fi
   
   local a2_shp_script="/awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh"
   
   echo "Importing the FFMP and WarnGen Shapefiles ... Please Wait."
   /bin/date >> ${log_file}
   echo "Preparing to import the FFMP shapefiles ..." >> ${log_file}   
   
   echo "" >> ${log_file}
   # import the shapefiles; log the output
   
   # import the ffmp basins
   /bin/bash ${a2_shp_script} \
      ${ffmp_shp_directory}/FFMP_aggr_basins.shp \
      mapdata ffmp_basins 0.064,0.016,0.004,0.001 \
      awips 5432 /awips2 >> ${log_file} 2>&1
   if [ $? -ne 0 ]; then
      echo "FATAL: failed to import the FFMP basins." >> ${log_file}
      return 0
   fi
   
   # import the ffmp streams
   /bin/bash ${a2_shp_script} \
      ${ffmp_shp_directory}/FFMP_ref_sl.shp \
      mapdata ffmp_streams 0.064,0.016,0.004,0.001 \
      awips 5432 /awips2 >> ${log_file} 2>&1
   if [ $? -ne 0 ]; then
      echo "FATAL: failed to import the FFMP streams." >> ${log_file}
      return 0
   fi
   
    # import the warngen boundaries for OAX
   /bin/bash ${a2_shp_script} \
      ${wg_shp_directory}/wg23fe15.shp mapdata warngenloc >> ${log_file} 2>&1 
   if [ $? -ne 0 ]; then
      echo "FATAL: failed to import the WarnGen locations." >> ${log_file}
      return 0
   fi

   # indicate success
   echo "INFO: The FFMP and WarnGen shapefiles were successfully imported." >> ${log_file}
   return 0
}

function removeHydroDbDirectory()
{
   # remove the hydro db directory since it is not officially part
   # of the localization.

   local hydro_db_directory="${edex_utility}/common_static/site/OAX/hydro/db"
   
   if [ -d ${hydro_db_directory} ]; then
      rm -rf ${hydro_db_directory}
      if [ $? -ne 0 ]; then
         echo "WARNING: Failed to remove hydro db directory from localization."
         echo "         Please remove directory manually: ${hydro_db_directory}."
      fi
   fi
   
   return 0
}

function restoreHydroDb()
{
   local hydro_db_directory="${edex_utility}/common_static/site/OAX/hydro/db"
   
   if [ ! -d ${hydro_db_directory} ]; then
      return 0
   fi
   
   # hydro databases exist
   prepare   
   
   # verify that the hydro database definition is present
   if [ ! -f ${hydro_db_directory}/hydroDatabases.sh ]; then
      return 0
   fi
   
   # discover the hydro databases
   source ${hydro_db_directory}/hydroDatabases.sh
   
   # ensure that the expected information has been provided
   if [ "${DAMCAT_DATABASE}" = "" ] ||
      [ "${DAMCAT_SQL_DUMP}" = "" ] ||
      [ "${IHFS_DATABASE}" = "" ] ||
      [ "${IHFS_SQL_DUMP}" = "" ]; then
      echo "Sufficient information has not been provided for the Hydro Restoration!" \
         >> ${log_file}
      return 0
   fi
   
   # ensure that the specified databases are available for import
   if [ ! -f ${hydro_db_directory}/${DAMCAT_DATABASE} ] ||
      [ ! -f ${hydro_db_directory}/${IHFS_DATABASE} ]; then
      echo "The expected Hydro Database Exports are not present!" >> ${log_file}
      return 0
   fi
   
   # update pg_hba.conf
   
   local default_damcat="dc_ob7oax"
   local default_ihfs="hd_ob92oax"
   local pg_hba_conf="/awips2/data/pg_hba.conf"
   
   # update the entry for the damcat database
   perl -p -i -e "s/${default_damcat}/${DAMCAT_DATABASE}/g" ${pg_hba_conf}
   if [ $? -ne 0 ]; then
      echo "Failed to update damcat database in ${pg_hba_conf}!" >> ${log_file}
      return 0
   fi
   
   # update the entry for the ihfs database
   perl -p -i -e "s/${default_ihfs}/${IHFS_DATABASE}/g" ${pg_hba_conf}
   if [ $? -ne 0 ]; then
      echo "Failed to update ihfs database in ${pg_hba_conf}!" >> ${log_file}
      return 0
   fi
   
   # prepare PostgreSQL
   restartPostgreSQL
   
   echo "Restoring the Hydro Databases ... Please Wait."
   /bin/date >> ${log_file}
   echo "Preparing to restore the Hydro databases ..." >> ${log_file}
   
   local a2_pg_restore="/awips2/postgresql/bin/pg_restore"
   
   # perform the restoration
   echo "Restoring Database ${DAMCAT_DATABASE} ..." >> ${log_file}
   ${a2_pg_restore} -U awips -C -d postgres ${hydro_db_directory}/${DAMCAT_DATABASE} \
      >> ${log_file} 2>&1
   # do not check the return code because any errors encountered during
   # the restoration may cause the return code to indicate a failure even
   # though the database was successfully restored.
   
   echo "" >> ${log_file} 
   
   echo "Restoring Database ${IHFS_DATABASE} ..." >> ${log_file}
   ${a2_pg_restore} -U awips -C -d postgres ${hydro_db_directory}/${IHFS_DATABASE} \
      >> ${log_file} 2>&1
   # do not check the return code because any errors encountered during
   # the restoration may cause the return code to indicate a failure even
   # though the database was successfully restored.
   
   # indicate success
   echo "INFO: The Hydro databases were successfully restored." >> ${log_file}
}

#importShapefiles
restoreHydroDb
removeHydroDbDirectory

a2_pg_ctl="/awips2/postgresql/bin/pg_ctl"
# if we started PostgreSQL, shutdown PostgreSQL
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
