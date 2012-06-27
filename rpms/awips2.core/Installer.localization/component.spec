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
Packager: Bryan Kowal

AutoReq: no
provides: %{_component_name}

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

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/utility

%build

%install
if [ ! -d %{_baseline_workspace}/%{_localization_directory} ]; then
   echo "ERROR: The specified localization directory does not exist - %{_localization_directory}."
   exit 1
fi

# Copy the localization.
cp -rv %{_baseline_workspace}/%{_localization_directory}/utility/* \
   ${RPM_BUILD_ROOT}/awips2/edex/data/utility
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

%pre

%post
# determine if we include ffmp shapefiles
edex_utility="/awips2/edex/data/utility"
site_directory="${edex_utility}/edex_static/site/%{_localization_site}"
ffmp_shp_directory="${site_directory}/shapefiles/FFMP"

# if we do not, exit
if [ ! -d ${ffmp_shp_directory} ]; then
   exit 0
fi

# verify the both the basins and streams shapefile are present.
if [ ! -f ${ffmp_shp_directory}/FFMP_aggr_basins.shp ] ||
   [ ! -f ${ffmp_shp_directory}/FFMP_ref_sl.shp ]; then
   # if they are not, exit
   exit 0
fi

# verify that the files the streams and basins shapefile depend on
# are present.
if [ ! -f ${ffmp_shp_directory}/FFMP_aggr_basins.dbf ] ||
   [ ! -f ${ffmp_shp_directory}/FFMP_aggr_basins.shx ] ||
   [ ! -f ${ffmp_shp_directory}/FFMP_ref_sl.dbf ] ||
   [ ! -f ${ffmp_shp_directory}/FFMP_ref_sl.shx ]; then
   # if they are not, exit
   exit 0
fi

# shapefiles exist
shapefile_import_log="shapefile_import-%{_localization_site}.log"
log_file="/awips2/database/sqlScripts/share/sql/${shapefile_import_log}"
if [ -f ${log_file} ]; then
   /bin/rm -f ${log_file}
fi
/bin/touch ${log_file}
chmod 666 ${log_file}

# only import the shapefiles, if we are on the same machine as the db
# verify the following exists:
#   1) /awips2/data/maps
#   2) /awips2/postgresql/bin/postmaster
#   3) /awips2/postgresql/bin/pg_ctl
#   4) /awips2/psql/bin/psql
#   5) /awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh
if [ ! -d /awips2/data/maps ] ||
   [ ! -f /awips2/postgresql/bin/postmaster ] ||
   [ ! -f /awips2/postgresql/bin/pg_ctl ] ||
   [ ! -f /awips2/psql/bin/psql ] ||
   [ ! -f /awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh ]; then
   # we are missing a file or directory, exit
   exit 0
fi

a2_postmaster="/awips2/postgresql/bin/postmaster"
a2_pg_ctl="/awips2/postgresql/bin/pg_ctl"
a2_shp_script="/awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh"

echo "Importing the FFMP Shapefiles ... Please Wait."
/bin/date > ${log_file}
echo "Preparing to import the FFMP shapefiles ..." >> ${log_file}

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
      exit 0
   fi
   
   # give PostgreSQL time to start.
   /bin/sleep 10
   I_STARTED_POSTGRESQL="YES"
fi

echo "" >> ${log_file}
# import the shapefiles; log the output

# import the ffmp basins
/bin/bash ${a2_shp_script} \
   ${ffmp_shp_directory}/FFMP_aggr_basins.shp \
   mapdata ffmp_basins 0.064,0.016,0.004,0.001 \
   awips 5432 /awips2 >> ${log_file} 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: failed to import the FFMP basins." >> ${log_file}
   exit 0
fi

# import the ffmp streams
/bin/bash ${a2_shp_script} \
   ${ffmp_shp_directory}/FFMP_ref_sl.shp \
   mapdata ffmp_streams 0.064,0.016,0.004,0.001 \
   awips 5432 /awips2 >> ${log_file} 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: failed to import the FFMP streams." >> ${log_file}
   exit 0
fi

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

# indicate success
echo "INFO: The FFMP shapefiles were successfully imported." >> ${log_file}
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