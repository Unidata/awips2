#
# AWIPS II edex-shapefiles Spec File
#
Name: awips2-edex-shapefiles
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Unidata
Packager: Michael James

AutoReq: no
provides: awips2-edex-shapefiles
requires: awips2
requires: awips2-edex-base

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

%description
AWIPS II Edex Shapefiles - includes the shapefiles required by AWIPS II.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "ERROR: The RPM Build Root has not been specified."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi

%build

%install
mkdir -p %{_build_root}/awips2/edex/data/utility/edex_static/base/shapefiles
if [ $? -ne 0 ]; then
   exit 1 
fi

SHAPEFILES=%{_awipscm_share}/awips2-static/maps/shapefiles
if [ ! -d ${SHAPEFILES} ]; then
   file ${SHAPEFILES}
   exit 1
fi

cp -r ${SHAPEFILES}/* \
   %{_build_root}/awips2/edex/data/utility/edex_static/base/shapefiles
if [ $? -ne 0 ]; then
   exit 1
fi

mkdir -p %{_build_root}/awips2/edex/data/utility/edex_static/base/shapefiles/WarnGenLoc
if [ $? -ne 0 ]; then
   exit 1 
fi


#create a list of all files packaged for /awips2/edex/data/utility
UTILITY=/awips2/edex/data/utility
if [ -d %{_build_root}/$UTILITY ]; then
   cd %{_build_root}/$UTILITY
   find . -type f > %{_build_root}/awips2/edex/util_filelist.%{name}.txt
fi

%pre

%post
a2_shp_script="/awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh"
shp_directory="/awips2/edex/data/utility/edex_static/base/shapefiles"
log_file="/awips2/database/sqlScripts/share/sql/maps/maps.log"
a2_postmaster="/awips2/postgresql/bin/postmaster"
a2_pg_ctl="/awips2/postgresql/bin/pg_ctl"
PSQL_INSTALL="/awips2/psql"
PSQL="${PSQL_INSTALL}/bin/psql"
if [ ! -f ${PSQL} ]; then
   exit 0
fi

function prepare()
{
   if [ "${POSTGRESQL_RUNNING}" = "YES" ]; then
      return 0
   fi
   
   
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
      /bin/sleep 5
      I_STARTED_POSTGRESQL="YES"
   fi
   POSTGRESQL_RUNNING="YES"
   
   return 0  
}

# Make sure postgres is running
prepare

# Import supplementary shapefiles
names=(NHAdomain StormSurgeWW)
for file in ${names[@]}
do
  # Check if the table exists (lowercase) and if not then import
  MAPS_DB=`${PSQL} -U awips maps -c "\dt mapdata.*"|grep ${file,,}|awk '{print $3}'`
  if [ "${MAPS_DB}" != ${file,,} ]; then
    /bin/bash ${a2_shp_script} ${shp_directory}/${file}/${file}.shp \
      mapdata ${file,,} 0.064,0.016,0.004,0.001 \
      awips 5432 /awips2 >> ${log_file} 2>&1
    if [ $? -ne 0 ]; then
      echo "FATAL: failed to import the $file shapefile."
    fi
  else 
    echo "$file already exists in maps.mapdata."
  fi
done

# if we started PostgreSQL, shut it down
if [ "${I_STARTED_POSTGRESQL}" = "YES" ]; then
   echo "" >> ${log_file}

   su ${DB_OWNER} -c \
      "${a2_pg_ctl} stop -D /awips2/data" >> ${log_file}
   if [ $? -ne 0 ]; then
      echo "WARNING: Failed to shutdown PostgreSQL." >> ${log_file}
      echo "         PostgreSQL will need to manually be shutdown." >> ${log_file}
   else
      # Give PostgreSQL time to shutdown.
      /bin/sleep 5
   fi
fi

#change date stamp of utility files
UTILITY=/awips2/edex/data/utility
UTIL_FILENAME=/awips2/edex/util_filelist.%{name}.txt
if [ -d $UTILITY ] && [ -f $UTIL_FILENAME ]; then
 while read fileName
 do
  touch "$UTILITY/$fileName"
 done < $UTIL_FILENAME
 rm -f $UTIL_FILENAME
fi

%preun

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(775,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*
