%define _component_name           awips2-maps-database
%define _component_project_dir    awips2.core/Installer.maps-database
#
# AWIPS II Maps Database Spec File
#
Name: %{_component_name}
Summary: AWIPS II Maps Database
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
Provides: awips2-maps-database
Requires: awips2-database

%description
AWIPS II Maps Database - includes the
maps database (when awips2-postgresql is installed).

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

%build

%install
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/maps
if [ $? -ne 0 ]; then
   exit 1
fi

# Copy the sql that is needed to create the maps database.
PATH_TO_DDL="build.edex/opt/db/ddl"
PATH_TO_MAPS_DDL="${PATH_TO_DDL}/maps"
cp -r %{_baseline_workspace}/${PATH_TO_MAPS_DDL}/* \
   ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/maps

PATH_TO_STATIC_DDL="%{_static_files}/maps/db"
if [ ! -d ${PATH_TO_STATIC_DDL} ]; then
   echo "File ${PATH_TO_STATIC_DDL} not found!"
   exit 1
fi
cp ${PATH_TO_STATIC_DDL}/* \
   ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/maps
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

SQL_LOG="${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/maps/maps.log"
# So that it will automatically be removed when the rpm is removed.
touch ${SQL_LOG}

%pre
# Verify that one of the official AWIPS II PostgreSQL configuration files exist.
if [ ! -f /awips2/database/data/postgresql.conf ]; then
   echo "ERROR: /awips2/database/data/postgresql.conf does not exist. However, "
   echo "       the AWIPS II PostgreSQL Configuration RPM is installed. "
   echo "       If you recently uninstalled awips2-database and purged "
   echo "       the /awips2/database/data directory, you will need to re-install "
   echo "       the AWIPS II PostgreSQL configuration rpm so that the "
   echo "       postgresql.conf file will be restored."
   exit 1
fi

%post
if [ "${1}" = "2" ]; then
   exit 0
fi
function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II Maps Database Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m Check the installation log: /awips2/database/sqlScripts/share/sql/maps/maps.log\e[m"
   echo ""
   if [ -f /awips2/database/sqlScripts/share/sql/maps/maps.log ]; then
      tail -n 6 /awips2/database/sqlScripts/share/sql/maps/maps.log
   fi
   exit 1
}

function cleanup_pg_hba()
{
   cp /tmp/pg_hba.conf.orig /awips2/database/data/pg_hba.conf
   if [ $? -ne 0 ]; then
      exit 1
   fi

   # trigger a reload of the configuration in PostgreSQL
   su ${DB_OWNER} -c \
      "${POSTGRESQL_INSTALL}/bin/pg_ctl reload -D /awips2/database/data > /dev/null 2>&1"
   if [ $? -ne 0 ]; then
      exit 1
   fi
   rm -f /tmp/pg_hba.conf.orig
   if [ $? -ne 0 ]; then
      exit 1
   fi
}

function grant_permissions()
{
   cp /awips2/database/data/pg_hba.conf /tmp/pg_hba.conf.orig
   if [ $? -ne 0 ]; then
      exit 1
   fi

   cat >> "/awips2/database/data/pg_hba.conf" <<EOF

# TYPE  DATABASE    USER        CIDR-ADDRESS          METHOD
# ===== Maps Configuration (Install) =====
local        all         all                               trust

EOF

   if [ $? -ne 0 ]; then
      cleanup_pg_hba
      exit 1
   fi

   # trigger a reload of the configuration in PostgreSQL
   su ${DB_OWNER} -c \
      "${POSTGRESQL_INSTALL}/bin/pg_ctl reload -D /awips2/database/data > /dev/null 2>&1"
   if [ $? -ne 0 ]; then
      cleanup_pg_hba
      exit 1
   fi

   # Give PostgreSQL time to reload
   sleep 5
}

A2LIBS=true source /etc/profile.d/awips2PSQL.sh
if [ $? -ne 0 ]; then
   exit 1
fi

POSTGRESQL_INSTALL="/usr"
PSQL_INSTALL="/usr"

POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
PG_RESTORE="${POSTGRESQL_INSTALL}/bin/pg_restore"
PSQL="${PSQL_INSTALL}/bin/psql"

# Determine who owns the PostgreSQL Installation
DB_OWNER=$(stat -c %U /awips2/database/data)
# Our log file
SQL_LOG="/awips2/database/sqlScripts/share/sql/maps/maps.log"

if ! systemctl status postgresql@awips; then
    i_started_postgresql=1
    systemctl start postgresql@awips
else
    i_started_postgresql=
fi

grant_permissions

# Is there a maps database?
MAPS_DB_EXISTS="false"
MAPS_DB=`${PSQL} -U awipsadmin -l | grep maps | awk '{print $1}'`
if [ "${MAPS_DB}" = "maps" ]; then
   MAPS_DB_EXISTS="true"
   # We Have a Maps Database, There Is Nothing To Do.
fi

if [ "${MAPS_DB_EXISTS}" = "false" ]; then
   # Create the maps directory; remove any existing directories.
   if [ -d /awips2/database/tablespaces/maps ]; then
      su ${DB_OWNER} -c "rm -rf /awips2/database/tablespaces/maps"
   fi
   su ${DB_OWNER} -c "mkdir -p /awips2/database/tablespaces/maps"

   # Update the sql script that creates the maps database / tables.
   sed --in-place "s/%{tablespace_dir}%/\/awips2\/database\/tablespaces/g" \
      /awips2/database/sqlScripts/share/sql/maps/createMapsDb.sql

   # Run the setup sql for the maps database.
   SQL_FILE="/awips2/database/sqlScripts/share/sql/maps/createMapsDb.sql"
   su ${DB_OWNER} -c \
      "${PSQL} -d postgres -U awipsadmin -q -p 5432 -f ${SQL_FILE}" >> ${SQL_LOG} 2>&1
   RC=$?
   if [ ! "${RC}" -eq 0 ]; then
      printFailureMessage
   fi

   su ${DB_OWNER} -c \
      "${PSQL} -d maps -U awipsadmin -q -p 5432 -c \"CREATE EXTENSION if not exists postgis;\"" >> ${SQL_LOG} 2>&1
   if [ $? -ne 0 ]; then
      printFailureMessage
   fi

   su ${DB_OWNER} -c \
      "${PSQL} -d maps -U awipsadmin -q -p 5432 -c \"CREATE EXTENSION if not exists postgis_raster;\"" >> ${SQL_LOG} 2>&1
   if [ $? -ne 0 ]; then
      printFailureMessage
   fi

   su ${DB_OWNER} -c \
      "${PSQL} -d maps -U awipsadmin -q -p 5432 -c \"CREATE EXTENSION if not exists postgis_topology;\"" >> ${SQL_LOG} 2>&1
   if [ $? -ne 0 ]; then
      printFailureMessage
   fi


   # Import the data into the maps database.
   DB_ARCHIVE="/awips2/database/sqlScripts/share/sql/maps/maps.db"
   su ${DB_OWNER} -c \
      "${PG_RESTORE} -d maps -U awipsadmin -p 5432 -n mapdata ${DB_ARCHIVE}" >> ${SQL_LOG} 2>&1

   su ${DB_OWNER} -c \
      "${PG_RESTORE} -d maps -U awipsadmin -p 5432 -n public -t geometry_columns -a ${DB_ARCHIVE}" \
      >> ${SQL_LOG} 2>&1
fi

cleanup_pg_hba

if [[ "$i_started_postgresql" != "" ]]; then
    systemctl stop postgresql@awips
fi

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi

# We use exit 0 throughout the uninstall, making an assumption that if a step
# fails, the database has probably already been removed. This is to make sure
# a full uninstall of awips2 succeeds, otherwise removing individual databases
# would fail as awips2-database and awips2-postgresql may have already been
# uninstalled.

function cleanup_pg_hba()
{
   cp /tmp/pg_hba.conf.orig /awips2/database/data/pg_hba.conf
   if [ $? -ne 0 ]; then
      exit 0
   fi

   # trigger a reload of the configuration in PostgreSQL
   su ${DB_OWNER} -c \
      "${POSTGRESQL_INSTALL}/bin/pg_ctl reload -D /awips2/database/data > /dev/null 2>&1"
   if [ $? -ne 0 ]; then
      exit 0
   fi
   rm -f /tmp/pg_hba.conf.orig
   if [ $? -ne 0 ]; then
      exit 0
   fi
}

function grant_permissions()
{
   cp /awips2/database/data/pg_hba.conf /tmp/pg_hba.conf.orig
   if [ $? -ne 0 ]; then
      exit 0
   fi

   cat >> "/awips2/database/data/pg_hba.conf" <<EOF

# TYPE  DATABASE    USER        CIDR-ADDRESS          METHOD
# ===== Maps Configuration (Install) =====
local        all         all                               trust

EOF

   if [ $? -ne 0 ]; then
      cleanup_pg_hba
      exit 0
   fi

   # trigger a reload of the configuration in PostgreSQL
   su ${DB_OWNER} -c \
      "${POSTGRESQL_INSTALL}/bin/pg_ctl reload -D /awips2/database/data > /dev/null 2>&1"
   if [ $? -ne 0 ]; then
      cleanup_pg_hba
      exit 0
   fi

   # Give PostgreSQL time to reload
   sleep 5
}

A2LIBS=true source /etc/profile.d/awips2PSQL.sh
if [ $? -ne 0 ]; then
   exit 0
fi

POSTGRESQL_INSTALL="/usr"
PSQL_INSTALL="/usr"

POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
PG_RESTORE="${POSTGRESQL_INSTALL}/bin/pg_restore"
PSQL="${PSQL_INSTALL}/bin/psql"

if [ ! -f ${POSTMASTER} ]; then
   exit 0
fi
if [ ! -f ${DROPDB} ]; then
   exit 0
fi
if [ ! -f ${PG_RESTORE} ]; then
   exit 0
fi
if [ ! -f ${PSQL} ]; then
   exit 0
fi

# Determine who owns the PostgreSQL Installation
DB_OWNER=$(stat -c %U /awips2/database/data)
# Our log file
SQL_LOG="/awips2/database/sqlScripts/share/sql/maps/maps.log"

if ! systemctl status postgresql@awips; then
    i_started_postgresql=1
    systemctl start postgresql@awips
else
    i_started_postgresql=
fi

grant_permissions

# Is there a maps database?
MAPS_DB=`${PSQL} -U awipsadmin -l | grep maps | awk '{print $1}'`

if [ "${MAPS_DB}" = "maps" ]; then
   # drop the maps database
   su ${DB_OWNER} -c \
      "${DROPDB} -U awipsadmin maps" >> ${SQL_LOG}
fi

# Is there a maps tablespace?
# ask psql where the maps tablespace is ...
MAPS_DIR=`${PSQL} -U awipsadmin -d postgres -c "\db" | grep maps | awk '{print $5}'`

if [ ! "${MAPS_DIR}" = "" ]; then
   # drop the maps tablespace
   su ${DB_OWNER} -c \
      "${PSQL} -U awipsadmin -d postgres -c \"DROP TABLESPACE maps\"" >> ${SQL_LOG}

   # remove the maps data directory that we created
   echo "Attempting To Removing Directory: ${MAPS_DIR}"
   if [ -d "${MAPS_DIR}" ]; then
      su ${DB_OWNER} -c "rmdir ${MAPS_DIR}"
   fi
fi

cleanup_pg_hba

if [[ "$i_started_postgresql" != "" ]]; then
    systemctl stop postgresql@awips
fi

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(666,awips,fxalpha,775)
%dir /awips2/database/sqlScripts/share/sql/maps
/awips2/database/sqlScripts/share/sql/maps/maps.log

%defattr(755,awips,fxalpha,755)
/awips2/database/sqlScripts/share/sql/maps/*.sh
/awips2/database/sqlScripts/share/sql/maps/*.db
/awips2/database/sqlScripts/share/sql/maps/*.sql
