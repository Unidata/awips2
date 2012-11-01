%define _component_name           awips2-database
%define _component_project_dir    awips2.core/Installer.database
%define _component_default_prefix /awips2/database
#
# AWIPS II Database Spec File
#

Name: %{_component_name}
Summary: AWIPS II Database Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-database
provides: awips2-static-user
requires: awips2-postgresql
requires: awips2-psql
requires: awips2-database-configuration

%description
AWIPS II Database Installation - Sets Up The Basic AWIPS II Database, Creating The
Required Tables And Schemas And Populating Static Tables As Needed.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/database

%build

PATH_TO_DDL="build.edex/opt/db/ddl"

# Create A Temporary Directory For The SQL Scripts That The Database
# RPM Will Need.
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/sqlScripts
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql

CONFIG_FILE_TO_INCLUDE="pg_hba.conf"
EXPECTED_PATH_TO_CONFIG="${PATH_TO_DDL}/setup"
KNOWN_CONFIG_DESTINATION="awips2/database/sqlScripts/share/sql"
# Ensure That We Have Access To The Configuration Files Before Continuing.
if [ ! -f %{_baseline_workspace}/${EXPECTED_PATH_TO_CONFIG}/${CONFIG_FILE_TO_INCLUDE} ]; then
   echo "The ${CONFIG_FILE_TO_INCLUDE} PostgreSQL Configuration File Can Not Be Found."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

# Copy The Configuration File
cp -r %{_baseline_workspace}/${EXPECTED_PATH_TO_CONFIG}/${CONFIG_FILE_TO_INCLUDE} \
   ${RPM_BUILD_ROOT}/${KNOWN_CONFIG_DESTINATION} 

# Copy The SQL Scripts That The Database RPM Will Need To The
# Temporary Directory.
DIRS_TO_COPY=('damcat' 'hmdb' 'migrated' 'setup' 'SHEF' 'vtec' 'ebxml' 'events')
for dir in ${DIRS_TO_COPY[*]};
do
   cp -r %{_baseline_workspace}/${PATH_TO_DDL}/${dir}/* \
      ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql
done

# All Files To Exclude Are In share/sql/setup
FILES_TO_EXCLUDE=( 'lwpostgis.sql' 'spatial_ref_sys.sql' 'uninstalldb.sh' \
   'setup.sh' 'setup_developer.sh' 'setup_server.sh' 'postgresql.conf' )
PATH_TO_EXCLUDE_FILES=${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql
for file in ${FILES_TO_EXCLUDE[*]};
do
   if [ -f ${PATH_TO_EXCLUDE_FILES}/${file} ]; then
      rm -f ${PATH_TO_EXCLUDE_FILES}/${file}
   fi
done

# Copy Two Other Files To The Temporary Share Directory.
cp -r %{_baseline_workspace}/${PATH_TO_DDL}/setup/lwpostgis.sql \
   ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share
cp -r %{_baseline_workspace}/${PATH_TO_DDL}/setup/spatial_ref_sys.sql \
   ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share
   
# Create our installation log file.
touch ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/sql_install.log   

%install

%pre
# Verify that one of the official AWIPS II PostgreSQL configuration files exist.
if [ ! -f /awips2/data/postgresql.conf ]; then
   echo "ERROR: /awips2/data/postgresql.conf does not exist. However, "
   echo "       the AWIPS II PostgreSQL Configuration RPM is installed. "
   echo "       If you recently uninstalled awips2-database and purged "
   echo "       the /awips2/data directory, you will need to re-install "
   echo "       the AWIPS II PostgreSQL configuration rpm so that the "
   echo "       postgresql.conf file will be restored."
   exit 1
fi

if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Database Installation...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
POSTGRESQL_INSTALL="/awips2/postgresql"
PSQL_INSTALL="/awips2/psql"
AWIPS2_DATA_DIRECTORY="/awips2/data"
echo -e "\e[1;34m         Installation Root = ${RPM_INSTALL_PREFIX}\e[m"
echo -e "\e[1;34m   PostgreSQL Install Root = ${POSTGRESQL_INSTALL}\e[m"
echo -e "\e[1;34m         PSQL Install Root = ${PSQL_INSTALL}\e[m"
echo -e "\e[1;34m   AWIPS II Data Directory = ${AWIPS2_DATA_DIRECTORY}\e[m"

%post
# Get Important Directories Again. Even Exporting Them The First Time Still
# Will Not Make Them Available.
POSTGRESQL_INSTALL="/awips2/postgresql"
PSQL_INSTALL="/awips2/psql"
AWIPS2_DATA_DIRECTORY="/awips2/data"

SHARE_DIR=${RPM_INSTALL_PREFIX}/sqlScripts/share
SQL_SHARE_DIR=${SHARE_DIR}/sql
if [ "${1}" = "2" ]; then   
   exit 0
fi

function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II Database Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m Check the installation log: /awips2/database/sqlScripts/share/sql/sql_install.log\e[m"
   echo ""
   if [ -f /awips2/database/sqlScripts/share/sql/sql_install.log ]; then
      tail -n 6 /awips2/database/sqlScripts/share/sql/sql_install.log
   fi
   
   exit 1
}
echo "--------------------------------------------------------------------------------"
echo "\| Preparing the SQL Scripts..."
echo "--------------------------------------------------------------------------------"

AWIPS_DEFAULT_OWNER="awips"
AWIPS_DEFAULT_USER="awips"
AWIPS_DEFAULT_GROUP="fxalpha"
AWIPS_DEFAULT_PORT="5432"

# This Is The Log File That We Will Use To Log All SQL Interactions.
SQL_LOG=${SQL_SHARE_DIR}/sql_install.log

# SQL Data Directories
METADATA=${AWIPS2_DATA_DIRECTORY}/metadata
IFHS=${AWIPS2_DATA_DIRECTORY}/pgdata_ihfs
MAPS=${AWIPS2_DATA_DIRECTORY}/maps
DAMCAT=${AWIPS2_DATA_DIRECTORY}/damcat
HMDB=${AWIPS2_DATA_DIRECTORY}/hmdb
EBXML=${AWIPS2_DATA_DIRECTORY}/ebxml

# Add The PostgreSQL Libraries And The PSQL Libraries To LD_LIBRARY_PATH.
export LD_LIBRARY_PATH=${POSTGRESQL_INSTALL}/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${PSQL_INSTALL}/lib:$LD_LIBRARY_PATH

function is_postgresql_running()
{
   NUM_PGSQL_COUNT=`/bin/netstat -l | /bin/grep -c "PGSQL.${AWIPS_DEFAULT_PORT}"`
   if [ ${NUM_PGSQL_COUNT} -gt 0 ]; then
      echo "--------------------------------------------------------------------------------"
      echo "\| Warning - PostgreSQL Is Already Running On Port ${AWIPS_DEFAULT_PORT}."
      echo "--------------------------------------------------------------------------------"
      printFailureMessage   
   fi

   echo "--------------------------------------------------------------------------------"
   echo "\| No Instances of PostgreSQL Were Found On Port ${AWIPS_DEFAULT_PORT}..."
   echo "--------------------------------------------------------------------------------"
}

function create_sql_element()
{
   # $1 == element
   
   mkdir -p ${1}
   update_owner ${1}
}

function update_owner()
{
   # $1 == element
   chown ${AWIPS_DEFAULT_OWNER} ${1}
   chgrp ${AWIPS_DEFAULT_GROUP} ${1}
}

function init_db()
{
   # move postgresql.conf in /awips2/data to a temporary location.
   if [ -f /awips2/data/postgresql.conf ]; then
      mv /awips2/data/postgresql.conf /awips2/
   fi
   
   su ${AWIPS_DEFAULT_USER} -c \
      "${POSTGRESQL_INSTALL}/bin/initdb --auth=trust --locale=en_US.UTF-8 --pgdata=${AWIPS2_DATA_DIRECTORY} --lc-collate=en_US.UTF-8 --lc-ctype=en_US.UTF-8"
   RC=$?   
      
   if [ -f /awips2/postgresql.conf ]; then
      mv /awips2/postgresql.conf /awips2/data
   fi
   
   return ${RC}
}

function control_pg_ctl()
{
   # $1 == pg_ctl command
   su ${AWIPS_DEFAULT_USER} -c \
      "${POSTGRESQL_INSTALL}/bin/pg_ctl ${1} -D ${AWIPS2_DATA_DIRECTORY} -o \"-p ${AWIPS_DEFAULT_PORT}\" -w"
   
}

function execute_initial_sql_script()
{
   # Make The Necessary Replacements In The Script.
   perl -p -i -e "s/%{databaseUsername}/${AWIPS_DEFAULT_OWNER}/g" \
      ${1}
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${1}

   # $1 == script to execute
   su ${AWIPS_DEFAULT_USER} -c \
      "${PSQL_INSTALL}/bin/psql postgres -U ${AWIPS_DEFAULT_USER} -q -p ${AWIPS_DEFAULT_PORT} -f ${1}" \
      > ${SQL_LOG} 2>&1
}

function update_lwpostgis()
{
   echo ${POSTGRESQL_INSTALL} | sed 's/\//\\\//g' > .awips2_escape.tmp
   POSTGRESQL_INSTALL_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{INSTALL_PATH}%/${POSTGRESQL_INSTALL_ESCAPED}/g" \
      ${SHARE_DIR}/lwpostgis.sql
}

function update_createMapsDb()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_SHARE_DIR}/createMapsDb.sql
}

function update_createDamcat()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_SHARE_DIR}/createDamcat.sql
}

function update_createEbxml()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_SHARE_DIR}/createEbxml.sql
}

function update_createHMDB()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_SHARE_DIR}/createHMDB.sql
}

function execute_psql_sql_script()
{
   # $1 == script to execute
   # $2 == database
   
   su ${AWIPS_DEFAULT_USER} -c \
      "${PSQL_INSTALL}/bin/psql -d ${2} -U ${AWIPS_DEFAULT_USER} -q -p ${AWIPS_DEFAULT_PORT} -f ${1}" \
      >> ${SQL_LOG} 2>&1
}

function copy_addl_config()
{
   rm -f ${AWIPS2_DATA_DIRECTORY}/pg_hba.conf
   cp ${SQL_SHARE_DIR}/pg_hba.conf ${AWIPS2_DATA_DIRECTORY}/pg_hba.conf
   update_owner ${AWIPS2_DATA_DIRECTORY}/pg_hba.conf
}

echo "--------------------------------------------------------------------------------"
echo "\| Determining If PostgreSQL Is Running..."
echo "--------------------------------------------------------------------------------"
is_postgresql_running
echo ""
 
echo "--------------------------------------------------------------------------------"
echo "\| Initializing the Database and Starting the Service..."
echo "--------------------------------------------------------------------------------"
init_db
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "--------------------------------------------------------------------------------"
   echo "\| ERROR - INITDB HAS FAILED."
   echo "--------------------------------------------------------------------------------"
   printFailureMessage 
fi
echo "--------------------------------------------------------------------------------"
echo "\| Copying Configuration Files and Setting Up SQL..."
echo "--------------------------------------------------------------------------------"
	
echo "--------------------------------------------------------------------------------"
echo "\| Creating a Directory for the metadata Tablespace..."
echo "--------------------------------------------------------------------------------"
create_sql_element ${METADATA}
echo "--------------------------------------------------------------------------------"
echo "\| Creating a Directory for the ihfs Tablespace..."
echo "--------------------------------------------------------------------------------"
create_sql_element ${IFHS}
echo "--------------------------------------------------------------------------------"
echo "\| Creating a Directory for the damcat Tablespace..."
echo "--------------------------------------------------------------------------------"
create_sql_element ${DAMCAT}
echo "--------------------------------------------------------------------------------"
echo "\| Creating a Directory for the hmdb Tablespace..."
echo "--------------------------------------------------------------------------------"
create_sql_element ${HMDB}
echo "--------------------------------------------------------------------------------"
echo "\| Creating a Directory for the ebxml Tablespace..."
echo "--------------------------------------------------------------------------------"
create_sql_element ${EBXML}
echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Starting PostgreSQL..."
echo "--------------------------------------------------------------------------------"
control_pg_ctl "start"
RC="$?"
# Ensure that the database started.
if [ ! "${RC}" = "0" ]; then
   echo "--------------------------------------------------------------------------------"
   echo "\| ERROR - UNABLE TO START THE POSTGRESQL SERVER."
   echo "--------------------------------------------------------------------------------"
   printFailureMessage   
fi
echo ""

echo "--------------------------------------------------------------------------------"
echo "\| Run the Initial SQL Script..."
echo "--------------------------------------------------------------------------------"
execute_initial_sql_script ${SQL_SHARE_DIR}/initial_setup_server.sql

update_lwpostgis
echo "--------------------------------------------------------------------------------"
echo "\| Spatially Enabling the metadata Database..."
echo "--------------------------------------------------------------------------------"
execute_psql_sql_script ${SHARE_DIR}/lwpostgis.sql metadata
execute_psql_sql_script ${SHARE_DIR}/spatial_ref_sys.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/permissions.sql metadata
echo "--------------------------------------------------------------------------------"
echo "\| Creating station Table..."
echo "--------------------------------------------------------------------------------"
execute_psql_sql_script ${SQL_SHARE_DIR}/create_subscription_tables.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/fxatext.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/afoslookup.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/bit_table.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/collective.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/national_category.sql metadata

# create the events schema
execute_psql_sql_script ${SQL_SHARE_DIR}/createEventsSchema.sql metadata

echo "--------------------------------------------------------------------------------"
echo "\| Creating shef Tables..."
echo "--------------------------------------------------------------------------------"
execute_psql_sql_script ${SQL_SHARE_DIR}/hd_ob83oax.sql metadata

update_createDamcat
echo "--------------------------------------------------------------------------------"
echo "\| Creating damcat Tables..."
echo "--------------------------------------------------------------------------------"
execute_psql_sql_script ${SQL_SHARE_DIR}/createDamcat.sql postgres
execute_psql_sql_script ${SQL_SHARE_DIR}/dcob7oax.sql dc_ob7oax
execute_psql_sql_script ${SQL_SHARE_DIR}/populateDamcatDatabase.sql dc_ob7oax

update_createHMDB
su ${AWIPS_DEFAULT_USER} -c \
   "${SQL_SHARE_DIR}/createHMDB.sh ${PSQL_INSTALL} ${AWIPS_DEFAULT_PORT} ${AWIPS_DEFAULT_USER} ${SQL_SHARE_DIR} ${SQL_LOG}"
   
update_createEbxml
echo "--------------------------------------------------------------------------------"
echo "\| Creating ebxml Tables..."
echo "--------------------------------------------------------------------------------"
execute_psql_sql_script ${SQL_SHARE_DIR}/createEbxml.sql postgres
echo "--------------------------------------------------------------------------------"
echo "\| Creating VTEC Tables..."
echo "--------------------------------------------------------------------------------"
execute_psql_sql_script ${SQL_SHARE_DIR}/vtec_initial_setup.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/create_p_vtec_tables.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/populate_p_vtec_tables.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/populate_vtec_events_table.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/populate_vtec_afos_product_table.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/populate_event_product_index.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/populate_vtec_event_tracking_table.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/create_h_vtec_tables.sql metadata
execute_psql_sql_script ${SQL_SHARE_DIR}/populate_h_vtec_tables.sql metadata

echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Stopping PostgreSQL..."
echo "--------------------------------------------------------------------------------"
control_pg_ctl "stop"

echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Copy Additional Configuration Files..."
echo "--------------------------------------------------------------------------------"
copy_addl_config

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Database Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi

# Ensure that our data directory exists.
if [ ! -d /awips2/data ]; then
   exit 0
fi

# We Need PostgreSQL  

POSTGRESQL_INSTALL="/awips2/postgresql"
PSQL_INSTALL="/awips2/psql"

POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
if [ ! -f ${POSTMASTER} ]; then
   exit 0
fi
PG_CTL="${POSTGRESQL_INSTALL}/bin/pg_ctl"
if [ ! -f ${PG_CTL} ]; then
   exit 0
fi
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
if [ ! -f ${DROPDB} ]; then
   exit 0
fi
PG_RESTORE="${POSTGRESQL_INSTALL}/bin/pg_restore"
if [ ! -f ${PG_RESTORE} ]; then
   exit 0
fi
PSQL="${PSQL_INSTALL}/bin/psql"
if [ ! -f ${PSQL} ]; then
   exit 0
fi
# Determine who owns the PostgreSQL Installation
DB_OWNER=`ls -l /awips2/ | grep -w 'data' | awk '{print $3}'`
# Our log file
SQL_LOG="${RPM_INSTALL_PREFIX}/static/database.maps/maps.log"

# Determine if PostgreSQL is running.
I_STARTED_POSTGRESQL="N"
su ${DB_OWNER} -c \
   "${PG_CTL} status -D /awips2/data > /dev/null 2>&1"
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "--------------------------------------------------------------------------------"
   echo "\| Starting PostgreSQL As User - ${DB_OWNER}..."
   echo "--------------------------------------------------------------------------------"
   su ${DB_OWNER} -c \
      "${POSTMASTER} -D /awips2/data > /dev/null 2>&1 &"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "Failed To Start The PostgreSQL Server."
      exit 1
   fi
   # Give PostgreSQL Time To Start.
   sleep 10
   I_STARTED_POSTGRESQL="Y"
   echo ""
fi

# Drop Databases The Official PostgreSQL Way. The User Is Responsible
# For Cleaning Up The Data Directory That Was Given To: 'initdb'.
DBS_TO_DROP=( 'dc_ob7oax' 'fxatext' 'hd_ob83oax' 'hmdb' 'metadata' )
TBS_TO_DROP=( 'damcat' 'hmdb' 'metadata' 'pgdata_ihfs' )

# Attempt To Drop The Databases In Our List ...
for db in ${DBS_TO_DROP[*]};
do
   echo "Dropping Database ... ${db}"
   su ${DB_OWNER} -c \
      "${DROPDB} -U awips ${db}"
done

echo ""
# Attempt To Drop The Tablespaces In Our List ...
for tb in ${TBS_TO_DROP[*]};
do
   echo "Dropping Tablespace ... ${tb}"
   TB_DIR=`${PSQL} -U awips -d postgres -c "\db" | grep ${tb} | awk '{print $5}'`
   su ${DB_OWNER} -c \
      "${PSQL} -U awips -d postgres -c \"DROP TABLESPACE ${tb}\""
      
   # remove the tablespace directory.
   echo "Attempting To Remove Directory: ${TB_DIR}"
   if [ -d "${TB_DIR}" ]; then
      su ${DB_OWNER} -c "rmdir ${TB_DIR}"
   fi
done

# Attempt To Stop PostgreSQL (If We Started It)
if [ "${I_STARTED_POSTGRESQL}" = "Y" ]; then
   echo ""
   su ${DB_OWNER} -c \
      "${PG_CTL} stop -D /awips2/data"   
fi

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Database Installation Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/database
%dir /awips2/database/sqlScripts
%dir /awips2/database/sqlScripts/share
/awips2/database/sqlScripts/share/sql/sql_install.log
/awips2/database/sqlScripts/share/sql/pg_hba.conf

%defattr(755,awips,fxalpha,755)
/awips2/database/sqlScripts/share/*.sql
%dir /awips2/database/sqlScripts/share/sql
/awips2/database/sqlScripts/share/sql/*.sql
/awips2/database/sqlScripts/share/sql/*.sh