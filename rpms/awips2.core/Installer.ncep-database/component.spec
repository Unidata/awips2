#
# AWIPS II NCEP Database Spec File
#
# This rpm will create the ncep database and tables.

Name: awips2-ncep-database
Summary: AWIPS II NCEP Database Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
Prefix: /awips2/database
URL: N/A
License: N/A
Distribution: N/A
Vendor: NCEP/NCO/SIB CWA
Packager: %{_build_site}

AutoReq: no
Provides: awips2-ncep-database
Requires: awips2-database
Requires: awips2-postgresql
Requires: awips2-psql

%description
AWIPS II NCEP Database Installation - This rpm creates the ncep database and tables.

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

%install
PATH_TO_DDL="build.edex/opt/db/ddl/ncep"

# Create A Temporary Directory For The SQL Scripts That The Database
# RPM Will Need.
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/ncep

# Copy the ncep sql scripts into the rpm.
cp -r %{_baseline_workspace}/${PATH_TO_DDL}/* \
   ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/ncep

# Create our installation log file.
touch ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/ncep/ncep_sql_install.log

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Creating the AWIPS II ncep Database...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%post

if [ "${1}" = "2" ]; then
   exit 0
fi

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
# ===== NCEP Configuration (Install) =====
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
DATABASE_INSTALL="/awips2/database"
AWIPS2_DATA_DIRECTORY="/awips2/database/data"
TABLESPACE_DIR="/awips2/database/tablespaces"
PSQL_INSTALL="/awips2/psql"

POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
PG_RESTORE="${POSTGRESQL_INSTALL}/bin/pg_restore"
PSQL="${PSQL_INSTALL}/bin/psql"
# Determine who owns the PostgreSQL Installation
DB_OWNER=$(stat -c %U ${AWIPS2_DATA_DIRECTORY})
# Our log file
SQL_LOG="${DATABASE_INSTALL}/sqlScripts/share/sql/ncep/ncep_sql_install.log"
SQL_SHARE_DIR="${DATABASE_INSTALL}/sqlScripts/share/sql/ncep"

if ! systemctl status postgresql@awips; then
    i_started_postgresql=1
    systemctl start postgresql@awips
else
    i_started_postgresql=
fi

# Create the ncep directory; remove any existing directories.
echo "--------------------------------------------------------------------------------"
echo "\| Creating a Directory for the ncep Tablespace..."
echo "--------------------------------------------------------------------------------"
if [ -d "${TABLESPACE_DIR}/ncep" ]; then
   su ${DB_OWNER} -c "rm -rf ${TABLESPACE_DIR}/ncep"
fi
su ${DB_OWNER} -c "mkdir -p ${TABLESPACE_DIR}/ncep"

echo "--------------------------------------------------------------------------------"
echo "\| Creating the ncep database..."
echo "--------------------------------------------------------------------------------"
# Update createNcepDb.sql
echo ${TABLESPACE_DIR} | sed 's/\//\\\//g' > .awips2_escape.tmp
TABLESPACE_DIR_ESCAPED=`cat .awips2_escape.tmp`
rm -f .awips2_escape.tmp
sed --in-place "s/%{tablespace_dir}%/${TABLESPACE_DIR_ESCAPED}/g" \
   ${SQL_SHARE_DIR}/createNcepDb.sql

grant_permissions

su ${DB_OWNER} -c \
   "${PSQL} -d postgres -U awipsadmin -q -p 5432 -f ${SQL_SHARE_DIR}/createNcepDb.sql" \
   >> ${SQL_LOG} 2>&1
su ${DB_OWNER} -c \
   "${PSQL} -d postgres -U awipsadmin -q -p 5432 -f ${SQL_SHARE_DIR}/createNcepSchemas.sql" \
   >> ${SQL_LOG} 2>&1

su ${DB_OWNER} -c \
   "${PSQL} -d ncep -U awipsadmin -q -p 5432 -c \"CREATE EXTENSION postgis;\"" >> ${SQL_LOG} 2>&1
su ${DB_OWNER} -c \
   "${PSQL} -d ncep -U awipsadmin -q -p 5432 -c \"CREATE EXTENSION postgis_topology;\"" >> ${SQL_LOG} 2>&1



su ${DB_OWNER} -c \
   "${SQL_SHARE_DIR}/createNcepDb.sh ${PSQL_INSTALL} 5432 awipsadmin ${SQL_SHARE_DIR} ${SQL_LOG}"
su ${DB_OWNER} -c \
   "${SQL_SHARE_DIR}/initializeNcepDb.sh ${POSTGRESQL_INSTALL} awipsadmin 5432 ${SQL_SHARE_DIR} ${SQL_LOG}"

su ${DB_OWNER} -c \
   "${DATABASE_INSTALL}/sqlScripts/share/sql/alter_database_roles_and_permissions.sh ncep" >> ${SQL_LOG} 2>&1

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II ncep Database Creation ~ SUCCESSFUL...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

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
# ===== NCEP Configuration (Install) =====
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

AWIPS2_DATA_DIRECTORY="/awips2/database/data"
TABLESPACE_DIR="/awips2/database/tablespaces"
POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
PG_RESTORE="${POSTGRESQL_INSTALL}/bin/pg_restore"
PSQL="${PSQL_INSTALL}/bin/psql"
# Determine who owns the PostgreSQL Installation
DB_OWNER="$(stat -c %U ${AWIPS2_DATA_DIRECTORY})"

if ! systemctl status postgresql@awips; then
    i_started_postgresql=1
    systemctl start postgresql@awips
else
    i_started_postgresql=
fi

grant_permissions

echo "--------------------------------------------------------------------------------"
echo "\| Dropping ncep database..."
echo "--------------------------------------------------------------------------------"

su ${DB_OWNER} -c \
   "${DROPDB} -U awipsadmin ncep"

# Is there a ncep tablespace?
# ask psql where the ncep tablespace is ...
NCEP_DIR=`${PSQL} -U awipsadmin -d postgres -c "\db" | grep ncep | awk '{print $5}'`

if [ ! "${NCEP_DIR}" = "" ]; then
   echo "--------------------------------------------------------------------------------"
   echo "\| Dropping ncep tablespace..."
   echo "--------------------------------------------------------------------------------"
   su ${DB_OWNER} -c \
      "${PSQL} -U awipsadmin -d postgres -c \"DROP TABLESPACE ncep\""

   # remove the maps data directory that we created
   echo "Attempting To Remove Directory: ${NCEP_DIR}"
   if [ -d "${NCEP_DIR}" ]; then
      su ${DB_OWNER} -c "rmdir ${NCEP_DIR}"
   fi
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II ncep Database Removal ~ SUCCESSFUL...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

cleanup_pg_hba

if [[ "$i_started_postgresql" != "" ]]; then
    systemctl stop postgresql@awips
fi

exit 0

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,awips,fxalpha,-)
%dir /awips2/database/sqlScripts/share/sql/ncep
%attr(777,root,root) /awips2/database/sqlScripts/share/sql/ncep/ncep_sql_install.log
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/addNcepNwxAdminMessageGpTable.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepConfigTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepDb.sh
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepDb.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepNcgribTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepNwxTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepSatTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepSchemas.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/createNcepStnsTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/importNcepShapeFile.sh
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/initializeNcepDb.sh
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadAirepWaypnts.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadBuoys.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadCities.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadClimReg.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadCntyclst.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadCoastal.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadConfigClo.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadCoordPts.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadCountynam.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadCounty.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadCpcstns.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadDlwx.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadFfgzon.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadFirezones.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadGeog.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadGfsmos.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadIdft.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadInactive.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadIntlsig.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadIsland.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadLsfstns.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadMardel.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadMarinenames.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadMarine.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadMsfstns.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadMzcntys.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNcepNwx.sh
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNcepStns.sh
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNcSat.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNcStnsCountyclust.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNexrad.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNgmmos.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxAdminMessagesBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxAviationforecastsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxCPCProductsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxFlashFloodBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxHPCHeatIndexBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxHPCProductsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxMarineBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxMasterAndGuiProducts.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxMOSBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxNHCProductsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxObservedDataBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxPtfcstProductsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxPublicProductsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxReconCARCAHBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxSPCProductsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxTropicalPacificBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadNwxVolcanoProductsBulletinTables.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadPermclust.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadPirepNavaids.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadRiverbas.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadScdstn.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSfstns.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadShef_COOP1.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadShef_COOP2.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadShef_COOP3.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadShef_COOP4.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadShef_COOP.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadShef_master.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadShpexception.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSnap8.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSnap.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSnstns.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSnworld.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSpcwatch.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadState.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadStns_II90.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSystns.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadSyworld.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTafstn.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTcabkpt_island.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTcabkpt_land.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTcabkptlz.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTcabkpt_ovl.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTcabkpt.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTcabkpt_water.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTpc_countries.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadTpc_states.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadVcrdgrib1.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadVolcano_small.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadVolcano.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadVors.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadWfo.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadWrqpf.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadXrainsort.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadZones.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/addUgcMzbnd.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/fixMzbnds.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/loadClimodata.sql
%attr(755,awips,fxalpha) /awips2/database/sqlScripts/share/sql/ncep/setPgHost.sh
/awips2/database/sqlScripts/share/sql/ncep/shapefiles
