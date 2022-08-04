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
#PATH_TO_SHP_FILES="awips2-static/db/ddl/ncep"

# Create A Temporary Directory For The SQL Scripts That The Database
# RPM Will Need.
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/ncep

# Copy the ncep sql scripts into the rpm.
cp -r %{_baseline_workspace}/${PATH_TO_DDL}/* \
   ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/ncep
   
# Copy the ncep shapefiles into the rpm.
#cp -r %{_awipscm_share}/${PATH_TO_SHP_FILES}/* \
#   ${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/ncep
   
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
POSTGRESQL_INSTALL="/awips2/postgresql"
DATABASE_INSTALL="/awips2/database"
AWIPS2_DATA_DIRECTORY="/awips2/database/data"
TABLESPACE_DIR="/awips2/database/tablespaces"
PSQL_INSTALL="/awips2/psql"

POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
PG_CTL="${POSTGRESQL_INSTALL}/bin/pg_ctl"
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
PG_RESTORE="${POSTGRESQL_INSTALL}/bin/pg_restore"
PSQL="${PSQL_INSTALL}/bin/psql"
# Determine who owns the PostgreSQL Installation
DB_OWNER=$(stat -c %U ${AWIPS2_DATA_DIRECTORY})
# Our log file
SQL_LOG="${DATABASE_INSTALL}/sqlScripts/share/sql/ncep/ncep_sql_install.log"
SQL_SHARE_DIR="${DATABASE_INSTALL}/sqlScripts/share/sql/ncep"

# Determine if PostgreSQL is running.
I_STARTED_POSTGRESQL="NO"
su ${DB_OWNER} -c \
   "${PG_CTL} status -D ${AWIPS2_DATA_DIRECTORY} > /dev/null 2>&1"
RC="$?"

# Start PostgreSQL if it is not running.
if [ ! "${RC}" = "0" ]; then
   echo "--------------------------------------------------------------------------------"
   echo "\| Starting PostgreSQL As User - ${DB_OWNER}..."
   echo "--------------------------------------------------------------------------------"
   su ${DB_OWNER} -c \
      "${POSTMASTER} -D ${AWIPS2_DATA_DIRECTORY} > /dev/null 2>&1 &"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo ""
      echo "--------------------------------------------------------------------------------"
      echo "\| Error - failed to start the PostgreSQL Server."
      echo "--------------------------------------------------------------------------------"
      printFailureMessage
   fi
   # Give PostgreSQL Time To Start.
   sleep 10
   I_STARTED_POSTGRESQL="YES"
else
   echo "--------------------------------------------------------------------------------"
   echo "\| Found Running PostgreSQL Server..."
   echo "--------------------------------------------------------------------------------"
   # Show The User.
   su ${DB_OWNER} -c \
      "${PG_CTL} status -D ${AWIPS2_DATA_DIRECTORY}"
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
perl -p -i -e "s/%{tablespace_dir}%/${TABLESPACE_DIR_ESCAPED}/g" \
   ${SQL_SHARE_DIR}/createNcepDb.sql
   

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
   "${PSQL} -d ncep -U awipsadmin -q -p 5432 -f /awips2/postgresql/share/contrib/postgis-2.4/legacy.sql" >> ${SQL_LOG} 2>&1

   
su ${DB_OWNER} -c \
   "${SQL_SHARE_DIR}/createNcepDb.sh ${PSQL_INSTALL} 5432 awipsadmin ${SQL_SHARE_DIR} ${SQL_LOG}"
su ${DB_OWNER} -c \
   "${SQL_SHARE_DIR}/initializeNcepDb.sh ${POSTGRESQL_INSTALL} awipsadmin 5432 ${SQL_SHARE_DIR} ${SQL_LOG}"

su ${DB_OWNER} -c \
   "${DATABASE_INSTALL}/sqlScripts/share/sql/alter_database_roles_and_permissions.sh ncep" >> ${SQL_LOG} 2>&1
   
# stop PostgreSQL if we started it.
if [ "${I_STARTED_POSTGRESQL}" = "YES" ]; then
   echo ""
   echo "--------------------------------------------------------------------------------"
   echo "\| Stopping PostgreSQL As User - ${DB_OWNER}..."
   echo "--------------------------------------------------------------------------------"
   su ${DB_OWNER} -c \
      "${PG_CTL} stop -D /awips2/database/data"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "Warning: Failed to shutdown PostgreSQL."
   fi
   sleep 10
fi
   
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II ncep Database Creation ~ SUCCESSFUL...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi

POSTGRESQL_INSTALL="/awips2/postgresql"
PSQL_INSTALL="/awips2/psql"

AWIPS2_DATA_DIRECTORY="/awips2/database/data"
TABLESPACE_DIR="/awips2/database/tablespaces"
POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
PG_CTL="${POSTGRESQL_INSTALL}/bin/pg_ctl"
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
PG_RESTORE="${POSTGRESQL_INSTALL}/bin/pg_restore"
PSQL="${PSQL_INSTALL}/bin/psql"
# Determine who owns the PostgreSQL Installation
DB_OWNER="$(stat -c %U ${AWIPS2_DATA_DIRECTORY})"

echo "--------------------------------------------------------------------------------"
echo "\| Preparing to drop the ncep tablespace and database..."
echo "--------------------------------------------------------------------------------"
# Drop the ncep database and tablespace.

# start PostgreSQL if it is not running
I_STARTED_POSTGRESQL="NO"
su ${DB_OWNER} -c \
   "${PG_CTL} status -D ${AWIPS2_DATA_DIRECTORY} > /dev/null 2>&1"
RC="$?"

# Start PostgreSQL if it is not running.
if [ ! "${RC}" = "0" ]; then
   echo "--------------------------------------------------------------------------------"
   echo "\| Starting PostgreSQL As User - ${DB_OWNER}..."
   echo "--------------------------------------------------------------------------------"
   su ${DB_OWNER} -c \
      "${POSTMASTER} -D ${AWIPS2_DATA_DIRECTORY} > /dev/null 2>&1 &"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "Failed To Start The PostgreSQL Server."
      exit 1
   fi
   # Give PostgreSQL Time To Start.
   sleep 10
   I_STARTED_POSTGRESQL="YES"
else
   echo "--------------------------------------------------------------------------------"
   echo "\| Found Running PostgreSQL Server..."
   echo "--------------------------------------------------------------------------------"
   # Show The User.
   su ${DB_OWNER} -c \
      "${PG_CTL} status -D ${AWIPS2_DATA_DIRECTORY}"
fi

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

# stop PostgreSQL if we started it.
if [ "${I_STARTED_POSTGRESQL}" = "YES" ]; then
   echo ""
   echo "--------------------------------------------------------------------------------"
   echo "\| Stopping PostgreSQL As User - ${DB_OWNER}..."
   echo "--------------------------------------------------------------------------------"
   su ${DB_OWNER} -c \
      "${PG_CTL} stop -D ${AWIPS2_DATA_DIRECTORY}"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "Warning: Failed to shutdown PostgreSQL."
   fi
   sleep 10
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II ncep Database Removal ~ SUCCESSFUL...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
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
/awips2/database/sqlScripts/share/sql/ncep/shapefiles
