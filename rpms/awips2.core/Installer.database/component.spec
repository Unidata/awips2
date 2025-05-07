%define _component_name           awips2-database
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
BuildArch: noarch
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: awips2-database
Provides: awips2-static-user

Requires: awips2
Requires: awips2-database-configuration
Requires: awips2-postgis
Requires: awips2-postgresql
Requires: awips2-psql
Requires: awips2-netcdf
Requires: awips2-netcdf-devel
Requires: net-tools
Requires: sudo


%description
AWIPS II Database Installation - Sets up the basic AWIPS II database, creating
the required tables and schemas and populating static tables as needed.

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

database_dir="${RPM_BUILD_ROOT}/awips2/database"
mkdir --parents "${database_dir}"

cp "%{_baseline_workspace}/installers/Linux/.global" "${database_dir}"

ddl_dir="%{_baseline_workspace}/build.edex/opt/db/ddl"
sql_dir="${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql"
mkdir --parents "$sql_dir"

# Install SQL scripts and setup files
for dir in 'migrated' 'setup' 'events' 'ebxml' 'climate'; do
   cp --recursive "${ddl_dir}/${dir}/"* "${sql_dir}"/
done

# Install replication scripts
replication_dir="${RPM_BUILD_ROOT}/awips2/database/replication"
mkdir --parents "${replication_dir}"
cp --recursive "%{_baseline_workspace}/build.edex/opt/db/replication/"* \
    "${replication_dir}"/


# Create our installation log file.
touch "${RPM_BUILD_ROOT}/awips2/database/sqlScripts/share/sql/sql_install.log"

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

/bin/systemctl daemon-reload
/bin/systemctl enable --quiet postgresql@awips

# Don't run when upgrading existing package
if [ "${1}" -ge "2" ]; then
   exit 0
fi

function printInRed() {
    echo -e "\e[1;31m${1}\e[m"
}

function printFailureMessageAndExit()
{
   logfile="/awips2/database/sqlScripts/share/sql/sql_install.log"
   printInRed "--------------------------------------------------------------------------------"
   printInRed "\| AWIPS II Database Installation - FAILED\e[m"
   printInRed "--------------------------------------------------------------------------------"
   printInRed "Check the installation log: $logfile"
   tail --lines 6 "$logfile"
   exit 1
}

bash /awips2/database/sqlScripts/share/sql/setup.sh || printFailureMessageAndExit

%preun
if [ ${1} = 0 ]; then
    # stop the service and disable the service script
    /bin/systemctl disable --now --quiet postgresql@awips
fi

%clean
rm --recursive --force ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
/awips2/database/.global
%dir /awips2/database/sqlScripts
%dir /awips2/database/replication
%dir /awips2/database/sqlScripts/share
/awips2/database/sqlScripts/share/sql/sql_install.log
/awips2/database/sqlScripts/share/sql/pg_hba.conf
/awips2/database/replication/README

%defattr(755,awips,fxalpha,755)
%dir /awips2/database/sqlScripts/share/sql
/awips2/database/sqlScripts/share/sql/*.sql
/awips2/database/sqlScripts/share/sql/*.sh
/awips2/database/replication/setup-standby.sh
/awips2/database/replication/create-replication-role.sh
/awips2/database/replication/replication-config.sh
