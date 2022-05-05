#
# AWIPS II Database Standalone Configuration Spec File
#
Name: awips2-database-standalone-configuration
Summary: AWIPS II Database Standalone Configuration
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
Requires: awips2
Requires: awips2-postgresql
Provides: awips2-database-standalone-configuration
Provides: awips2-database-configuration

%description
AWIPS II Database Server Configuration - contains the AWIPS II server
configuration files optimized for a standalone environment.

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

# Create data directory and other required directories
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/{data,tablespaces,pg_log,ssl}
if [ $? -ne 0 ]; then
   exit 1
fi

PROJECT_DIR="Installer.database-standalone-configuration"
CONFIGURATION_DIR="rpms/awips2.core/${PROJECT_DIR}/configuration"
CONF_FILE="postgresql.conf"

cp %{_baseline_workspace}/${CONFIGURATION_DIR}/${CONF_FILE} \
   ${RPM_BUILD_ROOT}/awips2/database/data
cp -p %{_baseline_workspace}/${CONFIGURATION_DIR}/*.{key,crt} \
   ${RPM_BUILD_ROOT}/awips2/database/ssl
   
%pre
# Remove any existing postgresql.conf files
if [ -f /awips2/database/data/postgresql.conf ]; then
   rm -f /awips2/database/data/postgresql.conf
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755) 
%dir /awips2/database
%attr(700,awips,fxalpha) /awips2/database/data
%attr(700,awips,fxalpha) /awips2/database/tablespaces
%attr(750,awips,fxalpha) /awips2/database/pg_log
/awips2/database/data/postgresql.conf

%defattr(600,awips,fxalpha,700)
/awips2/database/ssl
%config(noreplace) /awips2/database/ssl/server.crt
%config(noreplace) /awips2/database/ssl/root.crt
%config(noreplace) /awips2/database/ssl/server.key

%changelog
* Thu Sep 10 2020 Ron Anderson <ron.anderson@raytheon.com> 
- Remove /awips2/data symlink
