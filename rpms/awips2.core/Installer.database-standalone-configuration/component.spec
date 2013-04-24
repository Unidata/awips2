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
Packager: Bryan Kowal

AutoReq: no
requires: awips2-postgresql
provides: awips2-database-standalone-configuration
provides: awips2-database-configuration

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

mkdir -p ${RPM_BUILD_ROOT}/awips2/data

%build

%install
PROJECT_DIR="Installer.database-standalone-configuration"
CONFIGURATION_DIR="rpms/awips2.core/${PROJECT_DIR}/configuration"
CONF_FILE="postgresql.conf"

cp %{_baseline_workspace}/${CONFIGURATION_DIR}/${CONF_FILE} \
   ${RPM_BUILD_ROOT}/awips2/data
   
%pre
# Remove any existing postgresql.conf files
if [ -f /awips2/data/postgresql.conf ]; then
   rm -f /awips2/data/postgresql.conf
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755) 
/awips2/data/postgresql.conf

%defattr(644,awips,fxalpha,700)
%dir /awips2
%dir /awips2/data