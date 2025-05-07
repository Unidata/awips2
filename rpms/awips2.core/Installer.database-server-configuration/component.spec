%define _component_project_dir    awips2.core/Installer.database
#
# AWIPS II Database Server Configuration Spec File
#
Name: awips2-database-server-configuration
Summary: AWIPS II Database Server Configuration
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Requires: awips2
Requires: awips2-postgresql
Requires: awips2-watchdog
Provides: awips2-database-server-configuration
Provides: awips2-database-configuration

%description
AWIPS II Database Server Configuration - contains the AWIPS II server
configuration files optimized for a clustered, server environment.

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
mkdir -p ${RPM_BUILD_ROOT}/awips2/database/{data,tablespaces,pg_log,ssl} || exit 1
mkdir -p ${RPM_BUILD_ROOT}/etc/watchdog.d || exit 1

PROJECT_DIR="Installer.database-server-configuration"
CONFIGURATION_DIR="rpms/awips2.core/${PROJECT_DIR}/configuration"
WATCHDOG_DIR="rpms/awips2.core/${PROJECT_DIR}/watchdog.d"
CONF_FILE="postgresql.conf"

cp %{_baseline_workspace}/${CONFIGURATION_DIR}/${CONF_FILE} \
   ${RPM_BUILD_ROOT}/awips2/database/data
cp %{_baseline_workspace}/${CONFIGURATION_DIR}/${CONF_FILE}.ax \
   ${RPM_BUILD_ROOT}/awips2/database/data
cp %{_baseline_workspace}/${CONFIGURATION_DIR}/${CONF_FILE}.chps \
   ${RPM_BUILD_ROOT}/awips2/database/data
cp -p %{_baseline_workspace}/${CONFIGURATION_DIR}/*.{key,crt} \
   ${RPM_BUILD_ROOT}/awips2/database/ssl
# Include the watchdog test/repair script
cp %{_baseline_workspace}/${WATCHDOG_DIR}/postgres_watchdog.sh \
   ${RPM_BUILD_ROOT}/etc/watchdog.d

# Install systemd unit file
service_dir="${RPM_BUILD_ROOT}/etc/systemd/system/postgresql@awips.service.d"
mkdir --parents "${service_dir}"
cp "%{_baseline_workspace}/rpms/%{_component_project_dir}/30-postgresql-setup.conf" \
    "${service_dir}"

%pre
rm -f /awips2/database/data/postgresql.conf


case ${hostName} in
    ax* ) 
        rm -f /awips2/database/data/postgresql.conf
        install --owner=awips --group=fxalpha --mode=0644 --no-target-dir \
          /awips2/database/data/postgresql.conf.ax \
          /awips2/database/data/postgresql.conf
        ;;
    chps* )
        rm -f /awips2/database/data/postgresql.conf
        install --owner=awips --group=fxalpha --mode=0644 --no-target-dir \
          /awips2/database/data/postgresql.conf.chps \
          /awips2/database/data/postgresql.conf
        ;;    
    *)  ;;
esac


rm -f /awips2/database/data/postgresql.conf.ax
rm -f /awips2/database/data/postgresql.conf.chps
%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755) 
%dir /awips2/database
%attr(700,awips,fxalpha) /awips2/database/data
%attr(700,awips,fxalpha) /awips2/database/tablespaces
%attr(750,awips,fxalpha) /awips2/database/pg_log
/awips2/database/data/postgresql.conf
/awips2/database/data/postgresql.conf.ax
/awips2/database/data/postgresql.conf.chps

%attr(744,root,root) /etc/watchdog.d/postgres_watchdog.sh

%defattr(600,awips,fxalpha,700)
/awips2/database/ssl
%config(noreplace) /awips2/database/ssl/server.crt
%config(noreplace) /awips2/database/ssl/root.crt
%config(noreplace) /awips2/database/ssl/server.key

%defattr(644,root,root,755)
/etc/systemd/system/postgresql@awips.service.d/30-postgresql-setup.conf

%changelog
* Wed Nov 06 2024 John Sebahar <john.sebahar@rtx.com>
- Added systemd configuration file to installation
* Thu Aug 31 2023 Mark Peters <mark.a.peters@rtx.com>
- Remove central registry configuration
* Fri May 05 2023 Tom Gurney <thomas.gurney@rtx.com>
- Install postgresql.conf with awips:fxalpha ownership, not root:root
* Thu Aug 26 2021 Matt Richardson <matthew.richardson@raytheon.com>
- Added postgres watchdog script to installation
* Thu Sep 10 2020 Ron Anderson <ron.anderson@raytheon.com> 
- Remove /awips2/data symlink
