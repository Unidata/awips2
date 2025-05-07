%define _installation_directory "/awips2"
#
# awips2-edex-environment
#
Name: awips2-edex-environment
Summary: awips2-edex-environment
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: awips2-edex-environment
Requires: awips2-edex
Requires: awips2-edex-base
Requires: awips2-postgresql
Requires: awips2-qpid-broker-j
Requires: awips2-python
Requires: awips2-java
Requires: awips2-psql
Requires: awips2-edex-registry
Requires: awips2-database
Requires: awips2-database-configuration
Requires: awips2-httpd-pypies


BuildRequires: awips2-ant
BuildRequires: awips2-java

%description
The edex environment version of awips2-edex consists of
the AWIPS II edex-environment manager. The edex-environment manager exists to spawn
and configure additional instances of the AWIPS II processes: postgresql, qpid, pypies, and edex.
This utilizes the wes2bridge utilities.

# Disable byte-compiling of python and repacking of jar files.
%global _python_bytecompile_extra 0
%global __jar_repack 0

%prep
# Verify that a build root has been specified.
if [ "%{build_root}" = "" ]; then
   echo "ERROR: A 'BuildRoot' has not been specified."
   exit 1
fi

rm -rf %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

%build

%install
mkdir -p %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}%{_installation_directory}/edex-environment/macro/utilities
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}%{_installation_directory}/edex-environment/macro/functions
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

# create the edex environment directory.
mkdir -p %{_build_root}/usr/local/edex-environment

# Run the deployment to the specified directory: 
#	%{_build_root}%{_installation_directory}/edex

# "install" the wes2bridge utilities
cd %{_baseline_workspace}/com.raytheon.wes2bridge.common
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=%{_build_root}%{_installation_directory}/edex-environment/macro/utilities \
   -Declipse.directory=%{_uframe_target} \
   -Dbaseline.dir=%{_baseline_workspace}
if [ $? -ne 0 ]; then
   exit 1
fi
cd %{_baseline_workspace}/com.raytheon.wes2bridge.configuration
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=%{_build_root}%{_installation_directory}/edex-environment/macro/utilities \
   -Declipse.directory=%{_uframe_target} \
   -Dbaseline.dir=%{_baseline_workspace}
if [ $? -ne 0 ]; then
   exit 1
fi
cd %{_baseline_workspace}/com.raytheon.wes2bridge.datalink
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=%{_build_root}%{_installation_directory}/edex-environment/macro/utilities \
   -Declipse.directory=%{_uframe_target} \
   -Dbaseline.dir=%{_baseline_workspace}
if [ $? -ne 0 ]; then
   exit 1
fi
cd %{_baseline_workspace}/com.raytheon.wes2bridge.manager
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=%{_build_root}%{_installation_directory}/edex-environment/macro/utilities \
   -Declipse.directory=%{_uframe_target} \
   -Dbaseline.dir=%{_baseline_workspace}
if [ $? -ne 0 ]; then
   exit 1
fi

RPM_PROJECT="%{_baseline_workspace}/rpms"
QPID_SYSTEMD="%{_baseline_workspace}/installers/RPMs/qpid-broker-j/scripts/systemd/qpidd.service"
QPID_WATCHDOG="%{_baseline_workspace}/installers/RPMs/qpid-broker-j/scripts/watchdog.d/qpid_watchdog.sh"
EDEX_SYSTEMD="${RPM_PROJECT}/awips2.edex/Installer.edex/scripts/edex_camel@.service"
EDEX_TARGET="${RPM_PROJECT}/awips2.edex/Installer.edex/scripts/edex_camel.target"
EDEX_WATCHDOG="${RPM_PROJECT}/awips2.edex/Installer.edex/scripts/edex_camel_watchdog.sh"
HTTPD_SYSTEMD="%{_baseline_workspace}/installers/RPMs/httpd-pypies/configuration/systemd/httpd-pypies.service"
HTTPD_LOGGING_SYSTEMD="%{_baseline_workspace}/installers/RPMs/httpd-pypies/configuration/systemd/httpd-pypies-logging.service"
PYPIES_WATCHDOG="%{_baseline_workspace}/installers/RPMs/httpd-pypies/configuration/etc/watchdog.d/pypies_watchdog.sh"
POSTGRES_SYSTEMD="${RPM_PROJECT}/awips2.core/Installer.database/30-postgresql-setup.conf"
POSTGRES_WATCHDOG="${RPM_PROJECT}/awips2.core/Installer.database-server-configuration/watchdog.d/postgres_watchdog.sh"

# Copy the startup scripts.
cp ${QPID_SYSTEMD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${QPID_WATCHDOG} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${EDEX_SYSTEMD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${EDEX_TARGET} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${EDEX_WATCHDOG} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${HTTPD_SYSTEMD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${HTTPD_LOGGING_SYSTEMD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${PYPIES_WATCHDOG} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${POSTGRES_SYSTEMD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${POSTGRES_WATCHDOG} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi



# Copy the edex-environment macro, functions, and utilities.
DELIVERABLES="${RPM_PROJECT}/awips2.core/Installer.edex-environment/wes2bridge.files/deliverables"

# Macro and functions.
cp ${DELIVERABLES}/scripts/edex-environment \
   %{_build_root}%{_installation_directory}/edex-environment/macro
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${DELIVERABLES}/scripts/functions/*.sh \
   %{_build_root}%{_installation_directory}/edex-environment/macro/functions
if [ $? -ne 0 ]; then
   exit 1
fi

# The profile.d script.
cp ${DELIVERABLES}/profile.d/* \
   %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

%files
%defattr(644,awips,fxalpha,755)
%dir /usr/local/edex-environment

%defattr(644,root,root,755)
%dir /awips2/edex-environment
%dir /awips2/edex-environment/scripts
/awips2/edex-environment/scripts/*
%dir /awips2/edex-environment/macro
%dir /awips2/edex-environment/macro/utilities
/awips2/edex-environment/macro/utilities/*
%dir /awips2/edex-environment/macro/functions
/awips2/edex-environment/macro/functions/*
/etc/profile.d/awips2-edex-env.sh
/etc/profile.d/awips2-edex-env.csh

%defattr(700,root,root,755)
/awips2/edex-environment/macro/edex-environment
