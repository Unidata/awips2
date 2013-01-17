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
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-edex-environment
requires: awips2-edex-base
requires: awips2-postgresql
requires: awips2-qpid-server-store
requires: awips2-qpid-client
requires: awips2-qpid-server
requires: awips2-python
requires: awips2-java
requires: awips2-psql

%description
The edex environment version of awips2-edex consists of
the AWIPS II edex-environment manager. The edex-environment manager exists to spawn
and configure additional instances of the AWIPS II processing triad {postgresql,
qpid, edex}.

# Disable byte-compiling of python and repacking of jar files.
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

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

%build

# build the edex-environment utilities
pushd . > /dev/null 2>&1
# Run the pde build.
cd %{_baseline_workspace}/build.wes2bridge.utility
/awips2/ant/bin/ant -f build.xml \
   -Declipse.dir=%{_uframe_eclipse}
if [ $? -ne 0 ]; then
   echo "ERROR: The pde build of the wes2bridge utilities has failed."
   exit 1
fi
popd > /dev/null 2>&1

%install
# Run the deployment to the specified directory: 
#	%{_build_root}%{_installation_directory}/edex

# "install" the wes2bridge utilities
cd %{_baseline_workspace}/com.raytheon.wes2bridge.configuration
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=%{_build_root}%{_installation_directory}/edex-environment/macro/utilities \
   -Declipse.directory=%{_uframe_eclipse} \
   -Drpm.build=true
if [ $? -ne 0 ]; then
   exit 1
fi
cd %{_baseline_workspace}/com.raytheon.wes2bridge.datalink
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=%{_build_root}%{_installation_directory}/edex-environment/macro/utilities \
   -Declipse.directory=%{_uframe_eclipse} \
   -Drpm.build=true
if [ $? -ne 0 ]; then
   exit 1
fi
cd %{_baseline_workspace}/com.raytheon.wes2bridge.manager
/awips2/ant/bin/ant -f build.xml \
   -Ddestination.directory=%{_build_root}%{_installation_directory}/edex-environment/macro/utilities \
   -Declipse.directory=%{_uframe_eclipse} \
   -Drpm.build=true
if [ $? -ne 0 ]; then
   exit 1
fi

RPM_PROJECT="%{_baseline_workspace}/rpms"
POSTGRES_INITD="${RPM_PROJECT}/awips2.core/Installer.postgresql/scripts/init.d/edex_postgres"
QPID_INITD="${RPM_PROJECT}/awips2.qpid/SOURCES/qpidd"
QUEUE_SH="${RPM_PROJECT}/awips2.qpid/SOURCES/queueCreator.sh"
EDEX_INITD="${RPM_PROJECT}/awips2.edex/Installer.edex-base/scripts/init.d/edex_camel"
HTTPD_PYPIES_INITD="${RPM_PROJECT}/awips2.core/Installer.httpd-pypies/configuration/etc/init.d/httpd-pypies"

# Copy the startup scripts.
cp ${POSTGRES_INITD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${QPID_INITD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${QUEUE_SH} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${EDEX_INITD} \
   %{_build_root}%{_installation_directory}/edex-environment/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${HTTPD_PYPIES_INITD} \
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

%pre
%post
%preun
%postun

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2

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