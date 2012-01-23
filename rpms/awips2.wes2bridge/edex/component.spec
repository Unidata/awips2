%define _installation_directory "/awips2"
#
# awips2-wes2bridge-edex
#
Name: awips2-wes2bridge-edex
Summary: awips2-wes2bridge-edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-wes2bridge-edex
provides: awips2-edex-base
provides: awips2-base-component
provides: awips2-edex-gfe
requires: awips2-postgresql
requires: awips2-qpid-server-store
requires: awips2-qpid-client
requires: awips2-qpid-server
requires: awips2-python
requires: awips2-java
requires: awips2-psql

conflicts: awips2-edex-satellite
conflicts: awips2-edex-cots
conflicts: awips2-edex-radar
conflicts: awips2-edex-dataplugins
conflicts: awips2-edex-text
conflicts: awips2-edex-bufr
conflicts: awips2-edex-npp
conflicts: awips2-edex-grib
conflicts: awips2-edex-dat
conflicts: awips2-edex-common-core
conflicts: awips2-edex-configuration
conflicts: awips2-edex-hydro
conflicts: awips2-edex-ncep
conflicts: awips2-edex-core

%description
The wes2bridge version of awips2-edex consists of the edex runtime and
the AWIPS II wes2bridge manager. The wes2bridge manager exists to spawn
and configure additional instances of the wes2bridge triad {postgresql,
qpid, edex}. The wes2bridge version of awips2-edex is not designed to
be run in the same way that the standard awips2-edex is.

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

# Verify that the required scripts exist.
# build.sh
if [ ! -f %{_baseline_workspace}/build.edex/build.sh ]; then
   echo "ERROR: Unable to locate the edex pde build script."
   exit 1
fi

# deploy-install.sh
if [ ! -f %{_baseline_workspace}/build.edex/deploy-install.xml ]; then
   echo "ERROR: Unable to locate the edex deploy-install script."
   exit 1
fi

# ensure that ant is in the path.
which ant > /dev/null 2>&1
if [ $? -ne 0 ]; then
   which ant
   exit 1
fi

mkdir -p %{_build_root}%{_installation_directory}/edex
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}%{_installation_directory}/wes2bridge/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}%{_installation_directory}/wes2bridge/macro/utilities
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}%{_installation_directory}/wes2bridge/macro/functions
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

# create the edex environment directory.
mkdir -p %{_build_root}/usr/local/wes2bridge

%build
pushd . > /dev/null 2>&1
# Run the pde build.
cd %{_baseline_workspace}/build.edex
/bin/bash build.sh -eclipse=%{_uframe_eclipse}
if [ $? -ne 0 ]; then
   echo "ERROR: The pde build of edex has failed."
   exit 1
fi
popd > /dev/null 2>&1

%install
# Run the deployment to the specified: 
#	%{_build_root}%{_installation_directory}/edex

pushd . > /dev/null 2>&1
cd %{_baseline_workspace}/build.edex
ant -f deploy-install.xml \
	-Dinstall.dir=%{_build_root}%{_installation_directory}/edex \
	-Dinstaller=true
if [ $? -ne 0 ]; then
   echo "ERROR: the deploy-install of edex has failed."
   exit 1
fi
popd > /dev/null 2>&1

RPM_PROJECT="%{_baseline_workspace}/Installer.rpm"
POSTGRES_INITD="${RPM_PROJECT}/awips2.core/Installer.postgresql/scripts/init.d/edex_postgres"
QPID_INITD="${RPM_PROJECT}/awips2.qpid/SOURCES/qpidd"
QUEUE_SH="${RPM_PROJECT}/awips2.qpid/SOURCES/queueCreator.sh"
EDEX_INITD="${RPM_PROJECT}/awips2.edex/Installer.edex-base/scripts/init.d/edex_camel"

# Copy the startup scripts.
cp ${POSTGRES_INITD} \
   %{_build_root}%{_installation_directory}/wes2bridge/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${QPID_INITD} \
   %{_build_root}%{_installation_directory}/wes2bridge/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${QUEUE_SH} \
   %{_build_root}%{_installation_directory}/wes2bridge/scripts
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${EDEX_INITD} \
   %{_build_root}%{_installation_directory}/wes2bridge/scripts
if [ $? -ne 0 ]; then
   exit 1
fi

# Copy the wes2bridge macro, functions, and utilities.
DELIVERABLES="%{_baseline_workspace}/Installer.rpm/awips2.wes2bridge/wes2bridge.files/deliverables"

# Utilities
cp ${DELIVERABLES}/utility/*.jar \
   %{_build_root}%{_installation_directory}/wes2bridge/macro/utilities
if [ $? -ne 0 ]; then
   exit 1
fi

# Macro and functions.
cp ${DELIVERABLES}/scripts/wes2bridge \
   %{_build_root}%{_installation_directory}/wes2bridge/macro
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${DELIVERABLES}/scripts/functions/*.sh \
   %{_build_root}%{_installation_directory}/wes2bridge/macro/functions
if [ $? -ne 0 ]; then
   exit 1
fi

# The profile.d script.
cp ${DELIVERABLES}/profile.d/* \
   %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

# Temporary? Remove the edex management plugin.
JAR_FILE="com.raytheon.uf.edex.management.jar"
if [ -f %{_build_root}%{_installation_directory}/edex/lib/plugins/${JAR_FILE} ]; then
   rm -f %{_build_root}%{_installation_directory}/edex/lib/plugins/${JAR_FILE}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

%pre
%post
%preun
%postun

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/conf
/awips2/edex/conf/*
%dir /awips2/edex/data
/awips2/edex/data/*
%dir /awips2/edex/etc
/awips2/edex/etc/*
%dir /awips2/edex/lib
/awips2/edex/lib/*
%dir /awips2/edex/logs
%dir /awips2/edex/webapps
/awips2/edex/webapps/*
%dir /awips2/edex/bin/linux-x86-32
/awips2/edex/bin/linux-x86-32/*.so
/awips2/edex/bin/linux-x86-32/*.conf
/awips2/edex/bin/setup.env

%dir /usr/local/wes2bridge

%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*.sh
/awips2/edex/bin/linux-x86-32/wrapper
/awips2/edex/bin/wrapper.jar

%defattr(644,root,root,755)
%dir /awips2/wes2bridge
%dir /awips2/wes2bridge/scripts
/awips2/wes2bridge/scripts/*
%dir /awips2/wes2bridge/macro
%dir /awips2/wes2bridge/macro/utilities
/awips2/wes2bridge/macro/utilities/*
%dir /awips2/wes2bridge/macro/functions
/awips2/wes2bridge/macro/functions/*
/etc/profile.d/awips2-wes2bridge-env.sh
/etc/profile.d/awips2-wes2bridge-env.csh

%defattr(700,root,root,755)
/awips2/wes2bridge/macro/wes2bridge