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
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-edex-environment
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
The edex environment version of awips2-edex consists of the edex runtime and
the AWIPS II edex-environment manager. The edex-environment manager exists to spawn
and configure additional instances of the AWIPS II processing triad {postgresql,
qpid, edex}. The edex environment version of awips2-edex is not designed to
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
# build edex
pushd . > /dev/null 2>&1
# Run the pde build.
cd %{_baseline_workspace}/build.edex
/bin/bash build.sh -eclipse=%{_uframe_eclipse}
if [ $? -ne 0 ]; then
   echo "ERROR: The pde build of edex has failed."
   exit 1
fi
popd > /dev/null 2>&1

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

# "install" edex
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

RPM_PROJECT="%{_baseline_workspace}/Installer.rpm"
POSTGRES_INITD="${RPM_PROJECT}/awips2.core/Installer.postgresql/scripts/init.d/edex_postgres"
QPID_INITD="${RPM_PROJECT}/awips2.qpid/SOURCES/qpidd"
QUEUE_SH="${RPM_PROJECT}/awips2.qpid/SOURCES/queueCreator.sh"
EDEX_INITD="${RPM_PROJECT}/awips2.edex/Installer.edex-base/scripts/init.d/edex_camel"

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

# Copy the edex-environment macro, functions, and utilities.
DELIVERABLES="%{_baseline_workspace}/Installer.rpm/awips2.edex-environment/wes2bridge.files/deliverables"

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
# We need to create a link to the python shared library if it does not exist.
pushd . > /dev/null 2>&1
if [ -d /awips2/python/lib ]; then
   cd /awips2/python/lib
   if [ -L libpython.so ]; then
      # Ensure that we are pointing to the correct shared library.
      rm -f libpython.so
   fi
      
   if [ -f libpython2.7.so.1.0 ]; then
      ln -s libpython2.7.so.1.0 libpython.so
   fi
fi
popd > /dev/null 2>&1

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

%dir /usr/local/edex-environment

%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*.sh
/awips2/edex/bin/linux-x86-32/wrapper
/awips2/edex/bin/wrapper.jar

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