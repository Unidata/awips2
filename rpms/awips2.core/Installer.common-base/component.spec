%define _build_arch %(uname -i)
%define _zip_file common-base.zip

%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

#
# AWIPS II CAVE/EDEX common base Spec File
#
Name: awips2-common-base
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: %{_build_arch}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-common-base
requires: awips2-base

%description
AWIPS II Common Base - Contains common plugins utilized by both EDEX and CAVE.

%prep
# Ensure that a "buildroot" has been specified.
if [ "%{_build_root}" = "" ]; then
   echo "ERROR: A BuildRoot has not been specified."
   echo "FATAL: Unable to Continue ... Terminating."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
/bin/mkdir %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

%build
_hybrid_target=buildHybrid

_build_xml=build.xml
BUILD_EDEX=%{_baseline_workspace}/build.edex
EDEX_DIST=${BUILD_EDEX}/edex/dist

_pde_build_arch=x86
if [ "%{_build_arch}" = "x86_64" ]; then
   _pde_build_arch=%{_build_arch}
fi

cd ${BUILD_EDEX}
/awips2/ant/bin/ant -f ${_build_xml} \
   -Dbuild.arch=${_pde_build_arch} \
   -Duframe.eclipse=%{_uframe_eclipse} ${_hybrid_target}
if [ $? -ne 0 ]; then
   exit 1
fi

%install
BUILD_EDEX=%{_baseline_workspace}/build.edex
EDEX_DIST=${BUILD_EDEX}/edex/dist

/usr/bin/unzip ${EDEX_DIST}/common-base.zip -d %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

%pre
%post
# Set all paths required by CAVE before installing.
export LD_LIBRARY_PATH=/awips2/java/lib:/awips2/python/lib:$LD_LIBRARY_PATH
export LD_PRELOAD=libpython.so
if [ -d /awips2/cave/lib ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib/lib_illusion:$LD_LIBRARY_PATH
fi
if [ -d /awips2/cave/lib64 ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib64/lib_illusion:$LD_LIBRARY_PATH
fi
# Need to use awips2-java to do this.
export PATH=/awips2/java/bin:/awips2/python/bin:${PATH}
export JAVA_HOME="/awips2/java/jre"

# Set the CAVE logfile location.
export LOGFILE_CAVE=/dev/null

# Use the eclipse p2 manager.
CAVE_EXE="/awips2/cave/cave"
NOSPLASH_ARG="-nosplash"
DIRECTOR_APP="-application org.eclipse.equinox.p2.director"
DESTINATION_ARG="-destination /awips2/cave"
INSTALL_ARG="-i com.raytheon.uf.common.base.feature.feature.group"
UNINSTALL_ARG="-u com.raytheon.uf.common.base.feature.feature.group"
REPO="-repository file:/awips2/cave/.repository/"

COMMON_CMD="${CAVE_EXE} ${NOSPLASH_ARG} ${DIRECTOR_APP} ${DESTINATION_ARG}"
INSTALL_CMD="${COMMON_CMD} ${INSTALL_ARG} ${REPO}"
UNINSTALL_CMD="${COMMON_CMD} ${UNINSTALL_ARG}"

# EDEX installed?

# when the plugins are for EDEX, we just leave
# them on the filesystem; no action required.
rpm -q awips2-edex > /dev/null 2>&1
if [ $? -ne 0 ]; then
   # remove the edex plugins
   rm -rf /awips2/edex
fi

# CAVE installed?

# when the plugins are for CAVE, we need to
# use the p2 director to install from a repository.
rpm -q awips2-cave > /dev/null 2>&1
if [ $? -eq 0 ]; then
   # Uninstall any existing components since the p2 director does not
   # support updating.
   # If the feature is not installed, this does not fail quietly.
   # Determine if the feature needs to be uninstalled.
   ${UNINSTALL_CMD} -verifyOnly > /dev/null 2>&1
   if [ $? -eq 0 ]; then
      LOG_TIMESTAMP=`date`
      echo "uninstall previous STARTED: ${LOG_TIMESTAMP}"
      ${UNINSTALL_CMD}
      LOG_TIMESTAMP=`date`
      echo "uninstall previous COMPLETE: ${LOG_TIMESTAMP}"
   fi
   
   # complete the install
   LOG_TIMESTAMP=`date`
   echo "installation STARTED: ${LOG_TIMESTAMP}"
   ${INSTALL_CMD}
   if [ $? -ne 0 ]; then
      exit 1
   fi
   LOG_TIMESTAMP=`date`
   echo "installation COMPLETE: ${LOG_TIMESTAMP}"
   
   # remove the repository
   if [ -f /awips2/cave/.repository/artifacts.xml ]; then
      rm -f /awips2/cave/.repository/artifacts.xml
   fi
   
   if [ -f /awips2/cave/.repository/content.xml ]; then
      rm -f /awips2/cave/.repository/content.xml
   fi
   
   if [ -d /awips2/cave/.repository/features ]; then
      rm -rf /awips2/cave/.repository/features
   fi
   
   if [ -d /awips2/cave/.repository/plugins ]; then
      rm -rf /awips2/cave/.repository/plugins
   fi   
else
   # remove the cave repository
   rm -rf /awips2/cave
fi

%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*

%dir /awips2/cave/.repository
/awips2/cave/.repository/*
