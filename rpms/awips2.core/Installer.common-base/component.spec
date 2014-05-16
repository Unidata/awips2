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
/bin/mkdir -p %{_build_root}
#/bin/mkdir %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

%build
_hybrid_target=buildHybrid

_build_xml=build.xml
BUILD_EDEX=%{_baseline_workspace}/edexOsgi/build.edex
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
BUILD_EDEX=%{_baseline_workspace}/edexOsgi/build.edex
EDEX_DIST=${BUILD_EDEX}/edex/dist

/usr/bin/unzip ${EDEX_DIST}/common-base.zip -d %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

RPMS_CORE=%{_baseline_workspace}/rpms/awips2.core
RPMS_COMMON_BASE=${RPMS_CORE}/Installer.common-base
SCRIPTS=${RPMS_COMMON_BASE}/scripts
cp -vf ${RPMS_COMMON_BASE}/scripts/* %{_build_root}/awips2/cave
if [ $? -ne 0 ]; then
   exit 1
fi

%pre
%post
# EDEX installed?

# when the plugins are for EDEX, we just leave
# them on the filesystem; no action required.
rpm -q awips2-edex > /dev/null 2>&1
if [ $? -ne 0 ]; then
   # hide the edex plugins
   pushd . > /dev/null 2>&1
   cd /awips2
   rm -rf .edex
   mv edex .edex
   popd > /dev/null 2>&1
fi

# CAVE installed?

# when the plugins are for CAVE, we need to
# use the p2 director to install from a repository.
rpm -q awips2-cave > /dev/null 2>&1
if [ $? -eq 0 ]; then
   /bin/bash /awips2/cave/installCAVECommon.sh
   rm -f /awips2/cave/installCAVECommon.sh   
else
   # hide the cave repository
   pushd . > /dev/null 2>&1
   cd /awips2
   rm -rf .cave
   mv cave .cave
   popd > /dev/null 2>&1
fi

%preun
if [ -d /awips2/.cave ]; then
   rm -rf /awips2/.cave
fi
if [ -d /awips2/.edex ]; then
   rm -rf /awips2/.edex
fi

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*

%dir /awips2/cave
/awips2/cave/*
%dir /awips2/cave/.repository
/awips2/cave/.repository/*
