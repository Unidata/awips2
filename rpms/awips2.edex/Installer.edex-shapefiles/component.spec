#
# AWIPS II edex-shapefiles Spec File
#
Name: awips2-edex-shapefiles
Summary: AWIPS II Edex
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
provides: awips2-edex-shapefiles
requires: awips2
requires: awips2-edex-base

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

%description
AWIPS II Edex Shapefiles - includes the shapefiles required by AWIPS II.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "ERROR: The RPM Build Root has not been specified."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
mkdir -p %{_build_root}/awips2/edex/data/utility/edex_static/base/shapefiles

%build

%install
AWIPS2_STATIC=%{_awipscm_share}/awips2-static

# Determine which version of the shapefiles we should use.
COMMON=%{_baseline_workspace}/rpms/common
if [ ! -f ${COMMON}/static.versions/LATEST.maps ]; then
   file ${COMMON}/static.versions/LATEST.maps
   exit 1
fi

LATEST=`cat ${COMMON}/static.versions/LATEST.maps`
if [ $? -ne 0 ]; then
   exit 1
fi
SHAPEFILES=${AWIPS2_STATIC}/maps/${LATEST}/shapefiles
if [ ! -d ${SHAPEFILES} ]; then
   file ${SHAPEFILES}
   exit 1
fi

cp -r ${SHAPEFILES}/* \
   %{_build_root}/awips2/edex/data/utility/edex_static/base/shapefiles
if [ $? -ne 0 ]; then
   exit 1
fi

%pre

%post

%preun

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(775,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*
