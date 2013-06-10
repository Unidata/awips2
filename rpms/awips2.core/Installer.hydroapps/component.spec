%define _component_name           awips2-hydroapps-shared
%define _component_project_dir    awips2.core/Installer.hydroapps
#
# AWIPS II Hydroapps Spec File
#
Name: %{_component_name}
Summary: AWIPS II Hydroapps Distribution
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-hydroapps-shared
obsoletes: awips2-hydroapps
requires: awips2-edex-base
requires: awips2-edex-native

%description
AWIPS II Hydroapps Distribution - Includes applications, configurations, and
filesystems for Hydro.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data

%build

%install
FILES_NATIVE="%{_baseline_workspace}/files.native"

/bin/cp -rf ${FILES_NATIVE}/awipsShare \
   %{_build_root}/awips2/edex/data/share
if [ $? -ne 0 ]; then
   exit 1
fi

/usr/bin/find %{_build_root}/awips2/edex/data/share -name .gitignore -exec rm -rf {} \;
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
%defattr(644,awips,fxalpha,775)
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/data
%dir /awips2/edex/data/share
%defattr(777,awips,fxalpha,777)
%dir /awips2/edex/data/share/hydroapps
/awips2/edex/data/share/hydroapps/*
