#
# /awips2/edex/data/gfe Spec File
#
Name: awips2-data.gfe
Summary: AWIPS II gfe Distribution
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
# We default the buildroot to determine whether or not the user has
# specified the buildroot.
BuildArch: noarch
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
Provides: awips2-data.gfe
Requires: awips2-edex
Requires: awips2-edex-configuration

%description
AWIPS II gfe Distribution - Contains the AWIPS II gfe netcdf files.

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
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data
if [ $? -ne 0 ]; then
   exit 1
fi

GFE_SRC_DIR="%{_static_files}/gfe"

cp -r ${GFE_SRC_DIR} ${RPM_BUILD_ROOT}/awips2/edex/data
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
%defattr(775,awips,fxalpha,775)
%dir /awips2/edex/data/gfe
/awips2/edex/data/gfe/*
