Name: awips2-scripts
Summary: AWIPS II Maintenance Scripts
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
Provides: awips2-scripts

%description
AWIPS II Maintenance Scripts

%prep
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An actual buildroot must be specified. Use the --buildroot parameter."
   echo "Unable to continue ... terminating"
   exit 1
fi


%build

%install

mkdir -p ${RPM_BUILD_ROOT}/awips2/scripts

cp %{_baseline_workspace}/rpms/awips2.core/Installer.scripts/scripts/* \
   ${RPM_BUILD_ROOT}/awips2/scripts

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(755,awips,fxalpha,755)
%dir /awips2/scripts
/awips2/scripts/*
