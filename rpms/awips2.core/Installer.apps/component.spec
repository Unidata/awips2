Name: awips2-apps
Summary: awips2-apps Installation
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
Provides: awips2-apps
Requires: awips2

%description
Provides the /awips2/apps directory for Local Applications

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir --parents ${RPM_BUILD_ROOT}

%install
mkdir --parents %{_build_root}/awips2/apps
if [ $? -ne 0 ]; then
   exit 1
fi

%clean
rm --recursive --force ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/apps
