#
# AWIPS II localapps Environment Spec File
#
Name: awips2-localapps-environment
Summary: AWIPS II localapps Environment Spec File
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon

AutoReq: no
Provides: awips2-localapps-environment

%description
AWIPS II localapps Environment Spec File - This rpm will install
a shell script in /etc/profile.d that when sourced will set all
of the environment variables required by the "localapps".

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
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

LOCALAPPS_RPM_DIR="rpms/awips2.core/Installer.localapps-environment"
PROFILED_DIR="${LOCALAPPS_RPM_DIR}/scripts/profile.d"

# Copy the profile.d scripts.
cp %{_baseline_workspace}/${PROFILED_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d
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
%defattr(755,awips,fxalpha,-)
/etc/profile.d/*
