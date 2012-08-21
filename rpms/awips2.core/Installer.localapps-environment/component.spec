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
Packager: Bryan Kowal

AutoReq: no

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

mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d

%build

%install
LOCALAPPS_RPM_DIR="rpms/awips2.core/Installer.localapps-environment"
PROFILED_DIR="${LOCALAPPS_RPM_DIR}/scripts/profile.d"

# Copy the profile.d scripts.
cp %{_baseline_workspace}/${PROFILED_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II localapps environment...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%post
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II localapps environment installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II localapps environment Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo ""

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(755,awips,fxalpha,-)
/etc/profile.d/*
