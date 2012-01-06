#
# AWIPS II Adapt Native Spec File
#
Name: awips2-adapt-native
Summary: AWIPS II Adapt Native Distribution
Version: 1.0.0
Release: 1
Group: AWIPSII
BuildRoot: /tmp
Prefix: /awips2
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-adapt-native
requires: awips2-edex-base

# The two python native libraries are now included directly in the python
# distribution that is built.
%description
AWIPS II Native Distribution - Contains the AWIP II Native Files for Adapt.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2

%build

%install
NATIVE_TAR_FILE="dist.native/i386-pc-linux-gnu.tar"

/bin/gtar -xpf ${WORKSPACE_DIR}/${NATIVE_TAR_FILE} \
   -C ${RPM_BUILD_ROOT}/awips2

# Remove Unnecessary Directories.
rm -rf ${RPM_BUILD_ROOT}/awips2/setup
rm -rf ${RPM_BUILD_ROOT}/awips2/edex
rm -rf ${RPM_BUILD_ROOT}/awips2/lib
rm -rf ${RPM_BUILD_ROOT}/awips2/awipsShare

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
# Need To Ensure That We Are Lined Up With The Current Edex (Base) Installation.
EDEX_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-edex-base`
if [ ! "${RPM_INSTALL_PREFIX}" = "${EDEX_INSTALL}" ]; then
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| ERROR: This Extension MUST Be Installed At The Same Location As EDEX!!!" 
   echo -e "\e[1;34m\|  INFO: Use '--prefix=${EDEX_INSTALL}'.\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"

   exit 1
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Adapt Native Libraries...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}/adapt\e[m"

%post
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Adapt Native Library Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Adapt Native Libraries Have Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m" 

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/adapt
%dir /awips2/adapt/climate
%dir /awips2/adapt/climate/data
/awips2/adapt/climate/data/*
%dir /awips2/adapt/climate/bin

%defattr(755,awips,fxalpha,755)
%dir /awips2/adapt/climate/bin/Linux
/awips2/adapt/climate/bin/Linux/*