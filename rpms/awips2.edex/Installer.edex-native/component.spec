#
# AWIPS II Edex Cots Spec File
#
Name: awips2-edex-native
Summary: AWIPS II Edex Native
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
Prefix: /awips2
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-edex-native
requires: awips2
requires: awips2-edex-base

%description
AWIPS II Edex Installation - Installs The AWIPS II EDEX Native Libraries.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

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
   
# Remove all unnecessary files and directories.
rm -rf ${RPM_BUILD_ROOT}/awips2/adapt
rm -rf ${RPM_BUILD_ROOT}/awips2/lib
rm -rf ${RPM_BUILD_ROOT}/awips2/awipsShare
rm -rf ${RPM_BUILD_ROOT}/awips2/setup
rm -rf ${RPM_BUILD_ROOT}/awips2/edex/data

# Copy the mhs libraries to the native directory structure
MHS_LIB_DIR="${AWIPSCM_SHARE}/packages/mhs"
# Need to copy mhs libraries.
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/lib/native/linux32/awips1

cp -r ${MHS_LIB_DIR}/* \
   ${RPM_BUILD_ROOT}/awips2/edex/lib/native/linux32/awips1
   
%pre
if [ "${1}" = "1" ]; then
   # This Is An Installation - Not An Upgrade.
   # Ensure That We Are Being Installed To The Correct Location.
   EDEX_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-edex-base`
   if [ ! "${RPM_INSTALL_PREFIX}" = "${EDEX_INSTALL}" ]; then
      echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
      echo -e "\e[1;31m\| ERROR: These Libraries MUST Be Installed At The Same Location As EDEX!!!" 
      echo -e "\e[1;34m\|  INFO: Use '--prefix=${EDEX_INSTALL}'.\e[m"
      echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"

      exit 1
   fi
fi

if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II EDEX Native Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"
echo -e "\e[1;34m         Destination = ${RPM_INSTALL_PREFIX}/edex/lib\e[m"

%post
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II EDEX Native Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II EDEX Native Libraries Have Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex

%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*
%dir /awips2/edex/lib
/awips2/edex/lib/*