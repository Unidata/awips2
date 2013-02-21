%define _component_name           awips2-notification
%define _component_project_dir    awips2.core/Installer.notification
%define _component_default_prefix /awips2/notification
#
# AWIPS II Notification Spec File
#
%define __prelink_undo_cmd %{nil}

Name: %{_component_name}
Summary: AWIPS II Notification
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
requires: boost >= 1.33.1
provides: awips2-notification

%description
AWIPS II Notification Distribution - the AWIPS II Notification application.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/notification
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

PROFILE_D_DIR="rpms/awips2.core/Installer.notification/scripts/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

%build

%install
# Copies the standard Raytheon licenses into a license directory for the
# current component.
function copyLegal()
{
   # $1 == Component Build Root
   
   COMPONENT_BUILD_DIR=${1}
   
   mkdir -p ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   
   # Create a Tar file with our FOSS licenses.
   tar -cjf %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      %{_baseline_workspace}/rpms/legal/FOSS_licenses/
   
   cp %{_baseline_workspace}/rpms/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/rpms/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}
BUILD_NATIVE="%{_baseline_workspace}/build.native"

pushd . > /dev/null 2>&1
cd ${BUILD_NATIVE}
/bin/bash build-notification.sh %{_baseline_workspace} \
   %{_uframe_eclipse} ${RPM_BUILD_ROOT}
if [ $? -ne 0 ]; then
   exit 1
fi
rm -rf ${RPM_BUILD_ROOT}/workspace_
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null 2>&1

copyLegal "awips2/notification"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II Notification...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
if [ "${1}" = "2" ]; then   
   exit 0
fi
echo "--------------------------------------------------------------------------------"
echo "\| Setting up the AWIPS II Notification Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

echo "--------------------------------------------------------------------------------"
echo "\| Adding Environment Variables for AWIPS II Notification"
echo "--------------------------------------------------------------------------------"

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Notification Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II Notification Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2Notification.csh
%attr(755,root,root) /etc/profile.d/awips2Notification.sh
%dir /awips2/notification
%dir /awips2/notification/include
/awips2/notification/include/*
%dir /awips2/notification/lib
/awips2/notification/lib/*
%docdir /awips2/notification/licenses
%dir /awips2/notification/licenses
/awips2/notification/licenses/*
%dir /awips2/notification/src
/awips2/notification/src/*

%defattr(755,awips,fxalpha,755)
%dir /awips2/notification/bin
/awips2/notification/bin/*