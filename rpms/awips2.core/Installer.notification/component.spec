%define _component_name           awips2-notification
%define _component_project_dir    awips2.core/Installer.notification
%define _component_default_prefix /awips2/notification
%define _cdt_build_loc %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
%define _build_arch %(uname -i)
%if %_build_arch == "x86_64"
   %define   _lib_dir lib64
%else
   %define _lib_dir lib
%endif
#
# AWIPS II Notification Spec File
#
%define __prelink_undo_cmd %{nil}

Name: %{_component_name}
Summary: AWIPS II Notification
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: %{_build_arch}
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Requires: boost >= 1.33.1
Requires: awips2-qpid-lib
Provides: awips2-notification

BuildRequires: awips2-java
BuildRequires: awips2-qpid-lib, libcurl-devel, libuuid-devel

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
if [ -d %{_cdt_build_loc} ]; then
   rm -rf %{_cdt_build_loc}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

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
   
   cp "%{_baseline_workspace}/rpms/legal/Master_Rights_File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}

mkdir -p ${RPM_BUILD_ROOT}/awips2/notification
if [ $? -ne 0 ]; then
   exit 1
fi

BUILD_NATIVE="%{_baseline_workspace}/build.native"
mkdir -p %{_cdt_build_loc}
if [ $? -ne 0 ]; then
   exit 1
fi

pushd . > /dev/null 2>&1
cd ${BUILD_NATIVE}
/bin/bash build-notification.sh %{_baseline_workspace} \
   %{_uframe_eclipse} %{_cdt_build_loc} ${RPM_BUILD_ROOT}
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null 2>&1

copyLegal "awips2/notification"

%pre
%post
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}
rm -rf %{_cdt_build_loc}

%files
%defattr(644,awips,awips,755)
%dir /awips2/notification
%dir /awips2/notification/include
/awips2/notification/include/*
%dir /awips2/notification/%{_lib_dir}
/awips2/notification/%{_lib_dir}/*
%docdir /awips2/notification/licenses
%dir /awips2/notification/licenses
/awips2/notification/licenses/*
%dir /awips2/notification/src
/awips2/notification/src/*

%defattr(755,awips,awips,755)
%dir /awips2/notification/bin
/awips2/notification/bin/*
