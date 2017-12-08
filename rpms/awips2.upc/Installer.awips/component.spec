# RPM Metadata
%define _component_name           awips2
%define _component_desc           "awips2"
#
# awips2 Spec File
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: %{_component_name}
Summary: awips2 Installation
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
provides: %{_component_name}
Requires: wget unzip bc
Obsoletes: awips2-edex-upc

%description
%{_component_desc}

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

/bin/cp %{_baseline_workspace}/rpms/awips2.upc/Installer.awips/scripts/profile.d/* ${RPM_BUILD_ROOT}/etc/profile.d


%pre

%post

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}/*

%files
%attr(755,root,root) /etc/profile.d/awips2.csh
%attr(755,root,root) /etc/profile.d/awips2.sh
