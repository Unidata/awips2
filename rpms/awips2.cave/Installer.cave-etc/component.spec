# RPM Metadata
%define _component_name           awips2-cave-etc
%define _component_desc           "awips2-cave-etc"
#
# awips2-cave-etc Spec File
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: %{_component_name}
Summary: awips2-cave Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: %{_component_name}
requires: awips2-cave
requires: awips2

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

if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/cave/etc

%build

%install
BASELINE_ETC_DIR="build/static/common/cave/etc"
ETC_DIR_LOC="%{_baseline_workspace}/${BASELINE_ETC_DIR}"

if [ ! -d ${ETC_DIR_LOC} ]; then
   echo "ERROR: Unable to find CAVE etc directory."
   exit 1
fi

cp -r ${ETC_DIR_LOC}/* ${RPM_BUILD_ROOT}/awips2/cave/etc
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Copy of CAVE etc directory has failed."
   exit 1
fi

%pre
# Ensure that CAVE is available. There is not any point in installing
# the files if CAVE is not available.
if [ ! -f /awips2/cave/cave ]; then
   echo "ERROR: The cave executable was not found or is corrupt - /awips2/cave/cave;"
   echo "       awips2-cave must be re-installed. This installation will be terminated."
   exit 1
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/cave
%dir /awips2/cave/etc
/awips2/cave/etc/*
%defattr(664,awips,fxalpha,755)
/awips2/cave/etc/aviation/config/*