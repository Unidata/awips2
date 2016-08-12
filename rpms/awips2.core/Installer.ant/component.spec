%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
#
# AWIPS II Ant Spec File
#
Name: awips2-ant
Summary: AWIPS II Ant Distribution
Version: 1.7.1
Release: %{_component_version}.%{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: noarch
Prefix: /awips2/ant
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-ant

%description
AWIPS II Ant Distribution - Contains Ant V1.7.1

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "A Build Root has not been specified."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

rm -rf %{_build_root}
mkdir -p %{_build_root}

%build

%install
# Copies the standard Raytheon licenses into a license directory for the
# current component.
function copyLegal()
{
   # $1 == Component Build Root
   
   COMPONENT_BUILD_DIR=${1}
   
   mkdir -p %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
   
   # Create a Tar file with our FOSS licenses.
   tar -cjf %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      %{_baseline_workspace}/rpms/legal/FOSS_licenses/
   
   cp "%{_baseline_workspace}/rpms/legal/Master_Rights_File.pdf" \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}

mkdir -p ${RPM_BUILD_ROOT}/awips2/ant

_core_rpms="%{_baseline_workspace}/rpms/awips2.core"
_installer_ant="${_core_rpms}/Installer.ant"
ANT_TAR_FILE="apache-ant-1.7.1-bin.tar.gz"

# Will Be Extracted Into apache-ant-1.7.1
tar -xf ${_installer_ant}/src/${ANT_TAR_FILE} \
   -C %{_build_root}/awips2
# Move Files From 1.7.1 To The Generic Directory
cp -r %{_build_root}/awips2/apache-ant-1.7.1/* \
   %{_build_root}/awips2/ant 
rm -rf %{_build_root}/awips2/apache-ant-1.7.1

copyLegal "awips2/ant"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi

%post
if [ "${1}" = "2" ]; then
   exit 0
fi

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,awips,awips,-)
%dir /awips2/ant
%dir /awips2/ant/bin
%attr(755,awips,awips) /awips2/ant/bin/ant
%attr(644,awips,awips) /awips2/ant/bin/ant.bat
%attr(644,awips,awips) /awips2/ant/bin/ant.cmd
%attr(644,awips,awips) /awips2/ant/bin/antenv.cmd
%attr(755,awips,awips) /awips2/ant/bin/antRun
%attr(644,awips,awips) /awips2/ant/bin/antRun.bat
%attr(755,awips,awips) /awips2/ant/bin/antRun.pl
%attr(755,awips,awips) /awips2/ant/bin/complete-ant-cmd.pl
%attr(644,awips,awips) /awips2/ant/bin/envset.cmd
%attr(644,awips,awips) /awips2/ant/bin/lcp.bat
%attr(755,awips,awips) /awips2/ant/bin/runant.pl
%attr(755,awips,awips) /awips2/ant/bin/runant.py
%attr(644,awips,awips) /awips2/ant/bin/runrc.cmd
%docdir /awips2/ant/docs
/awips2/ant/docs
/awips2/ant/etc
/awips2/ant/fetch.xml
/awips2/ant/get-m2.xml
%doc /awips2/ant/INSTALL
%doc /awips2/ant/KEYS
/awips2/ant/lib
%doc /awips2/ant/LICENSE
%docdir /awips2/ant/licenses
/awips2/ant/licenses
%doc /awips2/ant/NOTICE
%doc /awips2/ant/README
%doc /awips2/ant/WHATSNEW
