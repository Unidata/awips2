#
# AWIPS II Ant Spec File
#
Name: awips2-ant
Summary: AWIPS II Ant Distribution
Version: 1.7.1
Release: 2
Group: AWIPSII
BuildRoot: /tmp
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

mkdir -p ${RPM_BUILD_ROOT}/awips2/ant
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d

PROFILE_D_DIR="rpms/awips2.core/Installer.ant/scripts/profile.d"
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

ANT_TAR_FILE="apache-ant-1.7.1-bin.tar.gz"
ANT_TAR_FILE_SRC_DIR="CMLibrary/Software/OpenSource/Ant/v1.7.1"

# Will Be Extracted Into apache-ant-1.7.1
tar -xf %{_awipscm_share}/${ANT_TAR_FILE_SRC_DIR}/${ANT_TAR_FILE} \
   -C ${RPM_BUILD_ROOT}/awips2
# Move Files From 1.7.1 To The Generic Directory
cp -r ${RPM_BUILD_ROOT}/awips2/apache-ant-1.7.1/* \
   ${RPM_BUILD_ROOT}/awips2/ant 
rm -rf ${RPM_BUILD_ROOT}/awips2/apache-ant-1.7.1 

copyLegal "awips2/ant"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Ant Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II Ant Distribution Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
}
echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II Ant Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

echo "--------------------------------------------------------------------------------"
echo "\| Adding Environment Variables for AWIPS II Ant"
echo "--------------------------------------------------------------------------------"

if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Ant Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Ant Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,awips,fxalpha,-)
%attr(755,root,root) /etc/profile.d/awips2Ant.csh
%attr(755,root,root) /etc/profile.d/awips2Ant.sh
%dir /awips2/ant
%dir /awips2/ant/bin
%attr(755,awips,fxalpha) /awips2/ant/bin/ant
%attr(644,awips,fxalpha) /awips2/ant/bin/ant.bat
%attr(644,awips,fxalpha) /awips2/ant/bin/ant.cmd
%attr(644,awips,fxalpha) /awips2/ant/bin/antenv.cmd
%attr(755,awips,fxalpha) /awips2/ant/bin/antRun
%attr(644,awips,fxalpha) /awips2/ant/bin/antRun.bat
%attr(755,awips,fxalpha) /awips2/ant/bin/antRun.pl
%attr(755,awips,fxalpha) /awips2/ant/bin/complete-ant-cmd.pl
%attr(644,awips,fxalpha) /awips2/ant/bin/envset.cmd
%attr(644,awips,fxalpha) /awips2/ant/bin/lcp.bat
%attr(755,awips,fxalpha) /awips2/ant/bin/runant.pl
%attr(755,awips,fxalpha) /awips2/ant/bin/runant.py
%attr(644,awips,fxalpha) /awips2/ant/bin/runrc.cmd
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