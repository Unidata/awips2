%define _component_name           awips2-rcm
%define _component_project_dir    awips2.core/Installer.rcm
%define _component_default_prefix /awips2/rcm
#
# AWIPS II Edex Radar Server File
#
Name: %{_component_name}
Summary: AWIPS II Radar Server
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
provides: awips2-rcm
requires: awips2-java

%description
AWIPS II Radar Server Installation - Sets Up AWIPS II Radar Server.

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

mkdir -p ${RPM_BUILD_ROOT}/awips2/rcm

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
   tar -cjf ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar \
      ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses/
   
   cp ${WORKSPACE_DIR}/Installer.rpm/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "${WORKSPACE_DIR}/Installer.rpm/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar    
}
RCM_PROPS_DIR="build.rcm/pdeprops"
DEPLOY_SCRIPT="build.rcm/build.xml"

# Deploy Radar Server To Our Temporary Build Directory.
/awips2/ant/bin/ant -file ${WORKSPACE_DIR}/${DEPLOY_SCRIPT} \
   -Ddeploy.dir=${RPM_BUILD_ROOT}/awips2/rcm \
   -Dinstaller=true -Dprops.dir=${WORKSPACE_DIR}/${RCM_PROPS_DIR}
   
# Overwrite the Existing start-config File With The Our start-config File.
rm -f ${RPM_BUILD_ROOT}/awips2/rcm/data/config/start-config
cp ${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.rcm/scripts/conf/start-config \
   ${RPM_BUILD_ROOT}/awips2/rcm/data/config/

# Create the radar server logs directory
mkdir -p ${RPM_BUILD_ROOT}/awips2/rcm/data/logs

# Include the rcm service script
mkdir -p ${RPM_BUILD_ROOT}/etc/init.d
cp ${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.rcm/scripts/init.d/edex_rcm \
   ${RPM_BUILD_ROOT}/etc/init.d

copyLegal "awips2/rcm"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II Radar Server (RCM)...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
if [ "${1}" = "2" ]; then   
   exit 0
fi
echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II Radar Server Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

if [ -f /etc/init.d/edex_rcm ]; then
   /sbin/chkconfig --add edex_rcm
fi

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Radar Server (RCM) Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi
if [ -f /etc/init.d/edex_rcm ]; then
   /sbin/chkconfig --del edex_rcm
fi

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II Radar Server (RCM) Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/rcm
%dir /awips2/rcm/data
/awips2/rcm/data/*

%docdir /awips2/rcm/licenses
%dir /awips2/rcm/licenses
/awips2/rcm/licenses/*

%defattr(755,awips,fxalpha,755)
%dir /awips2/rcm/bin
/awips2/rcm/bin/*
%dir /awips2/rcm/lib
/awips2/rcm/lib/*

%attr(744,root,root) /etc/init.d/edex_rcm
