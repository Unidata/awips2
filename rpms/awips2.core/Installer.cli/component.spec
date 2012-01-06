%define CORE_DELTA_SETUP ${WORKSPACE_DIR}/Installer.rpm/delta/setup/updateSetup.sh
%define _component_name           awips2-cli
%define _component_project_dir    awips2.core/Installer.cli
%define _component_default_prefix /awips2/fxa
#
# AWIPS II CLI Spec File
#
Name: %{_component_name}
Summary: AWIPS II CLI Installation
Version: 1.0.0
Release: 1
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-cli
requires: awips2-python

%description
AWIPS II CLI Installation - Contains The AWIPS II CLI Component.

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

mkdir -p ${RPM_BUILD_ROOT}/awips2/fxa

%build
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
source %{CORE_DELTA_SETUP}
copySetupCore ${RPM_BUILD_ROOT} %{_component_default_prefix}
copyApplicableDeltas ${RPM_BUILD_ROOT} %{_component_name} \
   %{_component_project_dir} %{_component_default_prefix}
#---------------------------------------------------------------------------#

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
# This Is The Workspace Project That Contains The Files That We
# Need For The CLI Component Installer.
CLI_PROJECT_DIR="com.raytheon.uf.tools.cli"

# Create the bin Directory for the CLI Component
mkdir -p ${RPM_BUILD_ROOT}/awips2/fxa/bin
cp -r ${WORKSPACE_DIR}/${CLI_PROJECT_DIR}/impl/* ${RPM_BUILD_ROOT}/awips2/fxa/bin

copyLegal "awips2/fxa"

# Copy our profile.d scripts.
PROFILE_D_DIRECTORY="Installer.rpm/awips2.core/Installer.cli/scripts/profile.d"
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
cp ${WORKSPACE_DIR}/${PROFILE_D_DIRECTORY}/* \
   ${RPM_BUILD_ROOT}/etc/profile.d

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II CLI...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
if [ "${1}" = "2" ]; then
   echo "INFO: Performing %{_component_name} Upgrade."
   echo "Preparing ..."
   
   # Check the delta directory to see if there are updates that
   # may need to be applied.
   cd ${RPM_INSTALL_PREFIX}/delta/%{_component_name}
   COUNT=`ls -1 | wc -l`
   
   if [ "${COUNT}" = "0" ]; then
      echo "INFO: No Updates To Perform."
      exit 0
   fi
   
   echo "INFO: Potentially Applying ${COUNT} Updates."
   
   # The Update Manager Is In: ${RPM_INSTALL_PREFIX}/delta
   UPDATE_MANAGER="${RPM_INSTALL_PREFIX}/delta/updateManager.sh"
   cd ${RPM_INSTALL_PREFIX}/delta
   export COMPONENT_INSTALL="${RPM_INSTALL_PREFIX}"
   ${UPDATE_MANAGER} %{_component_name}
   
   exit 0
fi
#---------------------------------------------------------------------------#
PYTHON_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-python`
PYTHON_INSTALL=${PYTHON_INSTALL}

function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II CLI Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
}
echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II CLI Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II CLI Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II CLI Installation Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo ""

%files
%defattr(644,awips,fxalpha,755)
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
%dir %{_component_default_prefix}/delta
%attr(700,root,root) %{_component_default_prefix}/delta/updateManager.sh
%attr(700,root,root) %{_component_default_prefix}/delta/createUpdateRegistry.sh
%{_component_default_prefix}/delta/%{_component_name}
#---------------------------------------------------------------------------#
%dir /awips2/fxa
%docdir /awips2/fxa/licenses
%dir /awips2/fxa/licenses
/awips2/fxa/licenses/*
/etc/profile.d/awips2CLI.csh
/etc/profile.d/awips2CLI.sh
%defattr(755,awips,fxalpha,755)
%dir /awips2/fxa/bin
%attr(755,awips,fxalpha) /awips2/fxa/bin/*
%config(noreplace) /awips2/fxa/bin/setup.env
