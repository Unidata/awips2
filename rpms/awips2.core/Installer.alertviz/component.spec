%define CORE_DELTA_SETUP ${WORKSPACE_DIR}/Installer.rpm/delta/setup/updateSetup.sh
%define _component_name           awips2-alertviz
%define _component_project_dir    awips2.core/Installer.alertviz
%define _component_default_prefix /awips2/alertviz
#
# AWIPS II AlertViz Spec File
#
%define __prelink_undo_cmd %{nil}

Name: %{_component_name}
Summary: AWIPS II AlertViz
Version: 1.0.0
Release: 1
Group: AWIPSII
BuildRoot: /tmp
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-alertviz
requires: awips2-python
requires: awips2-java

%description
AWIPS II AlertViz Distribution - the AWIPS II AlertViz application.

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

ALERTVIZ_ZIP_FILE="build/alertviz/tmp/I.AlertViz/AlertViz-linux.gtk.x86.zip"

cd ${RPM_BUILD_ROOT}/awips2
unzip ${WORKSPACE_DIR}/${ALERTVIZ_ZIP_FILE}

# Also Need To Create The alertvizEnvironment Directory
mkdir -p ${RPM_BUILD_ROOT}/awips2/alertviz/alertvizEnvironment

# Create our autostart template
mkdir -p ${RPM_BUILD_ROOT}/etc/xdg/autostart
cp ${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.alertviz/scripts/autostart/awips2-alertviz.desktop \
   ${RPM_BUILD_ROOT}/etc/xdg/autostart
   
copyLegal "awips2/alertviz"
   
%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II AlertViz...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
# Remove the alertviz autostart script if we have been installed on
# an xt machine.
if [ "`hostname | cut -b 1-2`" = "xt" ]; then
   # Remove the awips2-alertviz autostart script.
   rm -f /etc/xdg/autostart/awips2-alertviz.desktop
fi
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
echo "--------------------------------------------------------------------------------"
echo "\| Setting up the AWIPS II AlertViz Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II AlertViz Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II AlertViz Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,awips,fxalpha,-)
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
%dir %{_component_default_prefix}/delta
%attr(700,root,root) %{_component_default_prefix}/delta/updateManager.sh
%attr(700,root,root) %{_component_default_prefix}/delta/createUpdateRegistry.sh
%{_component_default_prefix}/delta/%{_component_name}
#---------------------------------------------------------------------------#
%dir /awips2
%dir /awips2/alertviz
/awips2/alertviz/.eclipseproduct
%docdir /awips2/alertviz/about_files
%dir /awips2/alertviz/about_files
/awips2/alertviz/about_files/*
%doc /awips2/alertviz/about.html
%dir /awips2/alertviz/alertvizEnvironment
/awips2/alertviz/alertviz.ini
%dir /awips2/alertviz/configuration
/awips2/alertviz/configuration/*
%dir /awips2/alertviz/etc
/awips2/alertviz/etc/*
%dir /awips2/alertviz/features
/awips2/alertviz/features/*
%docdir /awips2/alertviz/licenses
%dir /awips2/alertviz/licenses
/awips2/alertviz/licenses/*
%dir /awips2/alertviz/plugins
/awips2/alertviz/plugins/*

%defattr(755,awips,fxalpha,755)
/awips2/alertviz/alertviz
/awips2/alertviz/*.so
/awips2/alertviz/*.sh

%attr(644,root,root) /etc/xdg/autostart/awips2-alertviz.desktop