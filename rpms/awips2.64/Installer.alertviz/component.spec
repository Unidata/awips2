%define _component_name           awips2-alertviz
#
# AWIPS II AlertViz Spec File
#
%define __prelink_undo_cmd %{nil}

Name: %{_component_name}
Summary: AWIPS II AlertViz
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
# perform the AlertViz pde build.
cd %{_baseline_workspace}/build

# TODO: Eliminate the hard-coded uframe-eclipse location.
time ./build.sh -eclipse=/opt/uframe-eclipse
if [ $? -ne 0 ]; then
   exit 1
fi

%install
# Copies the standard Raytheon licenses into a license directory for the
# current component.
function copyLegal()
{
   # $1 == Component Build Root
   
   COMPONENT_BUILD_DIR=${1}
   
   mkdir -p ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   
   # Create a Tar file with our FOSS licenses.
   tar -cjf %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses.tar \
      %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses/
   
   cp %{_baseline_workspace}/Installer.rpm/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/Installer.rpm/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses.tar    
}

ALERTVIZ_ZIP_FILE="build/alertviz/tmp/I.AlertViz/AlertViz-linux.gtk.x86_64.zip"

cd ${RPM_BUILD_ROOT}/awips2
unzip %{_baseline_workspace}/${ALERTVIZ_ZIP_FILE}

# Also Need To Create The alertvizEnvironment Directory
mkdir -p ${RPM_BUILD_ROOT}/awips2/alertviz/alertvizEnvironment

# Create our autostart template
mkdir -p ${RPM_BUILD_ROOT}/etc/xdg/autostart
cp %{_baseline_workspace}/Installer.rpm/awips2.core/Installer.alertviz/scripts/autostart/awips2-alertviz.desktop \
   ${RPM_BUILD_ROOT}/etc/xdg/autostart

copyLegal "awips2/alertviz"
   
%pre

%post
# Remove the alertviz autostart script if we have been installed on
# an xt machine.
if [ "`hostname | cut -b 1-2`" = "xt" ]; then
   # Remove the awips2-alertviz autostart script.
   rm -f /etc/xdg/autostart/awips2-alertviz.desktop
fi

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
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