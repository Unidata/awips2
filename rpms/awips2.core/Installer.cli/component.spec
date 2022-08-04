%define _component_name           awips2-cli
%define _component_project_dir    awips2.core/Installer.cli
%define _component_default_prefix /awips2/fxa
#
# AWIPS II CLI Spec File
#
Name: %{_component_name}
Summary: AWIPS II CLI Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
Provides: awips2-cli
Requires: awips2-python

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
# This Is The Workspace Project That Contains The Files That We
# Need For The CLI Component Installer.
CLI_PROJECT_DIR="com.raytheon.uf.tools.cli"

# Create the bin Directory for the CLI Component
mkdir -p ${RPM_BUILD_ROOT}/awips2/fxa/bin
cp -r %{_baseline_workspace}/${CLI_PROJECT_DIR}/impl/* ${RPM_BUILD_ROOT}/awips2/fxa/bin

copyLegal "awips2/fxa"

# Copy our profile.d scripts.
PROFILE_D_DIRECTORY="rpms/awips2.core/Installer.cli/scripts/profile.d"
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
cp %{_baseline_workspace}/${PROFILE_D_DIRECTORY}/* \
   ${RPM_BUILD_ROOT}/etc/profile.d

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
%defattr(644,awips,fxalpha,755)
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
