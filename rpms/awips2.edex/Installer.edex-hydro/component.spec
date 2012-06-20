%define _component_name           awips2-edex-hydro
%define _component_project_dir    awips2.edex/Installer.edex-hydro
%define _component_zip            com.raytheon.uf.edex.hydro.feature-edex.zip
#
# AWIPS II Edex Hydro Spec File
#
Name: %{_component_name}
Summary: AWIPS II Edex Hydro
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
provides: awips2-edex-hydro
requires: awips2
requires: awips2-edex-base
requires: awips2-python
requires: awips2-java
requires: awips2-psql

%description
AWIPS II Edex Installation - Installs The AWIPS II Edex Hydro Plugins.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

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
mkdir -p ${RPM_BUILD_ROOT}

%build

%install
path_to_zip=${WORKSPACE_DIR}/Installer.rpm/awips2.edex/setup/dist

pushd . > /dev/null 2>&1
cd ${path_to_zip}
unzip %{_component_zip} -d ${RPM_BUILD_ROOT}  
popd > /dev/null 2>&1

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II Edex Hydro Plugins...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = /awips2/edex\e[m"

%post
if [ "${1}" = "2" ]; then   
   exit 0
fi

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Edex Hydro Plugin Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II Edex Hydro Plugins Have Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*
