%define _component_name           awips2-edex-hydro
%define _component_project_dir    awips2.edex/Installer.edex-hydro
#
# AWIPS II Edex Hydro Spec File
#
Name: %{_component_name}
Summary: AWIPS II Edex Hydro
Version: 1.0.0
Release: 1
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

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex

%build

%install
DEPLOY_SCRIPT="build.edex/deploy-install.xml"

# Deploy Edex To Our Temporary Build Directory.

ANT_EXE="/awips2/ant/bin/ant"
if [ ! -f ${ANT_EXE} ]; then
   echo "ERROR: awips2-ant Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

${ANT_EXE} -file ${WORKSPACE_DIR}/${DEPLOY_SCRIPT} \
   -Dinstall.dir=${RPM_BUILD_ROOT}/awips2/edex \
   -Dinstaller=true -Dlocal.build=false \
   -Dcomponent.to.deploy=edex-hydro

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