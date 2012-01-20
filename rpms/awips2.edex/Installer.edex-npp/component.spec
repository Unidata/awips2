%define _component_name           awips2-edex-npp
#
# AWIPS II Edex NPP Spec File
#
Name: %{_component_name}
Summary: AWIPS II Edex NPP Plugins
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
requires: awips2
requires: awips2-edex-base
requires: awips2-python
requires: awips2-java
requires: awips2-psql

%description
AWIPS II Edex Installation - Installs The AWIPS II Edex NPP Plugins.

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

# Determine which ant executable to use.
COMMAND=`rpm -q awips2-ant`
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: awips2-ant Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

ANT_EXE="/awips2/ant/bin/ant"

${ANT_EXE} -file ${WORKSPACE_DIR}/${DEPLOY_SCRIPT} \
   -Dinstall.dir=${RPM_BUILD_ROOT}/awips2/edex \
   -Dinstaller=true -Dlocal.build=false \
   -Dcomponent.to.deploy=edex-npp
   
%pre

%post

%preun

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*