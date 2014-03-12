#
# Generic AWIPS II XMPP Specs File
#

# Variables
# -----------------------------------------------------------------------------
%define _xmpp_server           openfire
%define _xmpp_software_version 3.9.1
%define _xmpp_software_url     http://www.igniterealtime.org/projects/openfire
%define _xmpp_software_license Apache License Version 2.0
# -----------------------------------------------------------------------------

Name: awips2-%{_xmpp_server}
Summary: AWIPS II XMPP Server
Version: %{_xmpp_software_version}
Release: 2
Group: AWIPSII
BuildRoot: /tmp
URL: %{_xmpp_software_url}
License: %{_xmpp_software_license}
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
requires: awips2-java
requires: awips2-httpd-collaboration
provides: awips2-%{_xmpp_server}

%description
The AWIPS II XMPP Server - %{_xmpp_server}

%prep
# Ensure that a valid build root has been supplied.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ] ||
   [ "${RPM_BUILD_ROOT}" = "" ]; then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

# Ensure that the required "generic" scripts are available.
xmpp_project="%{_baseline_workspace}/rpms/awips2.core/Installer.xmpp"
dist_scripts="${xmpp_project}/dist/scripts"

# the "packaging" script.
package_script="${dist_scripts}/package.sh"
if [ ! -f ${package_script} ]; then
   echo "ERROR: The package script does not exist - ${package_script}."
   exit 1
fi

# the configuration script.
configuration_script="${dist_scripts}/configure.sh"
if [ ! -f ${configuration_script} ]; then
   echo "ERROR: The configuration script does not exist - ${configuration_script}."
   exit 1
fi

%install
xmpp_project="%{_baseline_workspace}/rpms/awips2.core/Installer.xmpp"
dist_directory="${xmpp_project}/dist"
dist_scripts="${dist_directory}/scripts"
package_script="${dist_scripts}/package.sh"
configuration_script="${dist_scripts}/configure.sh"
filelist_txt="${dist_directory}/%{_xmpp_server}-files.txt"

# run the packaging script.
/bin/bash ${package_script} "%{_baseline_workspace}" "${RPM_BUILD_ROOT}"
if [ $? -ne 0 ]; then
   exit 1
fi

# copy the file list.
cp -v ${filelist_txt} %{_topdir}/BUILD
if [ $? -ne 0 ]; then
   exit 1
fi

# copy the configuration script.
mkdir -p ${RPM_BUILD_ROOT}/awips2/%{_xmpp_server}/tmp
if [ $? -ne 0 ]; then
   exit 1
fi
cp ${configuration_script} \
   ${RPM_BUILD_ROOT}/awips2/%{_xmpp_server}/tmp
if [ $? -ne 0 ]; then
   exit 1
fi

%pre

%post
configuration_script=/awips2/%{_xmpp_server}/tmp/configure.sh

# run the configuration script.
/bin/bash ${configuration_script}
if [ $? -ne 0 ]; then
   exit 1
fi

# remove the configuration script.
rm -rf /awips2/%{_xmpp_server}/tmp

%preun
if [ "${1}" = "2" ]; then   
   exit 0
fi

# Remove the openfire.sh link.
pushd . > /dev/null 2>&1
cd /awips2/openfire/bin
if [ -L openfire.sh ]; then
   rm -f openfire.sh
fi
popd > /dev/null 2>&1

# Remove and unregister the openfired service.
if [ -f /etc/init.d/openfired ]; then
   /sbin/chkconfig openfired off
   /sbin/chkconfig --del openfired
   
   rm -f /etc/init.d/openfired
fi

%postun

%clean
rm -f %{_topdir}/BUILD/%{_xmpp_server}-files.txt
rm -rf ${RPM_BUILD_ROOT}

%files -f %{_xmpp_server}-files.txt
%defattr(644,awips,fxalpha,755)
%dir /awips2

%defattr(755,root,root,755)
/awips2/%{_xmpp_server}/tmp/configure.sh
