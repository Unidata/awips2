%define _component_name           awips2-hydroapps-shared
%define _component_project_dir    awips2.core/Installer.hydroapps
#
# AWIPS II Hydroapps Spec File
#
Name: %{_component_name}
Summary: AWIPS II Hydroapps Distribution
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
provides: awips2-hydroapps-shared
obsoletes: awips2-hydroapps
requires: awips2-edex-base
requires: awips2-edex-native

%description
AWIPS II Hydroapps Distribution - Includes applications, configurations, and
filesystems for Hydro.

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

%install
NATIVE_TAR_FILE="dist.native/i386-pc-linux-gnu.tar"

# Untar the Native Library
/bin/gtar -xpf %{_baseline_workspace}/${NATIVE_TAR_FILE} \
   -C ${RPM_BUILD_ROOT}/awips2
   
# Remove all unnecessary files and directories.
rm -rf ${RPM_BUILD_ROOT}/awips2/adapt
rm -rf ${RPM_BUILD_ROOT}/awips2/lib
rm -rf ${RPM_BUILD_ROOT}/awips2/setup
rm -rf ${RPM_BUILD_ROOT}/awips2/edex
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/share
mv ${RPM_BUILD_ROOT}/awips2/awipsShare/hydroapps \
   ${RPM_BUILD_ROOT}/awips2/edex/data/share
rm -rf ${RPM_BUILD_ROOT}/awips2/awipsShare

# Add the ffmp_templates directory - part of the tar file now.
#mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/hydroapps/ffmp_templates

# Add file for VIM - part of the tar file now.
#WHFS_LOCAL="edex/data/hdf5/hydroapps/whfs/local/data"
#mkdir -p ${RPM_BUILD_ROOT}/awips2/${WHFS_LOCAL}/app/
#touch ${RPM_BUILD_ROOT}/awips2/${WHFS_LOCAL}/app/.vimrc

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Hydro Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = /awips2\e[m"
echo -e "\e[1;34m         Destination = /awips2/edex/data/share/hydroapps\e[m"

%post
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Hydro Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Hydro Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,775)
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/data
%dir /awips2/edex/data/share
%defattr(777,awips,fxalpha,777)
%dir /awips2/edex/data/share/hydroapps
/awips2/edex/data/share/hydroapps/*