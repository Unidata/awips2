%define CORE_DELTA_SETUP ${WORKSPACE_DIR}/Installer.rpm/delta/setup/updateSetup.sh
%define _component_name           awips2-edex-shapefiles
%define _component_project_dir    awips2.edex/Installer.edex-shapefiles
%define _component_default_prefix /awips2
#
# AWIPS II Edex Shapefiles Spec File
#
Name: %{_component_name}
Summary: AWIPS II Edex Shapefiles
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
provides: awips2-edex-shapefiles
requires: awips2
requires: awips2-edex-base

%description
AWIPS II Edex Static Data Distribution - includes the shapefiles and the
maps database (when awips2-postgresql is installed).

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/utility/edex_static/base/shapefiles

%build

%install
# Copy The shapefiles.
SHAPEFILES_DEST_DIR="awips2/edex/data/utility/edex_static/base/shapefiles"
STATIC_DATA_DIR="${AWIPSCM_SHARE}/awips2-static"

# Determine which version of the shapefiles we should use.
RPM_COMMON_DIR="${WORKSPACE_DIR}/Installer.rpm/common/static.versions"

if [ ! -f ${RPM_COMMON_DIR}/LATEST.maps ]; then
   file ${RPM_COMMON_DIR}/LATEST.maps
   exit 1
fi
VERSION_DIR=`cat ${RPM_COMMON_DIR}/LATEST.maps`
SHAPEFILES_DIR="${STATIC_DATA_DIR}/maps/${VERSION_DIR}/shapefiles"
if [ ! -d ${SHAPEFILES_DIR} ]; then
   file ${SHAPEFILES_DIR}
   exit 1
fi
cp -r ${SHAPEFILES_DIR}/* ${RPM_BUILD_ROOT}/${SHAPEFILES_DEST_DIR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
   
%pre
if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing The AWIPS II Edex Shapefiles...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}/edex\e[m"

%post
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| SUCCESSFUL INSTALLATION ~ awips2-edex-shapefiles\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
   
%preun
if [ "${1}" = "1" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| SUCCESSFUL UNINSTALLATION ~ awips2-edex-shapefiles\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
   
%files
%defattr(775,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*
