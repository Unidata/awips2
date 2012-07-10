#
# AWIPS II gfe.climo Spec File
#
Name: awips2-data.hdf5-gfe.climo
Summary: AWIPS II gfe.climo Distribution
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
Prefix: /awips2/edex
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-data.hdf5-gfe.climo

%description
AWIPS II gfe.climo Distribution - Contains the AWIP II gfe climo HDF Files. The
gfe climo Files Will Be Copied To The Specified Destination.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/gfe/climo

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
   
   cp %{_baseline_workspace}/rpms/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/rpms/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}

# Determine which version of the climo we should use.
RPM_COMMON_DIR="%{_baseline_workspace}/rpms/common/static.versions"

if [ ! -f ${RPM_COMMON_DIR}/LATEST.climo ]; then
   file ${RPM_COMMON_DIR}/LATEST.climo
   exit 1
fi
VERSION_DIR=`cat ${RPM_COMMON_DIR}/LATEST.climo`

GFE_CLIMO_SRC_DIR="%{_awipscm_share}/awips2-static/climo/${VERSION_DIR}"
if [ ! -d ${GFE_CLIMO_SRC_DIR} ]; then
   file ${GFE_CLIMO_SRC_DIR}
   exit 1
fi

cp -rv ${GFE_CLIMO_SRC_DIR}/${climoFile}/* \
   ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/gfe/climo
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

copyLegal "awips2/edex/data/hdf5/gfe/climo"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II gfe.climo Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"
echo -e "\e[1;34m         Destination = ${RPM_INSTALL_PREFIX}/data/hdf5/gfe/climo\e[m"
echo ""

%post
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II gfe.climo Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II gfe.climo Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/edex
%dir /awips2/edex/data
%dir /awips2/edex/data/hdf5
%dir /awips2/edex/data/hdf5/gfe
%dir /awips2/edex/data/hdf5/gfe/climo
/awips2/edex/data/hdf5/gfe/climo/*
%docdir /awips2/edex/data/hdf5/gfe/climo/licenses
%dir /awips2/edex/data/hdf5/gfe/climo/licenses
/awips2/edex/data/hdf5/gfe/climo/licenses/*