#
# AWIPS II Topo Spec File
#
Name: awips2-data.hdf5-topo
Summary: AWIPS II Topo Distribution
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
provides: awips2-data.hdf5-topo

%description
AWIPS II Topo Distribution - Contains the AWIP II Topo HDF Files. The
Topo Files Will Be Copied To The Specified Destination.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/topo

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

# Determine which version of the topo we should use.
RPM_COMMON_DIR="%{_baseline_workspace}/rpms/common/static.versions"

if [ ! -f ${RPM_COMMON_DIR}/LATEST.topo ]; then
   file ${RPM_COMMON_DIR}/LATEST.topo
   exit 1
fi
VERSION_DIR=`cat ${RPM_COMMON_DIR}/LATEST.topo`
TOPO_SRC_DIR="awips2-static/topo/${VERSION_DIR}"
if [ ! -d %{_awipscm_share}/${TOPO_SRC_DIR} ]; then
   file %{_awipscm_share}/${TOPO_SRC_DIR}
   exit 1
fi

TOPO_TO_COPY=\
(\
   'srtm30.hdf'\
   'akTopo.dat.gz' \
   'caribTopo.dat.gz' \
   'modelStaticTopo.h5' \
   'pacTopo.dat.gz' \
   'staticTopo.h5' \
   'usTopo.dat.gz' \
   'worldTopo.dat.gz' \
)
# !!!!! WARNING - THIS WILL TAKE A LONG TIME !!!!!

for topoFile in ${TOPO_TO_COPY[*]};
do
   cp -r %{_awipscm_share}/${TOPO_SRC_DIR}/${topoFile} \
      ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/topo
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi
done

# Copy our hlsTopo
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/topo/hlsTopo
cp -r %{_awipscm_share}/${TOPO_SRC_DIR}/hlsTopo/* \
   ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/topo/hlsTopo
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

copyLegal "awips2/edex/data/hdf5/topo"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Topo Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"
echo -e "\e[1;34m         Destination = ${RPM_INSTALL_PREFIX}/data/hdf5/topo\e[m"
echo ""

%post
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Topo Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Topo Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(664,awips,fxalpha,775)
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/data
%dir /awips2/edex/data/hdf5
%dir /awips2/edex/data/hdf5/topo
/awips2/edex/data/hdf5/topo/*
%docdir /awips2/edex/data/hdf5/topo/licenses