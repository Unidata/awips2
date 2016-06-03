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

%build

%install
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/topo
if [ $? -ne 0 ]; then
   exit 1
fi

TOPO_SRC_DIR="awips2-static/topo/"
if [ ! -d %{_awipscm_share}/${TOPO_SRC_DIR} ]; then
   file %{_awipscm_share}/${TOPO_SRC_DIR}
   exit 1
fi

TOPO_TO_COPY=\
(\
   'gtopo30.h5'\
   'defaultTopo.h5' \
   'usTopo.dat.gz' \
)

for topoFile in ${TOPO_TO_COPY[*]};
do
   cp -Pp %{_awipscm_share}/${TOPO_SRC_DIR}/${topoFile} \
      ${RPM_BUILD_ROOT}/awips2/edex/data/hdf5/topo
   if [ $? -ne 0 ]; then
      exit 1
   fi
done

%pre
%post
%preun
%postun

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
