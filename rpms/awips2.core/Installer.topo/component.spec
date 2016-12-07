#
# AWIPS II Topo Spec File
#
Name: awips2-data.hdf5-topo
Summary: AWIPS II Topo Distribution
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

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

TOPO_DIR="%{_static_files}/topo/"
if [ ! -d ${TOPO_DIR} ]; then
   echo "Directory ${TOPO_DIR} not found!"
   exit 1
fi
#defaultTopo.h5  gtopo30.h5  modelStaticTopo.h5  staticTopo.h5  usTopo.dat.gz
TOPO_TO_COPY=\
(\
   'defaultTopo.h5'\
   'modelStaticTopo.h5'\
   'staticTopo.h5'\
   'gtopo30.h5'\
   'usTopo.dat.gz' \
)
# !!!!! WARNING - THIS WILL TAKE A LONG TIME !!!!!

for topoFile in ${TOPO_TO_COPY[*]};
do
   cp -Pp ${TOPO_DIR}/${topoFile} \
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
%dir /awips2/edex/data/hdf5
%dir /awips2/edex/data/hdf5/topo
/awips2/edex/data/hdf5/topo/*
%docdir /awips2/edex/data/hdf5/topo/licenses
# rpm -qlp ./noarch/awips2-data.hdf5-topo-16.2.2-2.el7.centos.noarch.rpm
#/awips2/edex/data/hdf5
#/awips2/edex/data/hdf5/topo
#/awips2/edex/data/hdf5/topo/defaultTopo.h5
#/awips2/edex/data/hdf5/topo/gtopo30.h5
#/awips2/edex/data/hdf5/topo/usTopo.dat.gz
