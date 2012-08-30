%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%define _python_pkgs_dir "%{_baseline_workspace}/pythonPackages"

#
# AWIPS II Python h5py Spec File
#
Name: awips2-python-h5py
Summary: AWIPS II Python h5py Distribution - 64 Bit
Version: 1.3.0
Release: 2
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
requires: awips2-python
requires: awips2-python-numpy
provides: awips2-python-h5py

%description
AWIPS II Python h5py Site-Package - 64-bit.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

rm -rf %{_build_root}
mkdir -p %{_build_root}
mkdir -p %{_build_root}/build-python

PRE_REQS_HDF5_TAR="hdf5-1.8.4-patch1-linux-x86_64-shared.tar.gz"
PRE_REQS_DIR="%{_baseline_workspace}/Installer.rpm/awips2.64/deploy.builder/pre-reqs"
cp -v ${PRE_REQS_DIR}/${PRE_REQS_HDF5_TAR} \
   %{_build_root}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

pushd . > /dev/null
cd %{_build_root}
/bin/tar -xvf ${PRE_REQS_HDF5_TAR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
rm -f ${PRE_REQS_HDF5_TAR}
popd > /dev/null

%build
H5PY_SRC_DIR="%{_python_pkgs_dir}/h5py"
HDF5_PATH="%{_build_root}/hdf5-1.8.4-patch1-linux-x86_64-shared"

cp -rv ${H5PY_SRC_DIR}/* \
   %{_build_root}/build-python
pushd . > /dev/null
cd %{_build_root}/build-python
export LD_LIBRARY_PATH=/awips2/python/lib
/awips2/python/bin/python setup.py build \
   --hdf5=${HDF5_PATH}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

%install
HDF5_PATH="%{_build_root}/hdf5-1.8.4-patch1-linux-x86_64-shared"

pushd . > /dev/null
cd %{_build_root}/build-python
export LD_LIBRARY_PATH=/awips2/python/lib
/awips2/python/bin/python setup.py install \
   --root=%{_build_root} \
   --prefix=/awips2/python
popd > /dev/null

rm -rf ${HDF5_PATH}
rm -rf %{_build_root}/build-python

# patch: copy the szip libraries into the installation.
szip_lib=%{_baseline_workspace}/Installer.rpm/awips2.64/Installer.h5py/lib
cp -vP ${szip_lib}/* \
   %{_build_root}/awips2/python/lib
if [ $? -ne 0 ]; then
   exit 1
fi

%pre

%post

%preun

%postun

%clean
rm -rf %{_build_root}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/python/lib
/awips2/python/lib/*
