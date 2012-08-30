%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

#
# AWIPS II Python Spec File
#
Name: awips2-python
Summary: AWIPS II Python Distribution - 64 Bit
Version: 2.7.1
Release: 3
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-python

%description
AWIPS II Python (64 Bit) Distribution - Contains Python V2.7.1 plus modules
required for AWIPS II.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

rm -rf %{_build_root}
mkdir -p %{_build_root}/build-python
mkdir -p %{_build_root}/awips2/python
mkdir -p %{_build_root}/etc/profile.d

%build
# We can use the same source that is in the 32-bit python directory.
PYTHON_TAR="Python-2.7.1.tgz"
PYTHON_SRC_DIR="%{_baseline_workspace}/Installer.rpm/awips2.core/Installer.python/src"

cp -v ${PYTHON_SRC_DIR}/${PYTHON_TAR} \
   %{_build_root}/build-python

pushd . > /dev/null

# Untar the source.
cd %{_build_root}/build-python
tar -xf ${PYTHON_TAR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

cd Python-2.7.1
./configure --prefix=%{_build_root}/awips2/python \
   --enable-shared
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

make clean
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
make
if [ ${RC} -ne 0 ]; then
   exit 1
fi

popd > /dev/null

%install
# Copies the standard Raytheon licenses into a license directory for the
# current component.
function copyLegal()
{
   # $1 == Component Build Root
   
   COMPONENT_BUILD_DIR=${1}
   
   mkdir -p %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
   
   # Create a Tar file with our FOSS licenses.
   tar -cjf %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses.tar \
      %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses/
   
   cp %{_baseline_workspace}/Installer.rpm/legal/license.txt \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/Installer.rpm/legal/Master Rights File.pdf" \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses.tar \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/Installer.rpm/legal/FOSS_licenses.tar    
}
pushd . > /dev/null

cd %{_build_root}/build-python/Python-2.7.1
make install
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

popd > /dev/null

RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

# Our profile.d scripts.
PYTHON_PROJECT_DIR="%{_baseline_workspace}/Installer.rpm/awips2.64/Installer.python"
PYTHON_SCRIPTS_DIR="${PYTHON_PROJECT_DIR}/scripts"
PYTHON_PROFILED_DIR="${PYTHON_SCRIPTS_DIR}/profile.d"
cp -v ${PYTHON_PROFILED_DIR}/* %{_build_root}/etc/profile.d
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

# The external libraries (hdf5, netcdf, ...) and headers
# we include with python.

# Retrieve hdf5 from: hdf5-1.8.4-patch1-linux-x86_64-shared.tar.gz
HDF5_PREREQS_DIR="%{_baseline_workspace}/Installer.rpm/awips2.64/deploy.builder/pre-reqs"
HDF5_TAR="hdf5-1.8.4-patch1-linux-x86_64-shared.tar.gz"

# Copy the hdf5 tar file to our build directory.
cp -v ${HDF5_PREREQS_DIR}/${HDF5_TAR} \
   %{_build_root}/build-python
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
pushd . > /dev/null
cd %{_build_root}/build-python
tar -xvf ${HDF5_TAR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
rm -fv ${HDF5_TAR}
if [ ! -d hdf5-1.8.4-patch1-linux-x86_64-shared ]; then
   file hdf5-1.8.4-patch1-linux-x86_64-shared
   exit 1
fi
cp -v hdf5-1.8.4-patch1-linux-x86_64-shared/lib/* \
   %{_build_root}/awips2/python/lib
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

# The Raytheon native libraries.
PYTHON_NATIVE_DIR="%{_baseline_workspace}/Installer.rpm/awips2.64/Installer.python/native"
GRIDSLICE_SO="${PYTHON_NATIVE_DIR}/gridslice.so"
if [ ! -f ${GRIDSLICE_SO} ]; then
   echo "ERROR: ${GRIDSLICE_SO} does not exist."
   exit 1
fi
cp -v ${GRIDSLICE_SO} \
   %{_build_root}/awips2/python/lib/python2.7
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

popd > /dev/null

PYTHON_SRC_DIR="${PYTHON_PROJECT_DIR}/src"
LAPACK_TAR="lapack-3.1.1.tgz"
LAPACK_PATCH="lapack.patch1"

# Copy the LAPACK tar file and patch to our build directory.
cp -v ${PYTHON_SRC_DIR}/${LAPACK_TAR} \
   %{_build_root}/build-python
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
cp -v ${PYTHON_SRC_DIR}/${LAPACK_PATCH} \
   %{_build_root}/build-python
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
pushd . > /dev/null
cd %{_build_root}/build-python
tar -xvf ${LAPACK_TAR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
rm -fv ${LAPACK_TAR}
if [ ! -d lapack-3.1.1 ]; then
   file lapack-3.1.1
   exit 1
fi
cd lapack-3.1.1
patch -p1 -i ../${LAPACK_PATCH}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
mv make.inc.example make.inc
if [ $? -ne 0 ]; then
   exit 1
fi
make blaslib
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
make lapacklib
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
# Copy the libraries that we just built to
# the python lib directory.
if [ ! -f BLAS/SRC/libblas.so ]; then
   file BLAS/SRC/libblas.so
   exit 1
fi
cp -v BLAS/SRC/libblas.so \
   %{_build_root}/awips2/python/lib
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
if [ ! -f SRC/liblapack.so ]; then
   file SRC/liblapack.so
   exit 1
fi
cp -v SRC/liblapack.so \
   %{_build_root}/awips2/python/lib
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

rm -rf %{_build_root}/build-python
copyLegal "awips2/python"

%clean
rm -rf %{_build_root}

%files
%defattr(644,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2Python64.csh
%attr(755,root,root) /etc/profile.d/awips2Python64.sh
%dir /awips2/python
%dir /awips2/python/lib
/awips2/python/lib/*
%docdir /awips2/python/licenses
%dir /awips2/python/licenses
/awips2/python/licenses/*
%dir /awips2/python/share
/awips2/python/share/*
%defattr(755,awips,fxalpha,755)
%dir /awips2/python/include
/awips2/python/include/*
%dir /awips2/python/bin
/awips2/python/bin/*
