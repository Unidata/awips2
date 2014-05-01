%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%define _build_arch %(uname -i)
%define _python_build_loc %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
%define _lapack_version 3.4.2

#
# AWIPS II Python Spec File
#
Name: awips2-python
Summary: AWIPS II Python Distribution
Version: 2.7.1
Release: 10.el6
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: %{_build_arch}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-python

%description
AWIPS II Python Distribution - Contains Python V2.7.1 plus modules
required for AWIPS II.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "A Build Root has not been specified."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

rm -rf %{_build_root}
mkdir -p %{_build_root}/awips2/python
if [ -d %{_python_build_loc} ]; then
   rm -rf %{_python_build_loc}
fi
mkdir -p %{_python_build_loc}

%build
PYTHON_TAR="Python-2.7.1.tgz"
PYTHON_SRC_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.python/src"

cp -v ${PYTHON_SRC_DIR}/${PYTHON_TAR} %{_python_build_loc}

pushd . > /dev/null

# Untar the source.
cd %{_python_build_loc}
tar -xf ${PYTHON_TAR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

cd Python-2.7.1

# complete the substitution for python-config
sed -e "s,@EXENAME@,/awips2/python/bin/python," < Misc/python-config.in > Misc/python-config.in.new
if [ $? -ne 0 ]; then
   exit 1
fi
mv -f Misc/python-config.in.new Misc/python-config.in
if [ $? -ne 0 ]; then
   exit 1
fi

./configure --prefix=/awips2/python \
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
   
   cp %{_baseline_workspace}/rpms/legal/license.txt \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/rpms/legal/Master Rights File.pdf" \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses    
}
pushd . > /dev/null

cd %{_python_build_loc}/Python-2.7.1
make install prefix=%{_build_root}/awips2/python
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
mkdir -p %{_build_root}/etc/profile.d
PYTHON_PROJECT_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.python"
PYTHON_SRC_DIR="${PYTHON_PROJECT_DIR}/src"
PYTHON_SCRIPTS_DIR="${PYTHON_PROJECT_DIR}/scripts"
PYTHON_PROFILED_DIR="${PYTHON_SCRIPTS_DIR}/profile.d"
cp -v ${PYTHON_PROFILED_DIR}/* %{_build_root}/etc/profile.d
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

# The external libraries (hdf5, netcdf, ...) and headers
# we include with python.

# Retrieve hdf5 from: hdf5-1.8.4-patch1-linux-?-shared.tar.gz
HDF5184_PATTERN="hdf5-1.8.4-patch1-linux*-shared.tar.gz"
pushd . > /dev/null
cd ${PYTHON_SRC_DIR}/%{_build_arch}
HDF5_TAR=`ls -1 ${HDF5184_PATTERN}`
popd > /dev/null

# Copy the hdf5 tar file to our build directory.
cp -v ${PYTHON_SRC_DIR}/%{_build_arch}/${HDF5_TAR} \
   %{_python_build_loc}
if [ $? -ne 0 ]; then
   exit 1
fi
pushd . > /dev/null
cd %{_python_build_loc}
tar -xvf ${HDF5_TAR}
if [ $? -ne 0 ]; then
   exit 1
fi

# Determine what the hdf5 directory is.
HDF_ROOT_DIR=`/bin/tar -tf ${HDF5_TAR} | head -n 1`
rm -fv ${HDF5_TAR}

cp -v ${HDF_ROOT_DIR}lib/* \
   %{_build_root}/awips2/python/lib
if [ $? -ne 0 ]; then
   exit 1
fi

popd > /dev/null

PYTHON_PROJECT_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.python"
PYTHON_SRC_DIR="${PYTHON_PROJECT_DIR}/src"
PYTHON_NATIVE_DIR="${PYTHON_PROJECT_DIR}/nativeLib"
LAPACK_TAR="lapack-%{_lapack_version}.tgz"
LAPACK_PATCH="lapack.patch1"

# The Raytheon-built native (nativeLib) libraries.
cp -vP ${PYTHON_NATIVE_DIR}/%{_build_arch}/grib2.so \
       ${PYTHON_NATIVE_DIR}/%{_build_arch}/gridslice.so \
       %{_build_root}/awips2/python/lib/python2.7
if [ $? -ne 0 ]; then
   exit 1
fi
cp -vP ${PYTHON_NATIVE_DIR}/%{_build_arch}/libjasper.so \
       ${PYTHON_NATIVE_DIR}/%{_build_arch}/libjasper.so.1 \
       ${PYTHON_NATIVE_DIR}/%{_build_arch}/libjasper.so.1.0.0 \
       %{_build_root}/awips2/python/lib
if [ $? -ne 0 ]; then
   exit 1
fi

# An additional step for 32-bit rpms (for now).
if [ "%{_build_arch}" = "i386" ]; then
   /bin/tar -xvf ${PYTHON_SRC_DIR}/i386/awips2-python.tar \
      -C %{_build_root}/awips2/python
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

# Copy the LAPACK tar file and patch to our build directory.
cp -v ${PYTHON_SRC_DIR}/${LAPACK_TAR} \
   %{_python_build_loc}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
cp -v ${PYTHON_SRC_DIR}/${LAPACK_PATCH} \
   %{_python_build_loc}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
pushd . > /dev/null
cd %{_python_build_loc}
tar -xvf ${LAPACK_TAR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
rm -fv ${LAPACK_TAR}
if [ ! -d lapack-%{_lapack_version} ]; then
   file lapack-%{_lapack_version}
   exit 1
fi
patch -p1 -i ${LAPACK_PATCH}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
cd lapack-%{_lapack_version}
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

copyLegal "awips2/python"

%clean
rm -rf %{_build_root}
rm -rf %{_python_build_loc}

%files
%defattr(644,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2Python.csh
%attr(755,root,root) /etc/profile.d/awips2Python.sh
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
