%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%define _build_arch %(uname -i)

#
# AWIPS II Python Spec File
#
Name: awips2-python
Summary: AWIPS II Python Distribution
Version: 2.7.1
Release: 8
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
mkdir -p %{_build_root}/build-python
mkdir -p %{_build_root}/awips2/python
mkdir -p %{_build_root}/etc/profile.d

%build
PYTHON_TAR="Python-2.7.1.tgz"
PYTHON_SRC_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.python/src"

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
   
   cp %{_baseline_workspace}/rpms/legal/license.txt \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/rpms/legal/Master Rights File.pdf" \
      %{_build_root}/${COMPONENT_BUILD_DIR}/licenses    
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

pushd .
cd %{_build_root}/awips2/python/lib/pkgconfig
# Alter the prefix in: lib/pkgconfig/python-2.7.pc
/bin/sed -i '1c\'"prefix=/awips2/python" python-2.7.pc
if [ $? -ne 0 ]; then
   exit 1
fi

cd %{_build_root}/awips2/python/bin
# Update the first line of: 2to3, idle, pydoc, python2.7-config, smtpd.py
/bin/sed -i '1c\'"#!/awips2/python/bin/python2.7" 2to3
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/sed -i '1c\'"#!/awips2/python/bin/python2.7" idle
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/sed -i '1c\'"#!/awips2/python/bin/python2.7" pydoc
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/sed -i '1c\'"#!/awips2/python/bin/python2.7" python2.7-config
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/sed -i '1c\'"#!/awips2/python/bin/python2.7" smtpd.py
if [ $? -ne 0 ]; then
   exit 1
fi

popd

# Our profile.d scripts.
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
   %{_build_root}/build-python
if [ $? -ne 0 ]; then
   exit 1
fi
pushd . > /dev/null
cd %{_build_root}/build-python
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
LAPACK_TAR="lapack-3.1.1.tgz"
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
