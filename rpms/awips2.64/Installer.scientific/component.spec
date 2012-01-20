%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%define _python_pkgs_dir "%{_baseline_workspace}/pythonPackages"

#
# AWIPS II Python scientific Spec File
#
Name: awips2-python-scientific
Summary: AWIPS II Python scientific Distribution - 64 Bit
Version: 2.8
Release: 1
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
requires: awips2-python
provides: awips2-python-scientific

%description
AWIPS II Python scientific Site-Package - 64-bit.

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

%build
SCIENTIFIC_SRC_DIR="%{_python_pkgs_dir}/scientific"

cp -rv ${SCIENTIFIC_SRC_DIR}/* \
   %{_build_root}/build-python
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

pushd . > /dev/null
cd %{_build_root}/build-python
export LD_LIBRARY_PATH=/awips2/python/lib
/awips2/python/bin/python setup.py build
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

%install

pushd . > /dev/null
cd %{_build_root}/build-python
export LD_LIBRARY_PATH=/awips2/python/lib
/awips2/python/bin/python setup.py install \
   --root=%{_build_root} \
   --prefix=/awips2/python
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

rm -rf %{_build_root}/build-python

%clean
rm -rf %{_build_root}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/python/lib/python2.7/site-packages
/awips2/python/lib/python2.7/site-packages/*
%dir /awips2/python/include
/awips2/python/include/*
%defattr(755,awips,fxalpha,755)
%dir /awips2/python/bin
/awips2/python/bin/*
