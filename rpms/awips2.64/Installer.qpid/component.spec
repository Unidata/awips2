%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%define _python_pkgs_dir "%{_baseline_workspace}/pythonPackages"

#
# AWIPS II Python qpid Spec File
#
Name: awips2-python-qpid
Summary: AWIPS II Python qpid Distribution - 64 Bit
Version: 0.6
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
provides: awips2-python-qpid

%description
AWIPS II Python qpid Site-Package - 64-bit.

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
QPID_SRC_DIR="%{_python_pkgs_dir}/qpid"
QPID_TAR="qpid-0.6.tar.gz"

cp -rv ${QPID_SRC_DIR}/${QPID_TAR} \
   %{_build_root}/build-python
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
      
pushd . > /dev/null
cd %{_build_root}/build-python
tar -xvf ${QPID_TAR}
rm -f ${QPID_TAR}
popd > /dev/null

%install
QPID_SRC_DIR="%{_python_pkgs_dir}/qpid"
QPID_SRC="qpid-0.6/python"
QPID_SPECS="qpid-0.6/specs"
QPID_QUEUE_COUNT_SCRIPT="qpid-queue-count"

pushd . > /dev/null
cd %{_build_root}/build-python/${QPID_SRC}
mkdir -p %{_build_root}/awips2/python
export LD_LIBRARY_PATH=/awips2/python/lib
export PATH=/awips2/python/bin:${PATH}
make install PREFIX=%{_build_root}/awips2/python
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

# Copy the queue-counting script to bin
cp -v ${QPID_SRC_DIR}/bin/${QPID_QUEUE_COUNT_SCRIPT} \
   %{_build_root}/awips2/python/bin
   
# Copy the amqp dtd/xml data to share
mkdir -p %{_build_root}/awips2/python/share/amqp
cp -prv %{_build_root}/build-python/${QPID_SPECS} \
   %{_build_root}/awips2/python/share/amqp
   
rm -rf %{_build_root}/build-python

%pre

%post

%preun

%postun

%clean
rm -rf %{_build_root}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/python/lib/python2.7/site-packages
/awips2/python/lib/python2.7/site-packages/*
%dir /awips2/python/share
/awips2/python/share/*
%defattr(755,awips,fxalpha,755)
%dir /awips2/python/bin
/awips2/python/bin/*
