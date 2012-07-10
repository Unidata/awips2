#
# AWIPS II edex-native Spec File
#
Name: awips2-edex-native
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-edex-native
requires: awips2
requires: awips2-edex-base

%description
AWIPS II Edex Installation - Installs The AWIPS II EDEX Native Libraries.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "ERROR: The RPM Build Root has not been specified."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
mkdir -p %{_build_root}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/awips2/edex/lib/native/linux32/awips1
if [ $? -ne 0 ]; then
   exit 1
fi

%build

%install
DIST_NATIVE="%{_baseline_workspace}/dist.native"
PACKAGES="%{_awipscm_share}/packages"
# extract the native libraries
/bin/tar -xpf ${DIST_NATIVE}/i386-pc-linux-gnu.tar \
   -C %{_build_root}/awips2 ./edex/lib ./edex/bin
if [ $? -ne 0 ]; then
   exit 1
fi
# copy the AWIPS I mhs libraries
cp ${PACKAGES}/mhs/* \
   %{_build_root}/awips2/edex/lib/native/linux32/awips1
if [ $? -ne 0 ]; then
   exit 1
fi

%pre
%post
%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex

%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*
%dir /awips2/edex/lib
/awips2/edex/lib/*
