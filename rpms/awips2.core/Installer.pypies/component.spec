#
# AWIPS II PyPIES Spec File
#
Name: awips2-pypies
Summary: AWIPS II PyPIES
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: awips2-pypies
Requires: awips2-python >= 2.7-1
Requires: awips2-tools

%description
AWIPS II PyPIES Installation - Sets Up AWIPS II PyPIES.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

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
mkdir -p ${RPM_BUILD_ROOT}/${PYPIES_PY_DEST_DIR}
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/python/lib/python2.7/site-packages/pypies
if [ $? -ne 0 ]; then
   exit 1
fi

# Copy The PyPIES modules To The appropriate directory.
PYPIES_FILE_SRC_DIR="pythonPackages/pypies"
cp -r %{_baseline_workspace}/${PYPIES_FILE_SRC_DIR}/pypies/* \
   ${RPM_BUILD_ROOT}/awips2/python/lib/python2.7/site-packages/pypies
 
# copy pypies.cfg
mkdir -p ${RPM_BUILD_ROOT}/awips2/pypies/conf
cp %{_baseline_workspace}/${PYPIES_FILE_SRC_DIR}/pypies.cfg \
	${RPM_BUILD_ROOT}/awips2/pypies/conf/pypies.cfg
	
# create the pypies logging directory
mkdir -p ${RPM_BUILD_ROOT}/awips2/pypies/logs

%pre
%post
%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/pypies
/awips2/pypies/*
%dir /awips2/python/lib/python2.7/site-packages/pypies
/awips2/python/lib/python2.7/site-packages/pypies/*
