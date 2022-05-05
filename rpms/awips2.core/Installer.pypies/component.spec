%define _installed_python %(if [ -f /awips2/python/bin/python ]; then /awips2/python/bin/python -c 'import sys; print(".".join(map(str, sys.version_info[:3])))'; else echo 0; fi)
%define _installed_python_short %(if [ -f /awips2/python/bin/python ]; then /awips2/python/bin/python -c 'import sys; print(".".join(map(str, sys.version_info[:2])))'; else echo 0; fi)

#
# AWIPS II PyPIES Spec File
#
Name: awips2-pypies
Summary: AWIPS II PyPIES
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
Provides: awips2-pypies
Requires: awips2-python >= %{_installed_python}
Requires: awips2-hdf5

%description
AWIPS II PyPIES Installation - Sets Up AWIPS II PyPIES.

# Change the brp-python-bytecompile script to use the AWIPS2 version of Python. #7237
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's/\/usr\/bin\/python/\/awips2\/python\/bin\/python/g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

%install
mkdir -p ${RPM_BUILD_ROOT}/${PYPIES_PY_DEST_DIR}
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/python/lib/python%{_installed_python_short}/site-packages/pypies
if [ $? -ne 0 ]; then
   exit 1
fi

# Copy The PyPIES modules To The appropriate directory.
PYPIES_FILE_SRC_DIR="pythonPackages/pypies"
cp -r %{_baseline_workspace}/${PYPIES_FILE_SRC_DIR}/pypies/* \
   ${RPM_BUILD_ROOT}/awips2/python/lib/python%{_installed_python_short}/site-packages/pypies
 
# copy pypies.cfg
mkdir -p ${RPM_BUILD_ROOT}/awips2/pypies/conf
cp %{_baseline_workspace}/${PYPIES_FILE_SRC_DIR}/pypies.cfg \
	${RPM_BUILD_ROOT}/awips2/pypies/conf/pypies.cfg
	
# create the pypies logging directory
mkdir -p ${RPM_BUILD_ROOT}/awips2/pypies/logs

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/pypies
/awips2/pypies/*
%dir /awips2/python/lib/python%{_installed_python_short}/site-packages/pypies
/awips2/python/lib/python%{_installed_python_short}/site-packages/pypies/*
