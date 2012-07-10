#
# AWIPS II PyPIES Spec File
#
Name: awips2-pypies
Summary: AWIPS II PyPIES
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Nate Jensen

AutoReq: no
provides: awips2-pypies
requires: awips2-python >= 2.7-1
requires: awips2-httpd-pypies
requires: awips2-tools

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

mkdir -p ${RPM_BUILD_ROOT}/${PYPIES_PY_DEST_DIR}
mkdir -p ${RPM_BUILD_ROOT}/awips2/python/lib/python2.7/site-packages/pypies

%build

%install

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
if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II PyPIES...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = /awips2/pypies\e[m"

%post
# Determine the location of awips2-python
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II PyPIES Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II PyPIES Installation Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo ""

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/pypies
/awips2/pypies/*
/awips2/python/lib/python2.7/site-packages/*