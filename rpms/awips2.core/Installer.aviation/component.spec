#
# AWIPS II Hydroapps Spec File
#
Name: awips2-aviation-shared
Summary: AWIPS II Aviation Distribution
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-aviation-shared
requires: awips2-edex-base

%description
AWIPS II Aviation Distribution - includes *-ish files required by AvnFPS.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/share/aviation
# Create an empty 'tmp' directory.
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/share/aviation/tmp

%build

%install
AVIATION_SRC_DIR="build/static/common/cave/etc/aviation/thresholds"

# Copy the aviation *ish files to their destination.
cp %{_baseline_workspace}/${AVIATION_SRC_DIR}/*.txt \
   ${RPM_BUILD_ROOT}/awips2/edex/data/share/aviation
   
%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Aviation Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = /awips2\e[m"
echo -e "\e[1;34m         Destination = /awips2/edex/data/share/aviation\e[m"

%post
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Aviation Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Aviation Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/data
%dir /awips2/edex/data/share
%dir /awips2/edex/data/share/aviation
%dir /awips2/edex/data/share/aviation/tmp
%defattr(664,awips,fxalpha,755)
/awips2/edex/data/share/aviation/*.txt