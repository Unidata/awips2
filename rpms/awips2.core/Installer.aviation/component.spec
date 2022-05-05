#
# AWIPS II Hydroapps Spec File
#
Name: awips2-aviation-shared
Summary: AWIPS II Aviation Distribution
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
Provides: awips2-aviation-shared
Requires: awips2-edex
Requires: awips2-edex-base

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

%build

%install
AVIATION_SRC_DIR="com.raytheon.viz.avnconfig/localization/aviation/thresholds"
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/share/aviation/tmp
if [ $? -ne 0 ]; then
   exit 1
fi


# Copy the aviation *ish files to their destination.
cp %{_baseline_workspace}/${AVIATION_SRC_DIR}/*.txt \
   ${RPM_BUILD_ROOT}/awips2/edex/data/share/aviation
if [ $? -ne 0 ]; then
   exit 1
fi
   
%pre
%post
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(760,awips,fxalpha,770)
%dir /awips2/edex/data/share/aviation
%dir /awips2/edex/data/share/aviation/tmp
%defattr(760,awips,fxalpha,770)
/awips2/edex/data/share/aviation/*.txt
