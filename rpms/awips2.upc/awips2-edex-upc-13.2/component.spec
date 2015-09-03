#
# AWIPS II UPC Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-edex-upc
Summary: AWIPS II UPC EDEX Tools
Version: 13.2.1
Release: 2
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Unidata
Packager: Michael James

AutoReq: no
PreReq: awips2-edex-configuration
provides: awips2-edex-upc

%description
AWIPS II UPC EDEX Tools

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

if rpm -q awips2-edex-upc
then
   echo "ERROR: the awips2-edex-upc rpm must not be built"
   echo "       on a machine with an awips2-edex-upc"
   echo "       installation."
   echo "Unable To Continue ... Terminating."
fi

%build
# create build root directory
if [ -d ${RPM_BUILD_ROOT}/etc/init.d]; then
   rm -rf ${RPM_BUILD_ROOT}/etc/init.d
fi
if [ -d ${RPM_BUILD_ROOT}/awips2]; then
   rm -rf ${RPM_BUILD_ROOT}/awips2
fi   
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data/utility/edex_static/base/distribution
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/lib/plugins
mkdir -p ${RPM_BUILD_ROOT}/awips2/tools/bin
mkdir -p ${RPM_BUILD_ROOT}/etc/init.d

%install
export WORKSPACE_DIR=/opt/mj/rpmbuild/awips2-edex-upc
cp ${WORKSPACE_DIR}/edex ${RPM_BUILD_ROOT}/awips2/tools/bin/
cp ${WORKSPACE_DIR}/edex_camel.upc ${RPM_BUILD_ROOT}/etc/init.d/
cp ${WORKSPACE_DIR}/upc.grib.jar ${RPM_BUILD_ROOT}/awips2/edex/lib/plugins/
cp ${WORKSPACE_DIR}/upc.radar.jar ${RPM_BUILD_ROOT}/awips2/edex/lib/plugins/
cp ${WORKSPACE_DIR}/upc.grib.xml ${RPM_BUILD_ROOT}/awips2/edex/data/utility/edex_static/base/distribution/

%pre
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II UPC EDEX Tools...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%post
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II UPC EDEX Tools Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
mv /etc/init.d/edex_camel.upc /etc/init.d/edex_camel
mv /awips2/edex/lib/plugins/upc.grib.jar /awips2/edex/lib/plugins/com.raytheon.edex.plugin.grib.jar
mv /awips2/edex/lib/plugins/upc.radar.jar /awips2/edex/lib/plugins/com.raytheon.edex.plugin.radar.jar
mv /awips2/edex/data/utility/edex_static/base/distribution/upc.grib.xml /awips2/edex/data/utility/edex_static/base/distribution/grib.xml

%postun
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II UPC EDEX Tools Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}/*

%files
%attr(755,awips,fxalpha) /awips2/tools/bin/edex
%attr(755,root,root) /etc/init.d/edex_camel.upc
%attr(644,awips,fxalpha) /awips2/edex/lib/plugins/upc.grib.jar
%attr(644,awips,fxalpha) /awips2/edex/lib/plugins/upc.radar.jar
%attr(644,awips,fxalpha) /awips2/edex/data/utility/edex_static/base/distribution/upc.grib.xml
