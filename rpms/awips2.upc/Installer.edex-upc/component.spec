#
# AWIPS II UPC Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-edex-upc
Summary: AWIPS II UPC EDEX Tools
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Unidata
Packager: Michael James

AutoReq: no
Requires: awips2-tools, awips2-edex-configuration, awips2-database-standalone-configuration
Requires: awips2-postgresql, awips2-ldm
Requires: ksh, libstdc++(x86-32), libgfortran(x86-32)

obsoletes: awips2-hydroapps-shared < 15.1.4
obsoletes: awips2-edex-hydro < 15.1.4
provides: awips2-edex-upc

%description
AWIPS II UPC EDEX 

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

%install
# create build root directory
rm -rf ${RPM_BUILD_ROOT}/awips2
mkdir -p ${RPM_BUILD_ROOT}/awips2/tools/bin
/bin/cp -r %{_baseline_workspace}/rpms/awips2.upc/Installer.edex-upc/programs/* ${RPM_BUILD_ROOT}/awips2/tools/bin/

%pre

%post
/awips2/tools/bin/edex setup

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}/*

%files
%attr(755,awips,fxalpha) /awips2/tools/bin/edex
%attr(755,awips,fxalpha) /awips2/tools/bin/arinfo
%attr(755,awips,fxalpha) /awips2/tools/bin/areas
%attr(755,awips,fxalpha) /awips2/tools/bin/wgrib
%attr(755,awips,fxalpha) /awips2/tools/bin/wgrib2
%attr(644,awips,fxalpha) /awips2/tools/bin/sat/imgtyp.tbl
