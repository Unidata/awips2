%define _build_arch %(uname -i)
%define _zip_file common-base.zip
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
# disable jar repacking
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

#
# AWIPS II CAVE/EDEX common base Spec File
#
Name: awips2-common-base
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: %{_build_arch}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
Requires: awips2-common-foss
Requires: awips2-edex
Requires: awips2-netcdf
Requires: awips2-netcdf-devel

BuildRequires: awips2-ant
BuildRequires: awips2-java

%description
AWIPS II Common Base - Contains common plugins utilized by EDEX.

%prep
# Ensure that a "buildroot" has been specified.
if [ "%{_build_root}" = "" ]; then
   echo "ERROR: A BuildRoot has not been specified."
   echo "FATAL: Unable to Continue ... Terminating."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
/bin/mkdir -p %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

%build
_build_xml=build.xml
BUILD_EDEX=%{_baseline_workspace}/build.edex
EDEX_DIST=${BUILD_EDEX}/edex/dist

cd ${BUILD_EDEX}
# Build the java.extensions feature first so common.base can include it.
/awips2/ant/bin/ant -f ${_build_xml} \
   -Dfeature=com.raytheon.uf.common.java.extensions.feature \
   -Duframe.eclipse=%{_uframe_eclipse} \
   clean \
   build 
if [ $? -ne 0 ]; then
   exit 1
fi

/awips2/ant/bin/ant -f ${_build_xml} \
   -Dfeature=com.raytheon.uf.common.base.feature \
   -Duframe.eclipse=%{_uframe_eclipse} \
   build \
   clean
if [ $? -ne 0 ]; then
   exit 1
fi

%install
BUILD_EDEX=%{_baseline_workspace}/build.edex
EDEX_DIST=${BUILD_EDEX}/edex/dist

/usr/bin/unzip ${EDEX_DIST}/common-base.zip -d %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

#create a list of all files packaged for /awips2/edex/data/utility
UTILITY=/awips2/edex/data/utility
if [ -d %{_build_root}/$UTILITY ]; then
   cd %{_build_root}/$UTILITY
   find . -type f > %{_build_root}/awips2/edex/util_filelist.%{name}.txt
fi

/bin/mkdir -p %{_build_root}/awips2/edex/etc
if [ $? -ne 0 ]; then
   exit 1
fi

%post

#change date stamp of utility files
UTILITY=/awips2/edex/data/utility
UTIL_FILENAME=/awips2/edex/util_filelist.%{name}.txt
if [ -d $UTILITY ] && [ -f $UTIL_FILENAME ]; then
  while read fileName
  do
   touch "$UTILITY/$fileName"
  done < $UTIL_FILENAME
  rm -f $UTIL_FILENAME
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
/awips2/edex/conf
/awips2/edex/data
/awips2/edex/etc
/awips2/edex/lib/plugins
/awips2/edex/*.txt

# This does not get its own spec file because it needs to collect files from
# the awips2-common-base build.
%package -n awips2-common-foss

Summary: AWIPS II Common FOSS
Group: AWIPSII
AutoReq: no

%description -n awips2-common-foss
Contains Java FOSS packages used by EDEX

%files -n awips2-common-foss
%defattr(644,awips,fxalpha,755)
/awips2/edex/lib/dependencies
