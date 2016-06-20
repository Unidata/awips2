#
# AWIPS II Edex "component" spec file
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: awips2-%{_component_name}
Summary: awips2-%{_component_name} Installation
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

provides: awips2-%{_component_name}
requires: awips2
requires: awips2-edex-base
requires: awips2-python
requires: awips2-java
requires: awips2-psql

%{?filter_setup:
%filter_from_requires /guava/d; /raytheon/d; /javax/d; /net\.sf/d; /org\.apache/d; /org\.geotools/d; /org\.codehaus/d; /org\.hibernate/d; /org\.quartz/d; /org\.reflections/d; /org\.slf4j/d; /org\.springframework/d; /ucar\.nc2/d; /org\.opensaml/d; /gov\.nasa\.gsfc\.fits/d; /org\.eclipse\.jetty/d; /com\.sun\.xml\.bind/d; /org\.joda\.time/d; /org\.itadaki/d; /org/\.junit/d; /org\.eclipse\.core\.runtime/d
%filter_setup
}

%description
AWIPS II Edex - Installs AWIPS II Edex Plugins.

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

%build

%install
mkdir -p %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

unzip %{_baseline_workspace}/build.edex/edex/dist/%{_component_name}.zip \
   -d %{_build_root}

if [ $? -ne 0 ]; then
   exit 1
fi

#create a list of all files packaged for /awips2/edex/data/utility
UTILITY=/awips2/edex/data/utility
if [ -d %{_build_root}/$UTILITY ]; then
   cd %{_build_root}/$UTILITY
   find . -type f > %{_build_root}/awips2/edex/util_filelist.%{name}.txt
fi

%pre
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

%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/edex
/awips2/edex/*
