#
# AWIPS II EDEX Hazard Services spec file
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: awips2-%{_component_name}
Summary: awips2-%{_component_name} Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon

provides: awips2-%{_component_name}
requires: awips2
requires: awips2-edex-base
requires: awips2-python
requires: awips2-java
requires: awips2-psql

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

#create the edex scripts dir
EDEX_SCRIPTS_DIR=%{_build_root}/awips2/edex/scripts/
if [ ! -d $EDEX_SCRIPTS_DIR ]; then
mkdir -p $EDEX_SCRIPTS_DIR
fi

if [ $? -ne 0 ]; then
   exit 1
fi

# verify HazardServices directory exists and copy in files 
HS_NAME=HazardServices
TOOLS_HS_DIR=%{_baseline_workspace}/tools/$HS_NAME
if [ -d $TOOLS_HS_DIR ]; then
   cp -Rv $TOOLS_HS_DIR $EDEX_SCRIPTS_DIR
fi

# HazardServices dir may not be available, as tools/HazardServices may not exist
# if not available, create the directory for other scripts
if [ ! -d $EDEX_SCRIPTS_DIR/$HS_NAME ]; then
   mkdir -p $EDEX_SCRIPTS_DIR/$HS_NAME
fi

if [ $? -ne 0 ]; then
   exit 1
fi

#copy in specific files for HS
if [ -d $EDEX_SCRIPTS_DIR/$HS_NAME ]; then 
   cp -v %{_baseline_workspace}/tools/parseWarngenTemplate.py $EDEX_SCRIPTS_DIR/$HS_NAME
   cp -v %{_baseline_workspace}/tools/ingestshapefiles.sh $EDEX_SCRIPTS_DIR/$HS_NAME
fi

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
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/conf
/awips2/edex/*
%defattr(755,awips,fxalpha,-)
/awips2/edex/scripts/*
