#
# AWIPS II Edex "component" spec file
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: awips2-edex-datadelivery
Summary: awips2-edex-datadelivery Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

provides: awips2-edex-datadelivery
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
# prepare the init.d directory path
mkdir -p %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi

unzip %{_baseline_workspace}/build.edex/edex/dist/edex-datadelivery.zip \
   -d %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

# include the init.d script
INSTALLER_RPM="%{_baseline_workspace}/rpms"
EDEX_DATADELIVERY="${INSTALLER_RPM}/awips2.edex/Installer.edex-datadelivery"
cp -v ${EDEX_DATADELIVERY}/scripts/init.d/* \
   %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi
# rename the script to prevent naming conflicts during installation
pushd . > /dev/null 2>&1
cd %{_build_root}/etc/init.d
mv edexServiceList edexServiceList-datadelivery
popd > /dev/null 2>&1

#add central registry script
mkdir -p %{_build_root}/awips2/edex/bin/
if [ $? -ne 0 ]; then
   exit 1
fi

cp -v %{_baseline_workspace}/deploy.edex-Data_Delivery/esb/bin/centralRegistryProviderCredentials.sh %{_build_root}/awips2/edex/bin/
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
# replace the service list script with the datadelivery service list script
if [ -f /etc/init.d/edexServiceList ]; then
   mv /etc/init.d/edexServiceList /etc/init.d/edexServiceList.orig
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
cp /etc/init.d/edexServiceList-datadelivery /etc/init.d/edexServiceList
if [ $? -ne 0 ]; then
   exit 1
fi

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
if [ "${1}" = "1" ]; then
   exit 0
fi
# restore the original service list script with the datadelivery service list script
if [ -f /etc/init.d/edexServiceList.orig ]; then
   mv /etc/init.d/edexServiceList.orig /etc/init.d/edexServiceList
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
/awips2/edex/*
%dir /awips2/edex/bin
%attr(744, -, -) /awips2/edex/bin/centralRegistryProviderCredentials.sh

%attr(744,root,root) /etc/init.d/*
