#
# AWIPS II EDEX Spec File
#
Name: awips2-edex
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}
Requires: awips2
Requires: awips2-python
Requires: awips2-java
Requires: awips2-psql
Requires: awips2-yajsw
Requires: awips2-watchdog
requires: awips2-java-security

%description
AWIPS II Edex Installation - Installs and configures AWIPS II Edex.

# disable python byte-compile
%global _python_bytecompile_extra 0
# disable jar repacking
%global __jar_repack 0

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "ERROR: The RPM Build Root has not been specified."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm --recursive --force %{_build_root}
fi

%build

%install
mkdir --parents %{_build_root}/awips2/edex
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir --parents %{_build_root}/awips2/edex/bin
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir --parents %{_build_root}/awips2/etc
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir --parents %{_build_root}/%{_unitdir}/
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir --parents %{_build_root}/etc/watchdog.d
if [ $? -ne 0 ]; then
   exit 1
fi

mkdir --parents %{_build_root}/awips2/edex/data/ndm

if [ $? -ne 0 ]; then

   exit 1

fi

mkdir --parents %{_build_root}/awips2/dev

if [ $? -ne 0 ]; then

   exit 1

fi

mkdir --parents %{_build_root}/awips2/dev/logs
if [ $? -ne 0 ]; then

   exit 1

fi


/bin/cp --recursive %{_baseline_workspace}/rpms/awips2.edex/Installer.edex/ndm/* ${RPM_BUILD_ROOT}/awips2/edex/data/ndm/

/bin/cp --recursive %{_baseline_workspace}/rpms/awips2.edex/Installer.edex/programs/updateNDM.pl ${RPM_BUILD_ROOT}/awips2/dev/

/bin/cp --recursive %{_baseline_workspace}/rpms/awips2.edex/Installer.edex/programs/logs ${RPM_BUILD_ROOT}/awips2/dev/

# remove any .gitignore files
# currently, the ebxml webapp includes a .gitignore file

/usr/bin/find ${RPM_BUILD_ROOT}/awips2/edex -name .gitignore -exec rm -f {} \;
if [ $? -ne 0 ]; then
   exit 1
fi


# copy the service script
INSTALLER_RPM="%{_baseline_workspace}/rpms"
EDEX_BASE="${INSTALLER_RPM}/awips2.edex/Installer.edex"
cp --verbose ${EDEX_BASE}/scripts/edex_camel@.service \
   %{_build_root}/%{_unitdir}/
if [ $? -ne 0 ]; then
   exit 1
fi

# copy the watchdog test/repair scripts
cp --verbose ${EDEX_BASE}/scripts/*watchdog.sh \
   %{_build_root}/etc/watchdog.d
if [ $? -ne 0 ]; then
   exit 1
fi

# copy versions.sh.
UTILITY="${INSTALLER_RPM}/utility"
cp --verbose ${UTILITY}/scripts/versions.sh \
   %{_build_root}/awips2/edex/bin
if [ $? -ne 0 ]; then
   exit 1
fi

#create a list of all files packaged for /awips2/edex/data/utility
UTILITY=/awips2/edex/data/utility
if [ -d %{_build_root}/$UTILITY ]; then
   cd %{_build_root}/$UTILITY
   find . -type f > %{_build_root}/awips2/edex/etc/util_filelist.%{name}.txt
fi

%pre

%post

# Change date stamp of utility files.
# This step is necessary because certain parts of the system (namely GFE) rely
# on modification time to determine if a localization file has changed and
# needs to be reloaded.
UTILITY=/awips2/edex/data/utility
UTIL_FILENAME=/awips2/edex/etc/util_filelist.%{name}.txt
if [ -d $UTILITY ] && [ -f $UTIL_FILENAME ]; then
   while read fileName
   do
      touch "$UTILITY/$fileName"
   done < $UTIL_FILENAME
   rm --force $UTIL_FILENAME
fi

%preun

%postun

%clean
rm --recursive --force %{_build_root}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/edex

%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*.sh
%dir /awips2/edex/data/ndm
/awips2/edex/data/ndm/*

%attr(644,root,root) %{_unitdir}/edex_camel@.service
%attr(744,root,root) /etc/watchdog.d/edex_camel_watchdog.sh

%attr(755,awips,fxalpha) /awips2/dev/updateNDM.pl
%attr(755,awips,fxalpha) /awips2/dev/logs/updateNDM.log
