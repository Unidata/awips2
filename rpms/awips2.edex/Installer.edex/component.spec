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
Vendor: Raytheon
Packager: %{_build_site}
Requires: awips2
Requires: awips2-python
Requires: awips2-java
Requires: awips2-psql
Requires: awips2-yajsw
Requires: awips2-watchdog

%description
AWIPS II Edex Installation - Installs and configures AWIPS II Edex.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
# disable jar repacking
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

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
mkdir -p %{_build_root}/awips2/edex
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/awips2/edex/bin
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/awips2/etc
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/etc/watchdog.d
if [ $? -ne 0 ]; then
   exit 1
fi

# remove any .gitignore files
# currently, the ebxml webapp includes a .gitignore file
/usr/bin/find ${RPM_BUILD_ROOT}/awips2/edex -name .gitignore -exec rm -f {} \;
if [ $? -ne 0 ]; then
   exit 1
fi

INSTALLER_RPM="%{_baseline_workspace}/rpms"
# copy the service script.
EDEX_BASE="${INSTALLER_RPM}/awips2.edex/Installer.edex"
cp -v ${EDEX_BASE}/scripts/edex_camel \
   %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi

# copy the watchdog test/repair scripts
cp -v ${EDEX_BASE}/scripts/*watchdog.sh \
   %{_build_root}/etc/watchdog.d
if [ $? -ne 0 ]; then
   exit 1
fi

# copy versions.sh.
UTILITY="${INSTALLER_RPM}/utility"
cp -v ${UTILITY}/scripts/versions.sh \
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

#update edexServiceList on install
if [ "${1}" = "1" ]; then

  #add services to the edex service list
  LIST_FILE=/etc/init.d/edexServiceList
  BASE_SERVICES=(ingest ingestDat ingestGrib request)

  if [ -f $LIST_FILE ]; then
     source $LIST_FILE

     for service in ${BASE_SERVICES[*]}; do
        addService=true;
        for index in ${!SERVICES[@]};
        do
           if [ ${SERVICES[$index]} = $service ]; then
              addService=false;
           fi
        done
        if $addService; then
           SERVICES=(${SERVICES[@]} $service)
        fi
     done
  else
     SERVICES=${BASE_SERVICES[@]}
  fi

  echo "#generated on $(date)" > $LIST_FILE
  echo "export SERVICES=(${SERVICES[@]})" >> $LIST_FILE
fi

# We need to create a link to the python shared library if it does not exist.
pushd . > /dev/null 2>&1
if [ -d /awips2/python/lib ]; then
   cd /awips2/python/lib
   if [ -L libpython.so ]; then
      # Ensure that we are pointing to the correct shared library.
      rm -f libpython.so
   fi
      
   if [ -f libpython3.6m.so ]; then
      ln --symbolic libpython3.6m.so libpython.so
   fi
fi
popd > /dev/null 2>&1

if [ -f /etc/init.d/edex_camel ]; then
   /sbin/chkconfig --add edex_camel
   /sbin/chkconfig edex_camel on --level 35
fi

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
   rm -f $UTIL_FILENAME
fi

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi

if [ -f /etc/init.d/edex_camel ]; then
   /sbin/service edex_camel stop > /dev/null 2>&1
   /sbin/chkconfig --del edex_camel
fi

LIST_FILE=/etc/init.d/edexServiceList
if [ -f $LIST_FILE ]; then
   rm -f $LIST_FILE
fi

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/edex

%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*.sh

%attr(744,root,root) /etc/init.d/edex_camel
%attr(744,root,root) /etc/watchdog.d/edex_camel_watchdog.sh
