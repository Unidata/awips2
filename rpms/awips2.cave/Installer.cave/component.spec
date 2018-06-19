# RPM Metadata
%define _component_project_dir    awips2.cave/Installer.cave
%define _component_zip_file_name  CAVE-linux.gtk.%{_build_arch}.zip
%define _component_desc           "awips2-cave"

%define _swt_version 3.104.1.v20150825-0743
%define _ui_version 3.107.0.v20150507-1945
%define _jface_version 3.11.0.v20150602-1400
#
# awips2-cave Spec File
#
# Note: an upgrade prevention check has been added to the %pre section to ensure that
# this RPM cannot be upgraded. Upgrading this RPM will completely corrupt all of the CAVE
# rcp configuration information. So, a completely re-installation of this RPM and subsequently
# all of CAVE is required whenever an updated version of this RPM is released.
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
# disable jar repacking
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: %{_component_name}
Summary: awips2-cave Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
provides: awips2-cave
provides: awips2-base-component
provides: awips2-base
requires: awips2
requires: awips2-java
requires: awips2-python
requires: openmotif
requires: libMrm.so.4()(64bit)
requires: libXp.so.6()(64bit)
requires: libg2c.so.0()(64bit)

%description
%{_component_desc}

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
fi
CAVE_DIST_DIR="%{_baseline_workspace}/rpms/awips2.cave/setup/dist"

if [ ! -f ${CAVE_DIST_DIR}/%{_component_zip_file_name} ]; then
   echo "ERROR: Unable to find - ${CAVE_DIST_DIR}/%{_component_zip_file_name}."
   exit 1
fi

%build

%install
mkdir -p ${RPM_BUILD_ROOT}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi

CAVE_DIST_DIR="%{_baseline_workspace}/rpms/awips2.cave/setup/dist"

cp ${CAVE_DIST_DIR}/%{_component_zip_file_name} \
   ${RPM_BUILD_ROOT}/awips2

cd ${RPM_BUILD_ROOT}/awips2
unzip %{_component_zip_file_name}
rm -f %{_component_zip_file_name}

mkdir -p ${RPM_BUILD_ROOT}/awips2/cave/etc
if [ $? -ne 0 ]; then
   exit 1
fi

# Relocate any localization files
pushd . > /dev/null 2>&1
cd ${RPM_BUILD_ROOT}/awips2/cave/plugins
for localizationDirectory in `find . -maxdepth 2 -name localization -type d`;
do
   # copy the contents of the localization directory to the
   # etc directory.
   cp -rf ${localizationDirectory}/* ${RPM_BUILD_ROOT}/awips2/cave/etc
   if [ $? -ne 0 ]; then
      exit 1
   fi

   # remove the localization directory.
   rm -rf ${localizationDirectory}
   if [ $? -ne 0 ]; then
      exit 1
   fi
done

# Forcefully unzip: org.eclipse.swt.gtk.linux.x86_64_*.jar
if [ -f org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar ]; then
   mkdir org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}
   unzip -qq org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar \
      -d org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}
   rm -f org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar
   mv org.eclipse.swt.gtk.linux.x86_64_%{_swt_version} \
      org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar
fi

# Forcefully unzip: org.eclipse.ui_*.jar
if [ -f org.eclipse.ui_%{_ui_version}.jar ]; then
   mkdir org.eclipse.ui_%{_ui_version}
   unzip -qq org.eclipse.ui_%{_ui_version}.jar \
      -d org.eclipse.ui_%{_ui_version}
   rm -f org.eclipse.ui_%{_ui_version}.jar
   mv org.eclipse.ui_%{_ui_version} \
      org.eclipse.ui_%{_ui_version}.jar
fi

# Forcefully unzip: org.eclipse.jface_*.jar
if [ -f org.eclipse.jface_%{_jface_version}.jar ]; then
   mkdir org.eclipse.jface_%{_jface_version}
   unzip -qq org.eclipse.jface_%{_jface_version}.jar \
      -d org.eclipse.jface_%{_jface_version}
   rm -f org.eclipse.jface_%{_jface_version}.jar
   mv org.eclipse.jface_%{_jface_version} \
      org.eclipse.jface_%{_jface_version}.jar
fi

# Delete configuration information because it references the jar files that
# were just deleted.
rm -rf ${RPM_BUILD_ROOT}/awips2/cave/configuration/org.eclipse.osgi

popd > /dev/null 2>&1

%pre
if [ "${1}" = "2" ]; then
   echo "The %{_component_name} rpm cannot be upgraded. Re-install CAVE to update to a newer version of this RPM."
   exit 1
fi

# /awips2/cave must not exist.
if [ -d /awips2/cave ]; then
   # TODO: need to make CAVE RPMs do a better job of cleaning up files that they are
   # responsible for.
   echo -e "\e[1;31mERROR: the /awips2/cave directory already exists. /awips2/cave\e[m"
   echo -e "\e[1;31m       must be REMOVED before the installation will proceed.\e[m"
   exit 1
fi

%post
# We need to create a link to the python shared library if it does not exist.
pushd . > /dev/null 2>&1
if [ -d /awips2/python/lib ]; then
   cd /awips2/python/lib
   if [ -L libpython.so ]; then
      # Ensure that we are pointing to the correct shared library.
      rm -f libpython.so
   fi
      
   if [ -f libpython2.7.so.1.0 ]; then
      ln -s libpython2.7.so.1.0 libpython.so
   fi
fi
popd > /dev/null 2>&1

%preun
if [ "${1}" = "1" ]; then
   echo "The %{_component_name} rpm cannot be upgraded. Re-install CAVE to update to a newer version of this RPM."
   exit 1
fi

# Check and remove the cave configuration directory so that file change warnings do not show up on un-install.
if [ -d /awips2/cave/configuration ]; then
   rm -rf /awips2/cave/configuration/ > /dev/null 2>&1
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/cave
%doc /awips2/cave/about.html
/awips2/cave/artifacts.xml 
/awips2/cave/cave.ini
%dir /awips2/cave/etc
/awips2/cave/etc/*
%config(missingok) /awips2/cave/configuration/*
%dir /awips2/cave/features
/awips2/cave/features/*
%dir /awips2/cave/p2
/awips2/cave/p2/*
%dir /awips2/cave/plugins
/awips2/cave/plugins/*
%docdir /awips2/cave/readme
%dir /awips2/cave/readme
/awips2/cave/readme/*
/awips2/cave/.eclipseproduct
 
%defattr(755,awips,fxalpha,755)
/awips2/cave/cave
