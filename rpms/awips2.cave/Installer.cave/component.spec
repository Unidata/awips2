# RPM Metadata
%define _component_name           awips2-cave
%define _component_project_dir    awips2.cave/Installer.cave
%define _component_zip_file_name  CAVE-linux.gtk.%{_build_arch}.zip
%define _component_desc           "awips2-cave"

%define _swt_version 3.8.1.v3836b
%define _ui_version 3.8.2.v20121018-234953
#
# awips2-cave Spec File
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
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
Packager: Bryan Kowal

AutoReq: no
provides: %{_component_name}
provides: awips2-base-component
provides: awips2-base
requires: awips2-java
requires: awips2-python
requires: openmotif
requires: libMrm.so.4
requires: libXp.so.6
requires: libg2c.so.0

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
mkdir -p ${RPM_BUILD_ROOT}/awips2
mkdir -p ${RPM_BUILD_ROOT}/etc/xdg/autostart
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
CAVE_DIST_DIR="%{_baseline_workspace}/rpms/awips2.cave/setup/dist"

if [ ! -f ${CAVE_DIST_DIR}/%{_component_zip_file_name} ]; then
   echo "ERROR: Unable to find - ${CAVE_DIST_DIR}/%{_component_zip_file_name}."
   exit 1
fi

cp ${CAVE_DIST_DIR}/%{_component_zip_file_name} \
   ${RPM_BUILD_ROOT}/awips2

%build

%install
cd ${RPM_BUILD_ROOT}/awips2
unzip %{_component_zip_file_name}
rm -f %{_component_zip_file_name}

# Our profile.d scripts
PROFILE_D_DIR="%{_baseline_workspace}/rpms/common/environment/awips2-cave/profile.d"
cp ${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

# The AWIPS II version script.
VERSIONS_SCRIPT="rpms/utility/scripts/versions.sh"
cp %{_baseline_workspace}/${VERSIONS_SCRIPT} ${RPM_BUILD_ROOT}/awips2/cave

# testWS script
TEXTWS_SCRIPT="rpms/utility/scripts/textWS.sh"
cp %{_baseline_workspace}/${TEXTWS_SCRIPT} ${RPM_BUILD_ROOT}/awips2/cave

# text-workstation autostart script.
CAVE_SCRIPTS_DIR="%{_baseline_workspace}/rpms/awips2.cave/Installer.cave/scripts"
TEXTWS_AUTO_SCRIPT="${CAVE_SCRIPTS_DIR}/autostart/awips2-textws.desktop"
cp -v ${TEXTWS_AUTO_SCRIPT} ${RPM_BUILD_ROOT}/etc/xdg/autostart
if [ $? -ne 0 ]; then
   exit 1
fi

%pre
if [ "${1}" = "2" ]; then
   # During an upgrade, we need to copy CAVE components that should not
   # change without a complete re-install to a temporary location so that
   # they are not overwritten. If we do not do this, CAVE will not recognize
   # any of the features / plugins that have been installed after the upgrade.
   
   # Create the CAVE backup directory
   mkdir -p /awips2/cave.bak
   
   # Ensure that the file directories that need to be backed up exist
   if [ ! -f /awips2/cave/artifacts.xml ]; then
      echo "ERROR: The cave artifacts.xml file does not exist."
      echo "       Your CAVE install is corrupted. Please re-install."
      exit 1   
   fi
   
   if [ ! -f /awips2/cave/cave ]; then
      echo "ERROR: The cave executable does not exist."
      echo "       Your CAVE install is corrupted. Please re-install."
      exit 1
   fi
   
   if [ ! -d /awips2/cave/configuration ]; then
      echo "ERROR: The cave configuration directory does not exist."
      echo "       Your CAVE install is corrupted. Please re-install."
      exit 1   
   fi
   
   if [ ! -d /awips2/cave/features ]; then
      echo "ERROR: The cave features directory does not exist."
      echo "       Your CAVE install is corrupted. Please re-install."
      exit 1  
   fi
   
   if [ ! -d /awips2/cave/p2 ]; then
      echo "ERROR: The cave p2 directory does not exist."
      echo "       Your CAVE install is corrupted. Please re-install."
      exit 1   
   fi
   
   if [ ! -d /awips2/cave/plugins ]; then
      echo "ERROR: The cave plugins directory does not exist."
      echo "       Your CAVE install is corrupted. Please re-install."
      exit 1   
   fi
   
   # Create the backups
   mv /awips2/cave/artifacts.xml /awips2/cave.bak/
   mv /awips2/cave/cave /awips2/cave.bak/
   mv /awips2/cave/configuration /awips2/cave.bak/
   mv /awips2/cave/features /awips2/cave.bak/
   mv /awips2/cave/p2 /awips2/cave.bak/
   mv /awips2/cave/plugins /awips2/cave.bak/
   
   exit 0
fi

# /awips2/cave must not exist.
if [ -d /awips2/cave ]; then
   echo -e "\e[1;31mERROR: the /awips2/cave directory already exists. /awips2/cave\e[m"
   echo -e "\e[1;31m       must be REMOVED before the installation will proceed.\e[m"
   exit 1
fi

%post
# Remove the text-workstation autostart script if we have not been installed
# on an xt workstation
if [ ! "`hostname | cut -b 1-2`" = "xt" ]; then
   rm -f /etc/xdg/autostart/awips2-textws.desktop
fi

MACHINE_BIT=`uname -i`
if [ "${MACHINE_BIT}" = "i386" ]
then
    if [ -d /awips2/cave/lib/lib64 ]; then
       rm -rf /awips2/cave/lib/lib64
    fi
fi

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

pushd . > /dev/null 2>&1
cd /awips2/cave/plugins
# Forcefully unzip: org.eclipse.swt.gtk.linux.x86_3.6.1.v3655c.jar
# : if i386
if [ -f org.eclipse.swt.gtk.linux.x86_%{_swt_version}.jar ]; then
   mkdir org.eclipse.swt.gtk.linux.x86_%{_swt_version}
   unzip -qq org.eclipse.swt.gtk.linux.x86_%{_swt_version}.jar \
      -d org.eclipse.swt.gtk.linux.x86_%{_swt_version}
   rm -f org.eclipse.swt.gtk.linux.x86_%{_swt_version}.jar
   mv org.eclipse.swt.gtk.linux.x86_%{_swt_version} \
      org.eclipse.swt.gtk.linux.x86_%{_swt_version}.jar
fi
# Forcefully unzip: org.eclipse.swt.gtk.linux.x86_64_3.6.1.v3655c.jar
# : if x86_64
if [ -f org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar ]; then
   mkdir org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}
   unzip -qq org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar \
      -d org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}
   rm -f org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar
   mv org.eclipse.swt.gtk.linux.x86_64_%{_swt_version} \
      org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar
fi

# Forcefully unzip: org.eclipse.ui_3.6.1.M20100826-1330.jar
# : for i386 & x86_64
if [ -f org.eclipse.ui_%{_ui_version}.jar ]; then
   mkdir org.eclipse.ui_%{_ui_version}
   unzip -qq org.eclipse.ui_%{_ui_version}.jar \
      -d org.eclipse.ui_%{_ui_version}
   rm -f org.eclipse.ui_%{_ui_version}.jar
   mv org.eclipse.ui_%{_ui_version} \
      org.eclipse.ui_%{_ui_version}.jar
fi

popd > /dev/null 2>&1

if [ "${1}" = "2" ]; then
   # Restore the backup.
   
   # Remove the files that were just placed on the filesystem by the install.
   rm -f /awips2/cave/artifacts.xml
   rm -f /awips2/cave/cave
   rm -rf /awips2/cave/configuration
   rm -rf /awips2/cave/features
   rm -rf /awips2/cave/p2
   rm -rf /awips2/cave/plugins
   
   mv /awips2/cave.bak/* /awips2/cave
   rm -rf /awips2/cave.bak
fi

%preun
# Backup the core CAVE jar files so that they are
# not removed during the uninstallation of a previous
# version of CAVE during an upgrade.
if [ "${1}" = "1" ]; then
   # This is an upgrade.
   if [ -d /awips2/cave.bak ]; then
      rm -rf /awips2/cave.bak
   fi
   mkdir -p /awips2/cave.bak/plugins
   
   CAVE_APPLICATION_PLUGIN="com.raytheon.uf.viz.application_*.jar"
   CAVE_PRODUCT_PLUGIN="com.raytheon.viz.product.awips_*.jar"
   
   
   cp /awips2/cave/plugins/${CAVE_APPLICATION_PLUGIN} \
      /awips2/cave.bak/plugins
   cp /awips2/cave/plugins/${CAVE_PRODUCT_PLUGIN} \
      /awips2/cave.bak/plugins
fi

%postun
# Restore the CAVE jar files that were backed-up if this
# is an upgrade.
if [ "${1}" = "1" ]; then
   # This is an upgrade.
   CAVE_APPLICATION_PLUGIN="com.raytheon.uf.viz.application_*.jar"
   CAVE_PRODUCT_PLUGIN="com.raytheon.viz.product.awips_*.jar"
   
   if [ ! -d /awips2/cave.bak/plugins ]; then
      exit 0
   fi
   
   if [ -d /awips2/cave/plugins ]; then
      mv /awips2/cave.bak/plugins/${CAVE_APPLICATION_PLUGIN} \
         /awips2/cave/plugins
      mv /awips2/cave.bak/plugins/${CAVE_PRODUCT_PLUGIN} \
         /awips2/cave/plugins
   fi
   
   rm -rf /awips2/cave.bak
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,root,root,-)
/etc/profile.d/awips2Cave.csh
/etc/profile.d/awips2Cave.sh

%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/cave
%docdir /awips2/cave/about_files
%dir /awips2/cave/about_files
/awips2/cave/about_files/*
%doc /awips2/cave/about.html
/awips2/cave/artifacts.xml 
/awips2/cave/*.ini
%dir /awips2/cave/configuration
/awips2/cave/configuration/*
%doc /awips2/cave/epl-v10.html
%dir /awips2/cave/features
/awips2/cave/features/*
%doc /awips2/cave/notice.html
%dir /awips2/cave/p2
/awips2/cave/p2/*
%dir /awips2/cave/plugins
/awips2/cave/plugins/*
%docdir /awips2/cave/readme
%dir /awips2/cave/readme
/awips2/cave/readme/*
/awips2/cave/.eclipseproduct
 
%defattr(755,awips,fxalpha,755)
%dir /awips2/cave/caveEnvironment
/awips2/cave/caveEnvironment/*
/awips2/cave/cave
/awips2/cave/*.sh
/awips2/cave/*.so
%dir /awips2/cave/lib%{_build_bits}
/awips2/cave/lib%{_build_bits}/*

%attr(644,root,root) /etc/xdg/autostart/awips2-textws.desktop
