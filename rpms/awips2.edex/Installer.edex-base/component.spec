%define CORE_DELTA_SETUP ${WORKSPACE_DIR}/Installer.rpm/delta/setup/updateSetup.sh
%define _component_name           awips2-edex-base
%define _component_project_dir    awips2.edex/Installer.edex-base
%define _component_default_prefix /awips2
#
# AWIPS II Edex Spec File
#
Name: %{_component_name}
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-edex-base
provides: awips2-base-component
requires: awips2-python
requires: awips2-java
requires: awips2-psql

%description
AWIPS II Edex Installation - Sets Up AWIPS II Edex "Base".

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex

%build
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
source %{CORE_DELTA_SETUP}
copySetupCore ${RPM_BUILD_ROOT} %{_component_default_prefix}
copyApplicableDeltas ${RPM_BUILD_ROOT} %{_component_name} \
   %{_component_project_dir} %{_component_default_prefix}
#---------------------------------------------------------------------------#

%install
DEPLOY_SCRIPT="build.edex/deploy-common/deploy-esb.xml"

# Deploy Edex To Our Temporary Build Directory.

# Determine which ant executable to use.
COMMAND=`rpm -q awips2-ant`
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: awips2-ant Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

ANT_EXE="/awips2/ant/bin/ant"

${ANT_EXE} -file ${WORKSPACE_DIR}/${DEPLOY_SCRIPT} \
   -Ddeploy.data=true -Ddeploy.web=true \
   -Desb.overwrite=true \
   -Desb.directory=${WORKSPACE_DIR}/build.edex/esb \
   -Dedex.root.directory=${RPM_BUILD_ROOT}/awips2/edex \
   -Dbasedir=${WORKSPACE_DIR}/build.edex \
   -Dbasedirectories=${WORKSPACE_DIR}
if [ $? -ne 0 ]; then
   exit 1
fi
# Remove the directory with the shapefiles (if it exists)
if [ -d ${RPM_BUILD_ROOT}/awips2/edex/data/utility/edex_static/base/shapefiles ]; then
   rm -rf ${RPM_BUILD_ROOT}/awips2/edex/data/utility/edex_static/base/shapefiles
fi

# For our service script.
mkdir -p ${RPM_BUILD_ROOT}/etc/init.d
cp ${WORKSPACE_DIR}/Installer.rpm/awips2.edex/Installer.edex-base/scripts/init.d/* \
   ${RPM_BUILD_ROOT}/etc/init.d 

#copy the versions.sh script
cd ${RPM_BUILD_ROOT}/awips2/edex/bin
cp ${WORKSPACE_DIR}/Installer.rpm/utility/scripts/versions.sh .
   
# Create a directory for the 64 bit python libraries.
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/lib/lib64
# Copy the libpython.so to the lib64 directory.
cp ${WORKSPACE_DIR}/Installer.rpm/awips2.edex/Installer.edex-base/lib64/libpython.so \
   ${RPM_BUILD_ROOT}/awips2/edex/lib/lib64   

%pre
# Save banner.txt since we are no longer responsible
# for updating it.

if [ -f /awips2/edex/conf/banner.txt ]; then
   if [ -d /awips2/edexBak ]; then
      rm -rf /awips2/edexBak
   fi
   mkdir -p /awips2/edexBak
   cp -p /awips2/edex/conf/banner.txt /awips2/edexBak
fi

if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II Edex (Base)...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}/edex\e[m"

%post

# restore banner.txt since we saved it in %pre.
if [ -f /awips2/edexBak/banner.txt ]; then
   cp -pf /awips2/edexBak/banner.txt /awips2/edex/conf
   rm -rf /awips2/edexBak
fi

MACHINE_BIT=`uname -i`
if [ "${MACHINE_BIT}" = "i386" ]
then
    rm -Rf /awips2/edex/lib/lib64
    echo "\| Removing /awips2/edex/lib/lib64/libpython.so because running in 32 bit environment..."
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

echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II Edex Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

# Replace The Paths In The Various Scripts.
# Edex Environment Scripts To Update (?)
# 2] edex/bin/setup.env (Now Contains Default Values - RPM Needs To Make No Changes)
# 3] edex/bin/daminfotranslator.sh (Java)
# 5] edex/data/utility/common_static/base/hydro/Apps_defaults (Java)
# 6] edex/data/utility/common_static/site/OAX/hydro/Apps_defaults (N/A)

# DB Configuration Scripts To Update (?)
# 1] edex/conf/db/hibernateConfig/damCatalog/hibernate.cfg.xml (Now Contains Defaults)
# 2] edex/conf/db/hibernateConfig/fxa/hibernate.cfg.xml (Now Contains Defaults)
# 3] edex/conf/db/hibernateConfig/ihfs/hibernate.cfg.xml (Now Contains Defaults)
# 4] edex/conf/db/hibernateConfig/metadata/hibernate.cfg.xml (Now Contains Defaults)
# 5] edex/conf/db/hibernateConfig/maps/hibernate.cfg.xml (Now Contains Defaults)
# 6] edex/conf/db/hibernateConfig/hmdb/hibernate.cfg.xml (Now Contains Defaults)

# Edex Configuration Scripts To Update (?)
# 1] edex/conf/res/site/environment.xml (Already Contains Defaults / Uses Env Variables)
# 2] edex/conf/res/base/environment.xml (Already Contains Defaults / Uses Env Variables)

if [ -f /etc/init.d/edex_camel ]; then
   /sbin/chkconfig --add edex_camel
fi

#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
if [ "${1}" = "2" ]; then
   echo "INFO: Performing %{_component_name} Upgrade."
   echo "Preparing ..."
   
   # Check the delta directory to see if there are updates that
   # may need to be applied.
   cd ${RPM_INSTALL_PREFIX}/delta/%{_component_name}
   COUNT=`ls -1 | wc -l`
   
   if [ "${COUNT}" = "0" ]; then
      echo "INFO: No Updates To Perform."
      exit 0
   fi
   
   echo "INFO: Potentially Applying ${COUNT} Updates."
   
   # The Update Manager Is In: ${RPM_INSTALL_PREFIX}/delta
   UPDATE_MANAGER="${RPM_INSTALL_PREFIX}/delta/updateManager.sh"
   cd ${RPM_INSTALL_PREFIX}/delta
   export COMPONENT_INSTALL="${RPM_INSTALL_PREFIX}"
   ${UPDATE_MANAGER} %{_component_name}
   
   exit 0
fi
#---------------------------------------------------------------------------#
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Edex (Base) Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi
if [ -f /etc/init.d/edex_camel ]; then
   /sbin/chkconfig --del edex_camel
fi

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II Edex (Base) Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
%dir %{_component_default_prefix}/delta
%attr(700,root,root) %{_component_default_prefix}/delta/updateManager.sh
%attr(700,root,root) %{_component_default_prefix}/delta/createUpdateRegistry.sh
%{_component_default_prefix}/delta/%{_component_name}
#---------------------------------------------------------------------------#
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/conf
/awips2/edex/conf/*
%dir /awips2/edex/data
/awips2/edex/data/*
%dir /awips2/edex/etc
/awips2/edex/etc/*
%dir /awips2/edex/lib
/awips2/edex/lib/*
%dir /awips2/edex/logs
%dir /awips2/edex/webapps
/awips2/edex/webapps/*
/awips2/edex/bin/wrapper.jar
%dir /awips2/edex/bin/linux-x86-32
/awips2/edex/bin/linux-x86-32/*.so
/awips2/edex/bin/linux-x86-32/*.conf

%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*.sh
/awips2/edex/bin/linux-x86-32/wrapper

%attr(744,root,root) /etc/init.d/edex_camel