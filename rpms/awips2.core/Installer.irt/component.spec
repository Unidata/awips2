#
# AWIPS II IRT Spec File
#
Name: awips2-irt
Summary: AWIPS II IRT Installation
Version: 1.0.0
Release: 1%{?dist}
Group: AWIPSII
BuildRoot: /tmp
Prefix: /irt
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: awips2-irt
Requires: awips2-python

%description
AWIPS II IRT Installation - Contains The AWIPS II IRT Component.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/irt

%build

%install
# Copies the standard Raytheon licenses into a license directory for the
# current component.
function copyLegal()
{
   # $1 == Component Build Root
   
   COMPONENT_BUILD_DIR=${1}
   
   mkdir -p ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   
   # Create a Tar file with our FOSS licenses.
   tar -cjf %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      %{_baseline_workspace}/rpms/legal/FOSS_licenses/
   
   cp "%{_baseline_workspace}/rpms/legal/Master_Rights_File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   echo "\"/${COMPONENT_BUILD_DIR}/licenses/Master_Rights_File.pdf\"" \
      >> %{_topdir}/BUILD/component-files.txt
   echo "\"/${COMPONENT_BUILD_DIR}/licenses/FOSS_licenses.tar\"" \
      >> %{_topdir}/BUILD/component-files.txt
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}
cp -r %{_awipscm_share}/packages/irt-server/* ${RPM_BUILD_ROOT}/irt

# Copy The Configuration File To The Appropriate Directory.
IRT_CONFIG_FILE="IRT_Config.txt"
CONFIG_FILE_SRC_DIR="rpms/awips2.core/Installer.irt/scripts/conf"
CONFIG_FILE_DEST_DIR="IRT-operational/server"
cp %{_baseline_workspace}/${CONFIG_FILE_SRC_DIR}/${IRT_CONFIG_FILE} \
   ${RPM_BUILD_ROOT}/irt/${CONFIG_FILE_DEST_DIR}
   
# Create an IRT bin Directory and Copy the Startup Script to It.
IRT_STARTUP_FILE="start_irt.sh"
STARTUP_FILE_SRC_DIR="rpms/awips2.core/Installer.irt/scripts"
mkdir -p ${RPM_BUILD_ROOT}/irt/bin
cp %{_baseline_workspace}/${STARTUP_FILE_SRC_DIR}/${IRT_STARTUP_FILE} \
   ${RPM_BUILD_ROOT}/irt/bin

cd ${RPM_BUILD_ROOT}
for item in `find irt/ -name "*"`
do
   if [ ! -d ${item} ]; then
      echo "/"${item} >> %{_topdir}/BUILD/component-files.txt
   fi
done

copyLegal "irt"

%pre

%post
echo ${RPM_INSTALL_PREFIX} | sed 's/\//\\\//g' > .awips2_escape.tmp
IRT_INSTALL_ESCAPED=`cat .awips2_escape.tmp`
rm -f .awips2_escape.tmp

IRT_STARTUP_FILE="start_irt.sh"
perl -p -i -e "s/%{INSTALL_PATH}%/${IRT_INSTALL_ESCAPED}/g" \
   ${RPM_INSTALL_PREFIX}/bin/${IRT_STARTUP_FILE}
chmod a+x ${RPM_INSTALL_PREFIX}/bin/*

%postun

%files -f component-files.txt
