#
# AWIPS II Tools Spec File
#

Name: awips2-tools
Summary: AWIPS II Tools Distribution
Version: 1.0.0
Release: 3
Group: AWIPSII
BuildRoot: /tmp
BuildArch: i386
Prefix: /awips2/tools
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-tools

%description
AWIPS II Python Distribution - Contains the AWIPS II Tool-Set. Presently,
the AWIPS II Tool-Set consists of various hdf5 utilities.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/tools
# The temporary build location of hdf5
mkdir -p /tmp/awips2-hdf5/build
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d

%build
# Ensure that awips2-python has been installed
COMMAND=`rpm -q awips2-python`
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: awips2-python Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

PROFILE_D_DIR="rpms/awips2.core/Installer.tools/scripts/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

# Update LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/awips2/python/lib:$LD_LIBRARY_PATH

HDF5_SOURCE_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.tools/source"
HDF5_TAR_FILE="hdf5-1.8.4-patch1.tar.gz"
LZF_TAR_FILE="lzf.tar.gz"

# Copy the hdf5 source tar files to our temporary build directory
cp ${HDF5_SOURCE_DIR}/* /tmp/awips2-hdf5/build

cd /tmp/awips2-hdf5/build

# Untar both tar files.
tar -xf ${HDF5_TAR_FILE}
tar -xf ${LZF_TAR_FILE}

# Apply the patch.
cd hdf5-1.8.4-patch1
patch -p2 -i ../hdf5-1.8.4-patch1.patch0
RC=$?
if [ $? -ne 0 ]; then
   exit 1
fi

export AM_LDFLAGS="-L/tmp/awips2-hdf5/build/lzf/lib"
export AM_CPPFLAGS="-I/tmp/awips2-hdf5/build/lzf/include"
export LIBS="-llzf_filter"
export LD_LIBRARY_PATH=/tmp/awips2-hdf5/build/lzf/lib:$LD_LIBRARY_PATH

./configure --prefix=${RPM_BUILD_ROOT}/awips2/tools
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

make
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

%install
cd /tmp/awips2-hdf5/build/hdf5-1.8.4-patch1
make install

# Copy the lzf library to tools/lib
cp /tmp/awips2-hdf5/build/lzf/lib/* \
   ${RPM_BUILD_ROOT}/awips2/tools/lib

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
   
   cp %{_baseline_workspace}/rpms/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/rpms/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}
mkdir -p ${RPM_BUILD_ROOT}/awips2/tools/bin
mkdir -p ${RPM_BUILD_ROOT}/awips2/tools/include
mkdir -p ${RPM_BUILD_ROOT}/awips2/tools/lib

copyLegal "awips2/tools"
rm -rf /tmp/awips2-hdf5

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Tools Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II Tools Distribution Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
}

echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II Tools Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

#echo "--------------------------------------------------------------------------------"
#echo "\| Adding Environment Variables for AWIPS II Tools"
#echo "--------------------------------------------------------------------------------"

if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Tools Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
# Remove The profile.d Script If One Was Created.

AWIPS2_TOOLS_SH="/etc/profile.d/awips2Tools.sh"
if [ -f ${AWIPS2_TOOLS_SH} ]; then
   rm -f ${AWIPS2_TOOLS_SH}
fi

AWIPS2_TOOLS_CSH="/etc/profile.d/awips2Tools.csh"
if [ -f ${AWIPS2_TOOLS_CSH} ]; then
   rm -f ${AWIPS2_TOOLS_CSH}
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Tools Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2HDF5Tools.csh
%attr(755,root,root) /etc/profile.d/awips2HDF5Tools.sh
%dir /awips2/tools
%dir /awips2/tools/include
/awips2/tools/include/*
%dir /awips2/tools/lib
/awips2/tools/lib/*
%docdir /awips2/tools/licenses
%dir /awips2/tools/licenses
/awips2/tools/licenses/*

%defattr(755,awips,fxalpha,755)
%dir /awips2/tools/bin
/awips2/tools/bin/*