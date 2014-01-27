%define _build_arch %(uname -i)
%define _tools_build_loc %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

#
# AWIPS II Tools Spec File
#

Name: awips2-tools
Summary: AWIPS II Tools Distribution
Version: 1.8.5
Release: 2.el6
Group: AWIPSII
BuildRequires: awips2-python-h5py
BuildRoot: /tmp
BuildRoot: %{_build_root}
BuildArch: %{_build_arch}
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
if [ "%{_build_root}" = "" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

rm -rf %{_build_root}
mkdir -p %{_build_root}
# The temporary build location of hdf5 and lzf
if [ -d %{_tools_build_loc} ]; then
   rm -rf %{_tools_build_loc}
fi
mkdir -p %{_tools_build_loc}

%build
# Ensure that awips2-python has been installed
COMMAND=`rpm -q awips2-python`
if [ $? -ne 0 ]; then
   echo "ERROR: awips2-python Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# Update LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/awips2/python/lib:$LD_LIBRARY_PATH

HDF5_SOURCE_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.tools/source"
HDF5_TAR_FILE="hdf5-1.8.4-patch1.tar.gz"
LZF_TAR_FILE="lzf.tar.gz"

# Copy the hdf5 source tar files to our temporary build directory
cp ${HDF5_SOURCE_DIR}/* %{_tools_build_loc}

cd %{_tools_build_loc}

# Untar both tar files.
tar -xf ${HDF5_TAR_FILE}
tar -xf ${LZF_TAR_FILE}

pushd . > /dev/null 2>&1
# Apply the patch.
cd hdf5-1.8.4-patch1
patch -p2 -i ../hdf5-1.8.4-patch1.patch0
if [ $? -ne 0 ]; then
   exit 1
fi

export AM_CPPFLAGS="-I%{_tools_build_loc}/lzf/include"

# run configure to generate the auto-generated hdf5 headers
./configure --prefix=%{_build_root}/awips2/tools
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null 2>&1

pushd . > /dev/null 2>&1
# build lzf
cd lzf
gcc -O2 -I%{_tools_build_loc}/hdf5-1.8.4-patch1/src \
   -fPIC -shared lzf/*.c lzf_filter.c \
   -L /awips2/python/lib -lhdf5 \
   -o liblzf_filter.so
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null

cd hdf5-1.8.4-patch1
export AM_LDFLAGS="-L%{_tools_build_loc}/lzf"
export LIBS="-llzf_filter"
export LD_LIBRARY_PATH=%{_tools_build_loc}/lzf:/awips2/python/lib

# re-configure to include the lzf_filter library that was built previously
./configure --prefix=/awips2/tools
if [ $? -ne 0 ]; then
   exit 1
fi

make
if [ $? -ne 0 ]; then
   exit 1
fi

%install
mkdir -p %{_build_root}/awips2/tools
mkdir -p %{_build_root}/etc/profile.d

cd %{_tools_build_loc}/hdf5-1.8.4-patch1
make install prefix=%{_build_root}/awips2/tools

# Copy the lzf library to tools/lib
cp %{_tools_build_loc}/lzf/*.so \
   %{_build_root}/awips2/tools/lib

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

PROFILE_D_DIR="rpms/awips2.core/Installer.tools/scripts/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* %{_build_root}/etc/profile.d

copyLegal "awips2/tools"

%pre
%post
%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}
rm -rf %{_tools_build_loc}

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
