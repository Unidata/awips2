#
# AWIPS II Python Spec File
#
Name: awips2-python
Summary: AWIPS II Python Distribution
Version: 2.7.1
Release: 1
Group: AWIPSII
BuildRoot: /tmp
Prefix: /awips2/python
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-python
obsoletes: awips2-python < 2.7.1

#-----------------------------------------------------------------------------#
# IMPORTANT ! INFORMATION ABOUT UPGRADING PYTHON!                             #                              
#-----------------------------------------------------------------------------#
# Whenever python is upgraded, the following libraries will need to be
# re-compiled with the updated python shared library.
# 1) grib2.so
# 2) gridslice.so
# 3) mod_wsgi (pypies)
#-----------------------------------------------------------------------------#

%description
AWIPS II Python Distribution - Contains Python V2.7.1 plus modules
required for AWIPS II

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

mkdir -p ${RPM_BUILD_ROOT}/awips2/python
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d

%build
PROFILE_D_DIR="Installer.rpm/awips2.core/Installer.python/scripts/profile.d"
cp ${WORKSPACE_DIR}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

PYTHON_SRC_TAR="Python-2.7.1.tgz"
PATH_TO_PYTHON_SRC="${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.python"
PYTHON_SRC="${PATH_TO_PYTHON_SRC}/src/${PYTHON_SRC_TAR}"

# Copy the source to a temporary location
PYTHON_BUILD_DIR="${RPM_BUILD_ROOT}/awips2/python-build"

mkdir -p ${PYTHON_BUILD_DIR}
cp ${PYTHON_SRC} ${PYTHON_BUILD_DIR}

# Untar The Source
cd ${PYTHON_BUILD_DIR}
tar -xf ${PYTHON_BUILD_DIR}/${PYTHON_SRC_TAR}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1 
fi

# Check Return Codes For The Steps That Follow ...
# Run Configure
PYTHON_SRC_DIR="${RPM_BUILD_ROOT}/awips2/python-build/Python-2.7.1"

cd ${PYTHON_SRC_DIR}
./configure --prefix=${RPM_BUILD_ROOT}/awips2/python --enable-shared
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

# Run Make
make clean
make
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

%install
# Copies the standard Raytheon licenses into a license directory for the
# current component.
function copyLegal()
{
   # $1 == Component Build Root
   
   COMPONENT_BUILD_DIR=${1}
   
   mkdir -p ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   
   # Create a Tar file with our FOSS licenses.
   tar -cjf ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar \
      ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses/
   
   cp ${WORKSPACE_DIR}/Installer.rpm/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "${WORKSPACE_DIR}/Installer.rpm/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar    
}

PYTHON_BUILD_DIR="${RPM_BUILD_ROOT}/awips2/python-build"
PYTHON_SRC_DIR="${PYTHON_BUILD_DIR}/Python-2.7.1"
cd ${PYTHON_SRC_DIR}

make install
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

ADDL_TAR_PATH="${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.python"
PYTHON_ADDL_TAR="${ADDL_TAR_PATH}/src/awips2-python.tar.gz"
tar -xf ${PYTHON_ADDL_TAR} -C ${RPM_BUILD_ROOT}/awips2/python/
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

# Remove our temporary build directory
rm -rf ${PYTHON_BUILD_DIR}

copyLegal "awips2/python"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Python Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II Python Distribution Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
}

function update_script_first_line()
{
   # $1 == the name of the script

   PYTHON_BIN_DIR=${RPM_INSTALL_PREFIX}/bin
   PYTHON_EXE=${PYTHON_BIN_DIR}/python

   sed '1c\'"#!${PYTHON_EXE}" ${PYTHON_BIN_DIR}/${1} > ${PYTHON_BIN_DIR}/${1}.tmp
   rm -rf ${PYTHON_BIN_DIR}/${1}
   mv ${PYTHON_BIN_DIR}/${1}.tmp ${PYTHON_BIN_DIR}/${1}
   chmod a+x ${PYTHON_BIN_DIR}/${1}
}
echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II Python Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

update_script_first_line 2to3
update_script_first_line idle
update_script_first_line pydoc
update_script_first_line python2.7-config
update_script_first_line smtpd.py

echo "--------------------------------------------------------------------------------"
echo "\| Adding Environment Variables for AWIPS II Python"
echo "--------------------------------------------------------------------------------"

if [ -L ${RPM_INSTALL_PREFIX}/lib/.site-packages ]; then
   rm -f ${RPM_INSTALL_PREFIX}/lib/.site-packages
fi
cd ${RPM_INSTALL_PREFIX}/lib
ln -s python2.7/site-packages .site-packages

if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Python Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II Python Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,awips,fxalpha,-)
%attr(755,root,root) /etc/profile.d/awips2Python.csh
%attr(755,root,root) /etc/profile.d/awips2Python.sh
%dir /awips2/python
%dir /awips2/python/lib
/awips2/python/lib/*
%docdir /awips2/python/licenses
%dir /awips2/python/licenses
/awips2/python/licenses/*
%dir /awips2/python/share
/awips2/python/share/*
%defattr(755,awips,fxalpha,755)
%dir /awips2/python/include
/awips2/python/include/*
%dir /awips2/python/bin
/awips2/python/bin/*