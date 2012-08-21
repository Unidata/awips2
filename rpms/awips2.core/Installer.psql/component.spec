#
# AWIPS II PSQL Spec File
#
%define __prelink_undo_cmd %{nil}

Name: awips2-psql
Summary: AWIPS II PSQL Distribution
Version: 8.3.4
Release: 3
Group: AWIPSII
BuildRoot: /tmp
BuildArch: i386
Prefix: /awips2/psql
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-psql
requires: libtermcap >= 2.0.8-46.1
requires: libtermcap.so.2
requires: libreadline.so.4

%description
AWIPS II PSQL Distribution - Contains PSQL (PostgreSQL) V8.3.4

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/psql
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
mkdir -p ${RPM_BUILD_ROOT}/etc/ld.so.conf.d
touch ${RPM_BUILD_ROOT}/etc/ld.so.conf.d/awips2-psql-i386.conf

%build
PROFILE_D_DIR="rpms/awips2.core/Installer.psql/scripts/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

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
   
   cp %{_baseline_workspace}/rpms/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/rpms/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}
cp -r %{_awipscm_share}/packages/psql/* ${RPM_BUILD_ROOT}/awips2/psql
# Remove all shared libraries that are not created by psql.
if [ -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.a ]; then
   rm -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.a
fi
if [ -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.so ]; then
   rm -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.so
fi
if [ -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.so.4 ]; then
   rm -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.so.4
fi
if [ -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.so.4.3 ]; then
   rm -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libreadline.so.4.3
fi
if [ -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libtermcap.a ]; then
   rm -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libtermcap.a
fi
if [ -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libtermcap.so ]; then
   rm -f ${RPM_BUILD_ROOT}/awips2/psql/lib/libtermcap.so
fi

copyLegal "awips2/psql"

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II PSQL Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}\e[m"

%post
function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II PSQL Distribution Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
}
echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II PSQL Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

echo "${RPM_INSTALL_PREFIX}/lib" >> /etc/ld.so.conf.d/awips2-psql-i386.conf

# Run ldconfig
/sbin/ldconfig

echo "--------------------------------------------------------------------------------"
echo "\| Changing AWIPS II PSQL owner to awips:fxalpha"
echo "--------------------------------------------------------------------------------"
chown -R awips:fxalpha ${RPM_INSTALL_PREFIX}

if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II PSQL Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II PSQL Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%attr(755,root,root) /etc/profile.d/awips2PSQL.csh
%attr(755,root,root) /etc/profile.d/awips2PSQL.sh
%attr(755,root,root) /etc/ld.so.conf.d/awips2-psql-i386.conf
%defattr(755,awips,fxalpha,755)
%dir /awips2/psql
%dir /awips2/psql/bin
/awips2/psql/bin/*
%defattr(644,awips,fxalpha,755)
%dir /awips2/psql/lib
%attr(777,awips,fxalpha) /awips2/psql/lib/libpq.so
%attr(777,awips,fxalpha) /awips2/psql/lib/libpq.so.5
%attr(755,awips,fxalpha) /awips2/psql/lib/libpq.so.5.1
%docdir /awips2/psql/licenses
%dir /awips2/psql/licenses
/awips2/psql/licenses/*