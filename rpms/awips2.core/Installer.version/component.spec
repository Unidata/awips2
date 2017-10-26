# RPM Metadata
%define _component_name           awips2
%define _component_desc           "awips2"
# Other vars:
#   %{_component_build_date}
#   %{_component_build_time}
#   %{_component_build_system}
#
# awips2 Spec File
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: %{_component_name}
Summary: awips2 Installation
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
provides: %{_component_name}
requires: awips2-base-component
requires: wget unzip bc

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

mkdir -p ${RPM_BUILD_ROOT}

%build

%install
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

PROFILE_D_DIR="rpms/awips2.core/Installer.version/scripts/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

%pre

%post
# the length is actually 50; however, we account for 
# the leading "* " and the trailing " *"
CONST_BANNER_LENGTH=47
EDEX_BANNER_TXT=

function padEdexBannerLine()
{
	local _output="${1}"
	local _bannerTxt="${2}"
	
	local outputLength=${#_output}
	
	if [ ${outputLength} -ge ${CONST_BANNER_LENGTH} ]; then
	   _output=${_output:0:46}
	   echo "* ${_output} *" >> ${_bannerTxt}
	   return 0
	fi
	
	let padLength=${CONST_BANNER_LENGTH}-${outputLength}
	
	printf "* ${_output} %${padLength}s\\n" \* >> ${_bannerTxt}
	
	return 0
}

function updateEDEXVersion()
{
   rm -f ${EDEX_BANNER_TXT}
   touch ${EDEX_BANNER_TXT}
   
   echo "**************************************************" \
      > ${EDEX_BANNER_TXT}
   echo "* AWIPS II EDEX ESB Platform                     *" \
      >> ${EDEX_BANNER_TXT}
   padEdexBannerLine "Version: %{_component_version}-%{_component_release}" "${EDEX_BANNER_TXT}"
   echo "* Raytheon Company                               *" \
      >> ${EDEX_BANNER_TXT}
   echo "*------------------------------------------------*" \
      >> ${EDEX_BANNER_TXT}
   padEdexBannerLine "Build Date  : %{_component_build_date}" "${EDEX_BANNER_TXT}"
   padEdexBannerLine "Build Time  : %{_component_build_time}" "${EDEX_BANNER_TXT}"
   padEdexBannerLine "Build System: %{_component_build_system}" "${EDEX_BANNER_TXT}"
   echo "**************************************************" \
      >> ${EDEX_BANNER_TXT}
}

EDEX_BANNER_TXT="/awips2/edex/conf/banner.txt"
# does banner.txt exist?
if [ -f ${EDEX_BANNER_TXT} ]; then
   updateEDEXVersion   
fi

%preun

%postun

%files
%attr(755,root,root) /etc/profile.d/awips2.csh
%attr(755,root,root) /etc/profile.d/awips2.sh

%clean
rm -rf ${RPM_BUILD_ROOT}
