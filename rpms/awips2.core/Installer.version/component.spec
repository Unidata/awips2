# RPM Metadata
%define _component_name           awips2-version
%define _component_desc           "awips2-version"
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
Summary: %{_component_name} Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
provides: %{_component_name}

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

%install
mkdir -p %{_build_root}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi

mkdir -p %{_build_root}/awips2/etc
if [ $? -ne 0 ]; then
   exit 1
fi

%posttrans
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

function updateCaveVersion() {
   # Note: the system properties echoed to the versions script are based on
   # about.mappings in the com.raytheon.viz.product.awips plugin.
   AWIPS_VERSION_TXT=/awips2/cave/awipsVersion.txt

   echo "-DvizVersion=%{_component_version}-%{_component_release}" > ${AWIPS_VERSION_TXT}
   echo "-DbuildDate=%{_component_build_date}" >> ${AWIPS_VERSION_TXT}
   echo "-DbuildTime=%{_component_build_time}" >> ${AWIPS_VERSION_TXT}
   echo "-DbuildSystem=%{_component_build_system}" >> ${AWIPS_VERSION_TXT}
}

function updateAlertVizVersion() {
   # Note: alertviz does not include any of the branding information that CAVE does.
   # So, we will only be utilizing the version override.
   AWIPS_VERSION_TXT=/awips2/alertviz/awipsVersion.txt

   echo "--launcher.appendVmargs" > ${AWIPS_VERSION_TXT}
   echo "-vmargs" >> ${AWIPS_VERSION_TXT}
   echo "-DvizVersion=%{_component_version}-%{_component_release}" >> ${AWIPS_VERSION_TXT}
}

function createAWIPS2LibSource() {

   AWIPS_ENV=/awips2/etc/environment
  
   echo '#!/bin/bash' > ${AWIPS_ENV}
   echo "" >> ${AWIPS_ENV}
   echo "export A2LIBS=true" >> ${AWIPS_ENV}
   echo "" >> ${AWIPS_ENV}
   echo "for i in /etc/profile.d/awips2*.sh" >> ${AWIPS_ENV}
   echo "do" >> ${AWIPS_ENV}
   echo "    source \$i" >> ${AWIPS_ENV}
   echo "done" >> ${AWIPS_ENV}
}

if [ -d /awips2/cave ]; then
   updateCaveVersion
fi

if [ -d /awips2/alertviz ]; then
   updateAlertVizVersion
fi

EDEX_BANNER_TXT="/awips2/edex/conf/banner.txt"
# does banner.txt exist?
if [ -f ${EDEX_BANNER_TXT} ]; then
   updateEDEXVersion   
fi

createAWIPS2LibSource

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/etc
