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
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: %{_component_name}
requires: awips2-base-component

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

%pre

%post
# the length is actually 50; however, we account for 
# the leading "* " and the trailing " *"
CONST_BANNER_LENGTH=47

AWIPS_PRODUCT_JAR=
EDEX_BANNER_TXT=

# This function exists for upgrading from
# 11.6.X builds.
function updateCAVEVersionLegacy()
{
   local TMP_PRODUCTS_DIR="/awips2/cave/.tmp/products"

   cd ${TMP_PRODUCTS_DIR}
   # update plugin.xml
   sed '/aboutText/{n;s/\".*\"/\"%caveAboutText\"/;}' \
      -i plugin.xml
   
   # update the manifest.
   touch manifestUpdate
   echo "Bundle-Localization: plugin" > manifestUpdate
   
   # merge the updated plugin.xml and manifest into the jar.
   /awips2/java/bin/jar umf manifestUpdate ${AWIPS_PRODUCT_JAR}
   /awips2/java/bin/jar uf ${AWIPS_PRODUCT_JAR} plugin.xml
   
   rm -f manifestUpdate
}

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

function updateCAVEVersion()
{
   local BUILD_XML="/awips2/cave/build.xml"
   local TMP_PRODUCTS_DIR="/awips2/cave/.tmp/products"

   if [ -d ${TMP_PRODUCTS_DIR} ]; then
      rm -rf ${TMP_PRODUCTS_DIR}
   fi
   mkdir -p ${TMP_PRODUCTS_DIR}
   mv ${AWIPS_PRODUCT_JAR} ${TMP_PRODUCTS_DIR}
   AWIPS_PRODUCT_JAR=`ls -1 ${TMP_PRODUCTS_DIR}/*`
   
   touch ${BUILD_XML}
   echo "<project name=\"nothing\" default=\"doNothing\">" > ${BUILD_XML}
   echo "   <target name=\"doNothing\">" >> ${BUILD_XML}
   echo "   </target>" >> ${BUILD_XML}
   echo "</project>" >> ${BUILD_XML}
   cd /awips2/cave

   # Need to use awips2-java
   export PATH=/awips2/java/bin:${PATH}
   export JAVA_HOME="/awips2/java/jre"

   su awips -c './cave -application org.eclipse.ant.core.antRunner -nosplash' \
      > /dev/null 2>&1
   rm -f ${BUILD_XML}
   
   # Update the version information.
   cd ${TMP_PRODUCTS_DIR}
   unzip -qq ${AWIPS_PRODUCT_JAR}
   if [ ! -f plugin.properties ]; then
      updateCAVEVersionLegacy
   else
      rm -f plugin.properties
   fi
   
   ARCH="x86"
   # Determine the architecture.
   TMP=`file /awips2/cave/cave | grep "ELF 64-bit"`
   if [ ! "${TMP}" = "" ]; then
      ARCH="x86_64"
   fi
   
   touch plugin.properties
   # Write plugin.properties
   echo "caveAboutText=Common AWIPS Visualization Environment (CAVE) ${ARCH}\\n\\" \
      > plugin.properties
   echo "\\n\\" >> plugin.properties
   echo "Developed on the Raytheon Visualization Environment (viz)\\n\\" \
      >> plugin.properties
   echo "\\tBUILD VERSION: %{_component_version}-%{_component_release}\\n\\" \
      >> plugin.properties
   echo "\\tBUILD DATE: %{_component_build_date}\\n\\" \
      >> plugin.properties
   echo "\\tBUILD TIME: %{_component_build_time}\\n\\" >> plugin.properties
   echo "\\tBUILD SYSTEM: %{_component_build_system}\\n\\" \
      >> plugin.properties
   # Update the jar file.
   /awips2/java/bin/jar uf ${AWIPS_PRODUCT_JAR} plugin.properties
   # Relocate the jar file.
   mv ${AWIPS_PRODUCT_JAR} /awips2/cave/plugins 
   
   rm -rf ${TMP_PRODUCTS_DIR}
   
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

AWIPS_PRODUCT_WILDCARD="/awips2/cave/plugins/com.raytheon.viz.product.awips_*.jar"
# Get the actual name of the product jar.
if [ -d /awips2/cave/plugins ]; then
   AWIPS_PRODUCT_JAR=`ls -1 ${AWIPS_PRODUCT_WILDCARD}`
   RC=$?
   if [ ${RC} -eq 0 ]; then
      # does the jar exist?
      if [ -f ${AWIPS_PRODUCT_JAR} ]; then
         updateCAVEVersion
      fi
   fi
fi

EDEX_BANNER_TXT="/awips2/edex/conf/banner.txt"
# does banner.txt exist?
if [ -f ${EDEX_BANNER_TXT} ]; then
   updateEDEXVersion   
fi

%preun

%postun

%files

%clean
rm -rf ${RPM_BUILD_ROOT}
