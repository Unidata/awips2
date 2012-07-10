#
# AWIPS II Eclipse Spec File
#

# --define arguments:
#   %{_uframe_eclipse}
#   %{_build_root}
#   %{_baseline_workspace}

Name: awips2-eclipse
Summary: AWIPS II Eclipse Distribution
Version: 3.6.1
Release: 1
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-eclipse

%description
AWIPS II Eclipse Distribution - Contains the AWIPS II Eclipse Distribution.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
mkdir -p %{_build_root}/awips2/eclipse

%build

%install
# The location of the awips2 eclipse source directory will be
# specified as a command line argument. Fail if the specified
# directory cannot be found.
if [ ! -d %{_uframe_eclipse} ]; then
   echo "ERROR: Unable To Find The AWIPS II Eclipse Distribution."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

# Copy the uframe eclipse distribution.
cp -r %{_uframe_eclipse}/* %{_build_root}/awips2/eclipse

# Copy eclipse.sh to our build-directory.
cp %{_baseline_workspace}/rpms/awips2.ade/Installer.eclipse/scripts/* \
   %{_build_root}/awips2/eclipse
   
# delete the basemaps and etc links
rm -f %{_build_root}/awips2/eclipse/basemaps
rm -f %{_build_root}/awips2/eclipse/etc

%pre
JAVA_INSTALL="<Not Installed>"
PYTHON_INSTALL="<Not Installed>"
ANT_INSTALL="<Not Installed>"

INSTALL_PATH="/awips2/java"
if [ -d ${INSTALL_PATH} ]; then
   JAVA_INSTALL=${INSTALL_PATH}
fi

INSTALL_PATH="/awips2/python"
if [ -d ${INSTALL_PATH} ]; then
   PYTHON_INSTALL=${INSTALL_PATH}
fi

INSTALL_PATH="/awips2/ant"
if [ -d ${INSTALL_PATH} ]; then
   ANT_INSTALL=${INSTALL_PATH}
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II Eclipse Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Java   Detected At: ${JAVA_INSTALL}\e[m"
echo -e "\e[1;34m   Python Detected At: ${PYTHON_INSTALL}\e[m"
echo -e "\e[1;34m   Ant    Detected At: ${ANT_INSTALL}\e[m"

%post
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Creating ADE Eclipse Desktop Shortcut...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
ADE_ECLIPSE_SHORTCUT="ade-eclipse"
SHORTCUT_OWNER="${USER}"
CREATE_SHORTCUT="true"
if [ ! "${SUDO_USER}" = "" ]; then
   SHORTCUT_OWNER="${SUDO_USER}"
fi
echo -e "\e[1;34m   Creating Shortcut For User: ${SHORTCUT_OWNER}\e[m"

USER_HOME_DIR="~${SHORTCUT_OWNER}"
if [ ! -d ${USER_HOME_DIR} ]; then
   USER_HOME_DIR="/home/${SHORTCUT_OWNER}"
   echo "   (Assuming User Home Directory Is Under '/home')"
fi

if [ ! -d ${USER_HOME_DIR}/Desktop ]; then
   echo -e "\e[1;31m   ERROR: Unable To Find The User's Desktop!!!"
   CREATE_SHORTCUT="false"
fi

if [ "${CREATE_SHORTCUT}" = "true" ]; then
   SHORTCUT_TMP="${USER_HOME_DIR}/Desktop/${ADE_ECLIPSE_SHORTCUT}.tmp"
   SHORTCUT="${USER_HOME_DIR}/Desktop/${ADE_ECLIPSE_SHORTCUT}.desktop"
   
   if [ -f ${SHORTCUT} ]; then
      echo -n "   Attempting To Remove The Existing Shortcut ... "
      sudo -u ${SHORTCUT_OWNER} rm -f ${SHORTCUT}
      if [ ! -f ${SHORTCUT} ]; then
         echo -n "SUCCESS"
      else
         echo -n "FAILURE"
      fi
      echo ""
   fi
   sudo -u ${SHORTCUT_OWNER} touch ${SHORTCUT_TMP}
   sudo -u ${SHORTCUT_OWNER} chmod 666 ${SHORTCUT_TMP}

   echo "[Desktop Entry]" >> ${SHORTCUT_TMP}
   echo "Version=1.0" >> ${SHORTCUT_TMP}
   echo "Encoding=UTF-8" >> ${SHORTCUT_TMP}
   echo "Name=ADE Eclipse" >> ${SHORTCUT_TMP}
   echo "GenericName=Eclipse" >> ${SHORTCUT_TMP}
   echo "Comment=IDE" >> ${SHORTCUT_TMP}
   echo "Exec=/bin/bash -i -c \"xterm -title 'AWIPS II ADE Eclipse' -e '/awips2/eclipse/eclipseShortcutWrap.sh'\"" >> ${SHORTCUT_TMP}
   echo "Icon=/awips2/eclipse/icon.xpm" >> ${SHORTCUT_TMP}
   echo "Terminal=false" >> ${SHORTCUT_TMP}
   echo "Type=Application" >> ${SHORTCUT_TMP}
   echo "Categories=Development;IDE;" >> ${SHORTCUT_TMP}

   sudo -u ${SHORTCUT_OWNER} mv ${SHORTCUT_TMP} ${SHORTCUT}
   sudo -u ${SHORTCUT_OWNER} chmod 644 ${SHORTCUT}
fi

echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II Eclipse Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/eclipse
/awips2/eclipse/*
%defattr(755,awips,fxalpha,755)
/awips2/eclipse/about.html
/awips2/eclipse/artifacts.xml
/awips2/eclipse/eclipse
/awips2/eclipse/eclipse.ini
/awips2/eclipse/eclipse.sh
/awips2/eclipse/eclipseShortcutWrap.sh
/awips2/eclipse/epl-v10.html
/awips2/eclipse/icon.xpm
/awips2/eclipse/libcairo-swt.so
/awips2/eclipse/notice.html