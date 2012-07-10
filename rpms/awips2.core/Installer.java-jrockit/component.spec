%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

#
# AWIPS II Java JRockit Spec File
#
Name: awips2-java-jrockit
Summary: AWIPS II Java JRockit Distribution - 32 Bit
Version: 1.6.0_26
Release: 1
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-java-jrockit
requires: awips2-java

%description
AWIPS II Java JRockit Distribution - Contains JRockit JDK 1.6.0_26  and 
the JRockit Mission Control Utility.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

rm -rf %{_build_root}
mkdir -p %{_build_root}/build-java
mkdir -p %{_build_root}/awips2/java

%build

%install
RPM_CORE_PROJECT_DIR="%{_baseline_workspace}/rpms/awips2.core"
JROCKIT_PROJECT_DIR="${RPM_CORE_PROJECT_DIR}/Installer.java-jrockit"
JROCKIT_INSTALLER="jrockit-jdk1.6.0_26-R28.1.4-4.0.1-linux-ia32.bin"
SILENT_XML="silent.xml"

pushd . > /dev/null
# JRockit Setup
cd ${JROCKIT_PROJECT_DIR}/src
chmod u+x ${JROCKIT_INSTALLER}
./${JROCKIT_INSTALLER} -mode=silent -silent_xml="${SILENT_XML}"
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

# Copy the Java "Extensions" to build-java.
JROCKIT_SRC_DIR="${JROCKIT_PROJECT_DIR}/src"
JAI_BIN="jai-1_1_3-lib-linux-i586-jdk.bin"
JAI_PATCH="jai.patch1"
JAI_IMAGEIO_BIN="jai_imageio-1_1-lib-linux-i586-jdk.bin"
JAI_IMAGEIO_PATCH="jai_imageio.patch1"

# Prepare
touch %{_build_root}/build-java/yes.txt
echo "yes" > %{_build_root}/build-java/yes.txt

cp -v ${JROCKIT_SRC_DIR}/${JAI_BIN} \
   %{_build_root}/build-java
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

cp -v ${JROCKIT_SRC_DIR}/${JAI_PATCH} \
   %{_build_root}/build-java
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

cp -v ${JROCKIT_SRC_DIR}/${JAI_IMAGEIO_BIN} \
   %{_build_root}/build-java
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

cp -v ${JROCKIT_SRC_DIR}/${JAI_IMAGEIO_PATCH} \
   %{_build_root}/build-java
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

# JAI Setup
# ... Complete Patching.
pushd . > /dev/null
cd %{_build_root}/build-java
patch -i ${JAI_PATCH}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
chmod u+x ${JAI_BIN}
popd > /dev/null
# ... Complete Installation.
pushd . > /dev/null
cd %{_build_root}/awips2/java/jrockit
%{_build_root}/build-java/${JAI_BIN} < %{_build_root}/build-java/yes.txt
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

# JAI ImageIO Setup
# ... Complete Patching.
pushd . > /dev/null
cd %{_build_root}/build-java
patch -i ${JAI_IMAGEIO_PATCH}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
chmod u+x ${JAI_IMAGEIO_BIN}
popd > /dev/null
# ... Complete Installation.
pushd . > /dev/null
cd %{_build_root}/awips2/java/jrockit
%{_build_root}/build-java/${JAI_IMAGEIO_BIN} < %{_build_root}/build-java/yes.txt
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
popd > /dev/null

# pydev certificate
PYDEV_CERTIFICATE="pydev_certificate.cer"
cp -v ${JROCKIT_SRC_DIR}/${PYDEV_CERTIFICATE} \
   %{_build_root}/awips2/java/jrockit/jre/lib/security
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
touch %{_build_root}/build-java/changeit.txt
echo "changeit" > %{_build_root}/build-java/changeit.txt
chmod 666 %{_build_root}/awips2/java/jrockit/jre/lib/security/cacerts
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi
%{_build_root}/awips2/java/jrockit/bin/keytool -import \
   -file %{_build_root}/awips2/java/jrockit/jre/lib/security/pydev_certificate.cer \
   -keystore %{_build_root}/awips2/java/jrockit/jre/lib/security/cacerts \
   -noprompt < %{_build_root}/build-java/changeit.txt
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

rm -rf %{_build_root}/build-java

%pre

%post
# Create a link JRockit Mission Control.
pushd . > /dev/null
cd /awips2/java/bin
ln -sf /awips2/java/jrockit/bin/jrmc .
popd > /dev/null

%preun
# Remove the link JRockit Mission Control.
pushd . > /dev/null
cd /awips2/java/bin
if [ -L jrockit ]; then
   rm -f jrockit
fi
popd > /dev/null

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/java
%dir /awips2/java/jrockit
%dir /awips2/java/jrockit/bin
%dir /awips2/java/jrockit/include
/awips2/java/jrockit/include/*
%dir /awips2/java/jrockit/inventory
/awips2/java/jrockit/inventory/*
%dir /awips2/java/jrockit/jre
/awips2/java/jrockit/jre/*
%dir /awips2/java/jrockit/lib
%dir /awips2/java/jrockit/missioncontrol
/awips2/java/jrockit/missioncontrol/*
%doc /awips2/java/jrockit/THIRDPARTYLICENSEREADME.txt
%doc /awips2/java/jrockit/jre/THIRDPARTYLICENSEREADME.txt
/awips2/java/jrockit/jre/.systemPrefs/.system.lock
/awips2/java/jrockit/jre/.systemPrefs/.systemRootModFile
/awips2/java/jrockit/missioncontrol/.eclipseproduct
%doc /awips2/java/jrockit/COPYRIGHT-jai.txt
%doc /awips2/java/jrockit/COPYRIGHT-jai_imageio.txt
%doc /awips2/java/jrockit/DISTRIBUTIONREADME-jai.txt
%doc /awips2/java/jrockit/DISTRIBUTIONREADME-jai_imageio.txt
%doc /awips2/java/jrockit/ENTITLEMENT-jai_imageio.txt
%doc /awips2/java/jrockit/LICENSE-jai.txt
%doc /awips2/java/jrockit/LICENSE-jai_imageio.txt
%doc /awips2/java/jrockit/THIRDPARTYLICENSEREADME-jai.txt
%doc /awips2/java/jrockit/THIRDPARTYLICENSEREADME-jai_imageio.txt
%doc /awips2/java/jrockit/UNINSTALL-jai
%doc /awips2/java/jrockit/UNINSTALL-jai_imageio

%defattr(755,awips,fxalpha,755)
/awips2/java/jrockit/bin/*
/awips2/java/jrockit/lib/*
/awips2/java/jrockit/jre/bin/*
/awips2/java/jrockit/jre/lib/*