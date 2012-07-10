%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

#
# AWIPS II Java Spec File
#
Name: awips2-java
Summary: AWIPS II Java Distribution
Version: 1.6.0_27
Release: 1
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-java

%description
AWIPS II Java Distribution - Contains Java SE Development Kit (JDK) 1.6.0_27 
plus additional libraries used by AWIPS II.

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
mkdir -p %{_build_root}/awips2
mkdir -p %{_build_root}/etc/profile.d

%build

%install
JDK_BIN="jdk-6u27-linux-i586.bin"
JDK_BIN_var_javahome="jdk1.6.0_27"
JAI_BIN="jai-1_1_3-lib-linux-i586-jdk.bin"
JAI_PATCH="jai.patch1"
JAI_IMAGEIO_BIN="jai_imageio-1_1-lib-linux-i586-jdk.bin"
JAI_IMAGEIO_PATCH="jai_imageio.patch1"
PYDEV_CER="pydev_certificate.cer"
JAVA_SRC_DIR="%{_baseline_workspace}/Installer.rpm/awips2.core/Installer.java/src"

cp -v ${JAVA_SRC_DIR}/${JDK_BIN} \
   %{_build_root}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi
pushd .
cd %{_build_root}/awips2
chmod u+x ${JDK_BIN}
if [ $? -ne 0 ]; then
   exit 1
fi
./${JDK_BIN} -noregister
if [ $? -ne 0 ]; then
   exit 1
fi
mv ${JDK_BIN_var_javahome} java
if [ $? -ne 0 ]; then
   exit 1
fi
rm -fv ${JDK_BIN}
if [ $? -ne 0 ]; then
   exit 1
fi

cd ${JAVA_SRC_DIR}
patch -i ${JAI_PATCH} -o %{_build_root}/build-java/${JAI_BIN}
if [ $? -ne 0 ]; then
   exit 1
fi
patch -i ${JAI_IMAGEIO_PATCH} -o %{_build_root}/build-java/${JAI_IMAGEIO_BIN}
if [ $? -ne 0 ]; then
   exit 1
fi
cp -v ${PYDEV_CER} \
   %{_build_root}/awips2/java/jre/lib/security
if [ $? -ne 0 ]; then
   exit 1
fi

# Used to automatically agree to software licenses.
touch %{_build_root}/build-java/yes.txt
echo "yes" > %{_build_root}/build-java/yes.txt

cd %{_build_root}/awips2/java
chmod u+x %{_build_root}/build-java/${JAI_BIN}
if [ $? -ne 0 ]; then
   exit 1
fi
%{_build_root}/build-java/${JAI_BIN} \
   < %{_build_root}/build-java/yes.txt
if [ $? -ne 0 ]; then
   exit 1
fi
chmod u+x %{_build_root}/build-java/${JAI_IMAGEIO_BIN}
if [ $? -ne 0 ]; then
   exit 1
fi
%{_build_root}/build-java/${JAI_IMAGEIO_BIN} \
   < %{_build_root}/build-java/yes.txt
if [ $? -ne 0 ]; then
   exit 1
fi

touch %{_build_root}/build-java/changeit.txt
echo "changeit" > %{_build_root}/build-java/changeit.txt
chmod 666 %{_build_root}/awips2/java/jre/lib/security/cacerts
if [ $? -ne 0 ]; then
   exit 1
fi
%{_build_root}/awips2/java/bin/keytool -import \
   -file %{_build_root}/awips2/java/jre/lib/security/pydev_certificate.cer \
   -keystore %{_build_root}/awips2/java/jre/lib/security/cacerts \
   -noprompt < %{_build_root}/build-java/changeit.txt
if [ $? -ne 0 ]; then
   exit 1
fi
popd

# The profile.d environment scripts.
JAVA_SCRIPTS_DIR="%{_baseline_workspace}/Installer.rpm/awips2.core/Installer.java/scripts"
cp -v ${JAVA_SCRIPTS_DIR}/profile.d/* \
   %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

# The licenses
mkdir -p %{_build_root}/awips2/java/licenses
LEGAL_DIR="%{_baseline_workspace}/Installer.rpm/legal"
cp -v ${LEGAL_DIR}/*.txt ${LEGAL_DIR}/*.pdf \
   %{_build_root}/awips2/java/licenses
if [ $? -ne 0 ]; then
   exit 1
fi

rm -rf %{_build_root}/build-java

%pre
if [ "${1}" = "2" ]; then
   # Upgrade. Removing the existing /awips2/java/man
   # directory to prevent conflicts.
   if [ -d /awips2/java/man ]; then
      rm -rf /awips2/java/man
      if [ $? -ne 0 ]; then
         echo "ERROR: The awips2-java upgrade has FAILED."
         exit 1
      fi
   fi
fi

%post

%preun

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2Java.csh
%attr(755,root,root) /etc/profile.d/awips2Java.sh
%dir /awips2/java
%dir /awips2/java/bin

%doc /awips2/java/COPYRIGHT
%doc /awips2/java/COPYRIGHT-jai_imageio.txt
%doc /awips2/java/COPYRIGHT-jai.txt
%doc /awips2/java/DISTRIBUTIONREADME-jai_imageio.txt
%doc /awips2/java/DISTRIBUTIONREADME-jai.txt
%doc /awips2/java/ENTITLEMENT-jai_imageio.txt
%doc /awips2/java/jre/README
%doc /awips2/java/jre/THIRDPARTYLICENSEREADME.txt
%doc /awips2/java/jre/Welcome.html
%doc /awips2/java/jre/LICENSE
%doc /awips2/java/LICENSE
%doc /awips2/java/LICENSE-jai_imageio.txt
%doc /awips2/java/LICENSE-jai.txt
%docdir /awips2/java/licenses
%dir /awips2/java/licenses
/awips2/java/licenses/*
%docdir /awips2/java/man
%dir /awips2/java/man
/awips2/java/man/*
%doc /awips2/java/README.html
%doc /awips2/java/register.html
%doc /awips2/java/register_ja.html
%doc /awips2/java/register_zh_CN.html
%doc /awips2/java/THIRDPARTYLICENSEREADME-jai_imageio.txt
%doc /awips2/java/THIRDPARTYLICENSEREADME-jai.txt
%doc /awips2/java/THIRDPARTYLICENSEREADME.txt
%doc /awips2/java/UNINSTALL-jai
%doc /awips2/java/UNINSTALL-jai_imageio
%doc /awips2/java/jre/COPYRIGHT

%dir /awips2/java/db
/awips2/java/db/*
%dir /awips2/java/demo
/awips2/java/demo/*
%dir /awips2/java/include
/awips2/java/include/*
%dir /awips2/java/jre
%dir /awips2/java/jre/.systemPrefs
/awips2/java/jre/.systemPrefs/.*
#/awips2/java/jre/.systemPrefs/.systemRootModFile
%dir /awips2/java/jre/bin

/awips2/java/jre/javaws
%dir /awips2/java/jre/lib

%dir /awips2/java/jre/plugin
/awips2/java/jre/plugin/*
%dir /awips2/java/lib

/awips2/java/sample
/awips2/java/src.zip

%defattr(755,awips,fxalpha,755)
/awips2/java/bin/*
/awips2/java/lib/*
/awips2/java/jre/bin/*
/awips2/java/jre/lib/*