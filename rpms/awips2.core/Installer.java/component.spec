%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')
%define _java_version 1.6.0_43 
%define _build_arch %(uname -i)

#
# AWIPS II Java Spec File
#
Name: awips2-java
Summary: AWIPS II Java Distribution
Version: %{_java_version}
Release: 1
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: %{_build_arch}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-java

%description
AWIPS II Java Distribution - Contains Java SE Development Kit (JDK) 1.6.0_43 
plus additional libraries used by AWIPS II.

%prep
# Ensure that a "buildroot" has been specified.
if [ "%{_build_root}" = "" ]; then
   echo "ERROR: A BuildRoot has not been specified."
   echo "FATAL: Unable to Continue ... Terminating."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir -p %{_build_root}/build-java
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/awips2/java
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

%build

%install
build_arch=
arch_directory=
# determine the architecture based on the system architecture.
if [ "%{_build_arch}" = "x86_64" ]; then
   build_arch="amd64"
   arch_directory="x86_64"
   jdk_arch="x64"
else
   if [ "%{_build_arch}" = "i386" ]; then
      build_arch="i586"
      arch_directory="i386"
      jdk_arch=${build_arch}
   else
      echo "ERROR: Unrecognized architecture '%{_build_arch}."
      exit 1
   fi
fi

JDK_BIN_var_javahome="jdk%{_java_version}"
jdk_bin="jdk-6u43-linux-${jdk_arch}.bin"
jai_bin="jai-1_1_3-lib-linux-${build_arch}-jdk.bin"
jai_imageio_bin="jai_imageio-1_1-lib-linux-${build_arch}-jdk.bin"
jai_bin_patch="jai.patch1"
jai_imageio_bin_patch="jai_imageio.patch1"
pydev_cert="pydev_certificate.cer"

# locate the java src.
CORE_PROJECT_DIR="%{_baseline_workspace}/rpms/awips2.core"
JAVA_SRC_DIR="${CORE_PROJECT_DIR}/Installer.java/src"
JAVA_SCRIPTS_DIR="${CORE_PROJECT_DIR}/Installer.java/scripts"
JAVA_ARCH_SRC_DIR="${JAVA_SRC_DIR}/${arch_directory}"

pushd . > /dev/null
cd ${JAVA_ARCH_SRC_DIR}
/bin/cp -v ${jdk_bin} %{_build_root}/build-java
if [ $? -ne 0 ]; then
   exit 1
fi
/usr/bin/patch -i ${jai_bin_patch} \
   -o %{_build_root}/build-java/${jai_bin}
if [ $? -ne 0 ]; then
   exit 1
fi
/usr/bin/patch -i ${jai_imageio_bin_patch} \
   -o %{_build_root}/build-java/${jai_imageio_bin}
if [ $? -ne 0 ]; then
   exit 1
fi

chmod a+x %{_build_root}/build-java/*.bin
cd %{_build_root}/awips2/java
# Used to automatically agree to software licenses.
touch yes.txt
echo "yes" > yes.txt

%{_build_root}/build-java/${jdk_bin} -noregister
if [ $? -ne 0 ]; then
   exit 1
fi

/bin/mv ${JDK_BIN_var_javahome}/* .
if [ $? -ne 0 ]; then
   exit 1
fi
rm -rfv ${JDK_BIN_var_javahome}
if [ $? -ne 0 ]; then
   exit 1
fi
%{_build_root}/build-java/${jai_bin} < yes.txt
if [ $? -ne 0 ]; then
   exit 1
fi
%{_build_root}/build-java/${jai_imageio_bin} < yes.txt
if [ $? -ne 0 ]; then
   exit 1
fi

rm -fv yes.txt
if [ $? -ne 0 ]; then
   exit 1
fi
rm -rf %{_build_root}/build-java
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null

# Our profile.d scripts.
JAVA_PROFILED_DIR="${JAVA_SCRIPTS_DIR}/profile.d"
cp -v ${JAVA_PROFILED_DIR}/* %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

# The pydev certificate.
cp -v ${JAVA_SRC_DIR}/${pydev_cert} \
   %{_build_root}/awips2/java/jre/lib/security
if [ $? -ne 0 ]; then
   exit 1
fi
touch changeit.txt
echo "changeit" > changeit.txt
chmod 666 %{_build_root}/awips2/java/jre/lib/security/cacerts
if [ $? -ne 0 ]; then
   exit 1
fi
%{_build_root}/awips2/java/bin/keytool -import \
   -file %{_build_root}/awips2/java/jre/lib/security/pydev_certificate.cer \
   -keystore %{_build_root}/awips2/java/jre/lib/security/cacerts \
   -noprompt < changeit.txt
rm -fv changeit.txt
if [ $? -ne 0 ]; then
   exit 1
fi

# The licenses
mkdir -p %{_build_root}/awips2/java/licenses
LEGAL_DIR="%{_baseline_workspace}/rpms/legal"
cp -v ${LEGAL_DIR}/*.txt ${LEGAL_DIR}/*.pdf \
   %{_build_root}/awips2/java/licenses
if [ $? -ne 0 ]; then
   exit 1
fi

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

/awips2/java/src.zip

%defattr(755,awips,fxalpha,755)
/awips2/java/bin/*
/awips2/java/lib/*
/awips2/java/jre/bin/*
/awips2/java/jre/lib/*