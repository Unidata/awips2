%define _ldm_version 6.11.2
%define _ldm_src_tar awips2-ldm.%{_ldm_version}.tar.gz
#
# AWIPS II LDM Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-ldm
Summary: AWIPS II LDM Distribution
Version: %{_ldm_version}
Release: 1
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
Requires: qpid-cpp-client = 0.7.946106-28.el5.centos.1 
Requires: qpid-cpp-client-devel = 0.7.946106-28.el5.centos.1
Requires: zlib-devel
Requires: /usr/lib/libz.a
provides: awips2-ldm
provides: awips2-base-component

%description
AWIPS II LDM Distribution - Contains AWIPS II LDM.

%prep
# Ensure that a "buildroot" has been specified.
if [ "%{_build_root}" = "" ]; then
   echo "ERROR: A BuildRoot has not been specified."
   echo "FATAL: Unable to Continue ... Terminating."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
/bin/mkdir -p %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi

# create the ldm directory
/bin/mkdir -p %{_build_root}/usr/local/ldm-%{_ldm_version}/SOURCES
if [ $? -ne 0 ]; then
   exit 1
fi

# create profile directory and ld directory
/bin/mkdir -p %{_build_root}/etc/ld.so.conf.d
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

%build

%install
_ldm_destination=%{_build_root}/usr/local/ldm-%{_ldm_version}
_ldm_destination_source=${_ldm_destination}/SOURCES

_NATIVELIB_PROJECTS=( 'edexBridge' 'decrypt_file' )
_RPM_directory=%{_baseline_workspace}/rpms
_Installer_ldm=${_RPM_directory}/awips2.core/Installer.ldm

# copy the ldm source to the ldm destination directory.
/bin/cp ${_Installer_ldm}/src/%{_ldm_src_tar} ${_ldm_destination_source}
if [ $? -ne 0 ]; then
   exit 1
fi
# package nativelib projects
pushd . > /dev/null 2>&1
cd %{_baseline_workspace}
if [ $? -ne 0 ]; then
   exit 1
fi
for nativeProject in ${_NATIVELIB_PROJECTS[*]};
do
   /bin/tar -cf ${nativeProject}.tar ${nativeProject}
   if [ $? -ne 0 ]; then
      exit 1
   fi
   # move nativeLib to LDM SOURCES for post-installation
   # build.
   /bin/mv ${nativeProject}.tar \
      ${_ldm_destination_source}/${nativeProject}.tar
   if [ $? -ne 0 ]; then
      exit 1
   fi
done
popd > /dev/null 2>&1

# copy ldm "patches" to SOURCES for post-installation
# unpacking.
cd ${_Installer_ldm}/patch
if [ $? -ne 0 ]; then
   exit 1
fi
_PATCH_DIRS=( 'bin' 'decoders' 'etc' )
for patchDir in ${_PATCH_DIRS[*]};
do
   /bin/tar -cf ${patchDir}.tar ${patchDir}
   if [ $? -ne 0 ]; then
      exit 1
   fi
   /bin/mv ${patchDir}.tar \
      ${_ldm_destination_source}/${patchDir}.tar
   if [ $? -ne 0 ]; then
      exit 1
   fi
done

# copy environment scripts to their destination
/bin/cp profile.d/* %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/touch %{_build_root}/etc/ld.so.conf.d/awips2-ldm-noarch.conf
if [ $? -ne 0 ]; then
   exit 1
fi
echo "/usr/local/ldm-%{_ldm_version}/lib" > \
   %{_build_root}/etc/ld.so.conf.d/awips2-ldm-noarch.conf
if [ $? -ne 0 ]; then
   exit 1
fi
echo "/awips2/qpid/lib" >> \
   %{_build_root}/etc/ld.so.conf.d/awips2-ldm-noarch.conf
if [ $? -ne 0 ]; then
   exit 1
fi

%pre
if [ -d /tmp/ldm ]; then
   rm -rf /tmp/ldm
fi
mkdir -p /tmp/ldm
for dir in etc .ssh;
do
   if [ -d /usr/local/ldm-%{_ldm_version}/${dir} ]; then
      scp -qrp /usr/local/ldm-%{_ldm_version}/${dir} /tmp/ldm
   fi
done

%post
_ldm_dir=/usr/local/ldm-%{_ldm_version}
_ldm_root_dir=${_ldm_dir}/ldm-%{_ldm_version}

pushd . > /dev/null 2>&1
cd ${_ldm_dir}/SOURCES
# unpack the ldm source
/bin/tar -xf awips2-ldm.%{_ldm_version}.tar.gz \
   -C ${_ldm_dir}
if [ $? -ne 0 ]; then
   exit 1
fi
rm -f awips2-ldm.%{_ldm_version}.tar.gz
if [ $? -ne 0 ]; then
   exit 1
fi
# unpack bin, decoders, and etc.
_PATCH_DIRS=( 'bin' 'decoders' 'etc' )
for patchDir in ${_PATCH_DIRS[*]};
do
   /bin/tar -xf ${patchDir}.tar -C ${_ldm_dir}
   if [ $? -ne 0 ]; then
      exit 1
   fi
   /bin/rm -f ${patchDir}.tar
   if [ $? -ne 0 ]; then
      exit 1
   fi
done
/bin/chown -R ldm:fxalpha ${_ldm_dir}
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null 2>&1

# create the ldm directory link
pushd . > /dev/null 2>&1
cd /usr/local
if [ -h /usr/local/ldm ]; then
   # if this command fails, ldm may be a directory
   # instead of a link.
   rm -f /usr/local/ldm
   if [ $? -ne 0 ]; then
      echo "FATAL: failed to remove the /usr/local/ldm link!"
      exit 1
   fi
else
   if [ -d /usr/local/ldm ]; then
      # archive the directory
      _identifier=`date +"%s"`
      mv /usr/local/ldm /usr/local/ldm.archive_${_identifier}
      if [ $? -ne 0 ]; then
         echo "FATAL: failed to archive the /usr/local/ldm directory!"
         exit 1
      fi
      echo "INFO: archived /usr/local/ldm to /usr/local/ldm.archive_${_identifier}."
   fi
fi

ln -s ${_ldm_dir} ldm
if [ $? -ne 0 ]; then
   echo "FATAL: failed to create the /usr/local/ldm link."
   exit 1
fi

# create .bash_profile
echo 'export PATH=$HOME/decoders:$HOME/util:$HOME/bin:$PATH' > \
   /usr/local/ldm/.bash_profile
echo 'export MANPATH=$HOME/share/man:/usr/share/man' >> \
   /usr/local/ldm/.bash_profile
popd > /dev/null 2>&1

# construct pqact
pushd . > /dev/null 2>&1
cd ${_ldm_dir}/etc
if [ $? -ne 0 ]; then
   exit 1
fi
if [ ! -f pqact.conf.template ]; then
   echo "ERROR: pqact.conf.template does not exist."
   exit 1
fi
if [ ! -f pqact.conf.dev ]; then
   echo "ERROR: pqact.conf.dev does not exist."
   exit 1
fi

cp pqact.conf.template pqact.conf
if [ $? -ne 0 ]; then
   exit 1
fi
cat pqact.conf.dev >> pqact.conf
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to merge pqact.conf.dev and pqact.conf."
   exit 1
fi
popd > /dev/null 2>&1

pushd . > /dev/null 2>&1
# build ldm
cd ${_ldm_root_dir}/src
if [ $? -ne 0 ]; then
   exit 1
fi
export _current_dir=`pwd`
su ldm -lc "cd ${_current_dir}; ./configure --disable-max-size --with-noaaport --disable-root-actions" \
   > configure.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: ldm configure has failed!"
   exit 1
fi
export _current_dir=`pwd`
su ldm -lc "cd ${_current_dir}; make install" > install.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: make install has failed!"
   exit 1
fi
popd > /dev/null 2>&1
pushd . > /dev/null 2>&1
cd ${_ldm_root_dir}/src/noaaport
if [ $? -ne 0 ]; then
   exit 1
fi
export _current_dir=`pwd`
su ldm -lc "cd ${_current_dir}; /bin/bash my-make" > my-make.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: my-make has failed!"
   exit 1
fi
popd > /dev/null 2>&1
pushd . > /dev/null 2>&1
cd ${_ldm_root_dir}/src
make root-actions > root-actions.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: root-actions has failed!"
   exit 1
fi
popd > /dev/null 2>&1

# build decrypt_file & edexBridge
pushd . > /dev/null 2>&1
cd ${_ldm_dir}/SOURCES
/bin/tar -xf decrypt_file.tar
if [ $? -ne 0 ]; then
   echo "FATAL: failed to untar decrypt_file.tar!"
   exit 1
fi
/bin/tar -xf edexBridge.tar
if [ $? -ne 0 ]; then
   echo "FATAL: failed to untar edexBridge.tar!"
   exit 1
fi
/bin/rm -f *.tar
if [ $? -ne 0 ]; then
   echo "FATAL: failed to remove edexBridge.tar and decrypt_file.tar!"
   exit 1
fi
/bin/chown -R ldm:fxalpha ${_ldm_dir}/SOURCES
if [ $? -ne 0 ]; then
   echo "FATAL: failed to change owner of ldm SOURCES directory."
   exit 1
fi
cd decrypt_file
if [ $? -ne 0 ]; then
   exit 1
fi
export _current_dir=`pwd`
su ldm -lc "cd ${_current_dir}; gcc -D_GNU_SOURCE -o decrypt_file decrypt_file.c" > \
   decrypt_file.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: failed to build decrypt_file!"
   exit 1
fi
/bin/mv decrypt_file ${_ldm_dir}/decoders/decrypt_file
if [ $? -ne 0 ]; then
   echo "FATAL: failed to move built decrypt_file to ldm decoders directory!"
   exit 1
fi
cd ../edexBridge
if [ $? -ne 0 ]; then
   exit 1
fi
export _current_dir=`pwd`
su ldm -lc "cd ${_current_dir}; g++ edexBridge.cpp -I${_ldm_root_dir}/src/pqact \
   -I${_ldm_root_dir}/include \
   -I${_ldm_root_dir}/src \
   -I/usr/include/qpid \
   -L${_ldm_root_dir}/lib \
   -L%{_libdir} \
   -l ldm -l xml2 -l qpidclient -l qpidcommon -o edexBridge" > \
   edexBridge.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: failed to build edexBridge!"
   exit 1
fi
/bin/mv edexBridge ${_ldm_dir}/bin/edexBridge
if [ $? -ne 0 ]; then
   echo "FATAL: failed to move edexBridge to ldm bin directory!"
   exit 1
fi
cd ..

popd > /dev/null 2>&1

for dir in etc .ssh;
do
   if [ -d /tmp/ldm/${dir} ]; then
      scp -qrp /tmp/ldm/${dir} /usr/local/ldm-%{_ldm_version}
   fi
done
#if a remote CP site, copy over the filtered data configuration
case $SITE_IDENTIFIER in gum|hfo|pbp|vrh)
		echo -e "\nInstalling ldmd.conf for $SITE_IDENTIFIER."
        if ! scp /usr/local/ldm-%{_ldm_version}/etc/ldmd.conf.$SITE_IDENTIFIER cpsbn1:/usr/local/ldm/etc/ldmd.conf
        then
            echo "ERROR: Failed copy of ldmd.conf to cpsbn1"
        fi

        if ! scp /usr/local/ldm-%{_ldm_version}/etc/ldmd.conf.$SITE_IDENTIFIER cpsbn2:/usr/local/ldm/etc/ldmd.conf
        then
            echo "ERROR: Failed copy of ldmd.conf to cpsbn2"
        fi
        ;;
esac

# remove the extra configuration files
rm -f /usr/local/ldm-%{_ldm_version}/etc/ldmd.conf.*

/sbin/ldconfig

# create route-eth1, if it does not already exist.
if [ ! -f /etc/sysconfig/network-scripts/route-eth1 ]; then
   _route_eth1=/etc/sysconfig/network-scripts/route-eth1

   touch ${_route_eth1}
   echo "ADDRESS0=224.0.1.1" > ${_route_eth1}
   echo "NETMASK0=255.255.255.255" >> ${_route_eth1}
   echo "ADDRESS1=224.0.1.2" >> ${_route_eth1}
   echo "NETMASK1=255.255.255.255" >> ${_route_eth1}
   echo "ADDRESS2=224.0.1.3" >> ${_route_eth1}
   echo "NETMASK2=255.255.255.255" >> ${_route_eth1}
   echo "ADDRESS3=224.0.1.4" >> ${_route_eth1}
   echo "NETMASK3=255.255.255.255" >> ${_route_eth1}
   echo "ADDRESS4=224.0.1.5" >> ${_route_eth1}
   echo "NETMASK4=255.255.255.255" >> ${_route_eth1}

   # restart networking
   /sbin/service network restart
fi

rm -rf /tmp/ldm

# remove the ldm SOURCES directory
/bin/rm -rf ${_ldm_dir}/SOURCES
if [ $? -ne 0 ]; then
   exit 1
fi

%preun
%postun
/sbin/ldconfig

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,ldm,fxalpha,-)
%dir /usr/local/ldm-%{_ldm_version}
%dir /usr/local/ldm-%{_ldm_version}/SOURCES
/usr/local/ldm-%{_ldm_version}/SOURCES/*

%attr(644,root,root) /etc/ld.so.conf.d/awips2-ldm-noarch.conf
%attr(755,root,root) /etc/profile.d/awipsLDM.csh
