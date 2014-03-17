%define _ldm_version 6.11.5
%define _ldm_src_tar ldm-%{_ldm_version}.tar.gz
# ldm-%{_ldm_version}.tar.gz is tarred up ldm-%{_ldm_version}/src dir after
# ISG makes retrans changes
#
# AWIPS II LDM Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-ldm
Summary: AWIPS II LDM Distribution
Version: %{_ldm_version}
Release: 10
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
Requires: awips2-qpid-lib
requires: awips2-python
requires: awips2-python
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

%build

%install

# create the ldm directory
/bin/mkdir -p %{_build_root}/usr/local/ldm/SOURCES
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/ld.so.conf.d
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/logrotate.d
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi


_ldm_destination=%{_build_root}/usr/local/ldm
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

/bin/cp ld.so.conf.d/* %{_build_root}/etc/ld.so.conf.d
if [ $? -ne 0 ]; then
   exit 1
fi

/bin/cp logrotate.d/* %{_build_root}/etc/logrotate.d
if [ $? -ne 0 ]; then
   exit 1
fi

/bin/cp init.d/* %{_build_root}/etc/init.d
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
   if [ -d /usr/local/ldm/${dir} ]; then
      scp -qrp /usr/local/ldm/${dir} /tmp/ldm
   fi
done

%post
_ldm_dir=/usr/local/ldm
_ldm_root_dir=${_ldm_dir}/ldm-%{_ldm_version}
_myHost=`hostname`
_myHost=`echo ${_myHost} | cut -f1 -d'-'`

pushd . > /dev/null 2>&1
cd ${_ldm_dir}/SOURCES
# unpack the ldm source
/bin/tar -xf %{_ldm_src_tar} \
   -C ${_ldm_dir}
if [ $? -ne 0 ]; then
   exit 1
fi
rm -f %{_ldm_src_tar}
if [ $? -ne 0 ]; then
   exit 1
fi
chown -R ldm:fxalpha ${_ldm_dir}

# create .bash_profile
if [ ! -f /usr/local/ldm/.bash_profile ]; then
   echo 'umask 002' > \
      /usr/local/ldm/.bash_profile
   echo 'export PATH=$HOME/decoders:$HOME/util:$HOME/bin:$PATH' >> \
      /usr/local/ldm/.bash_profile
   echo 'export MANPATH=$HOME/share/man:/usr/share/man' >> \
      /usr/local/ldm/.bash_profile
   /bin/chown ldm:fxalpha /usr/local/ldm/.bash_profile
fi

pushd . > /dev/null 2>&1
# build ldm
rm -f ~ldm/runtime
cd ${_ldm_root_dir}/src
if [ $? -ne 0 ]; then
   exit 1
fi
export _current_dir=`pwd`
su ldm -lc "cd ${_current_dir}; ./configure --disable-max-size --with-noaaport --disable-root-actions --prefix=${_ldm_root_dir} CFLAGS='-g -O0'" \
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

su ldm -lc "cd ${_current_dir}; /bin/bash my-install" > my-install.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: my-install has failed!"
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
/bin/chmod a+x ${_ldm_dir}/bin/*
/bin/chown ldm:fxalpha ${_ldm_root_dir}/bin
/bin/chown -R ldm:fxalpha ${_ldm_dir}/etc ${_ldm_dir}/decoders
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

if [ ${_myHost} != "cpsbn1" -a ${_myHost} != "cpsbn2" -a ${_myHost} != "dx1" -a ${_myHost} != "dx2" ] ; then
   cat pqact.conf.dev >> pqact.conf
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to merge pqact.conf.dev and pqact.conf."
      exit 1
   fi
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
   -I/awips2/qpid/include \
   -L${_ldm_root_dir}/lib \
   -L/awips2/qpid/lib \
   -l ldm -l xml2 -l qpidclient -l qpidmessaging -l qpidcommon -l qpidtypes -o edexBridge" > \
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

if [ ! -d .ssh ] && 
   [ -d /tmp/ldm/.ssh ]; then
   scp -qrp /tmp/ldm/.ssh /usr/local/ldm
fi

for _file in $( ls /tmp/ldm/etc/pqact.conf.* | grep -wE "pqact.conf.[a-z]{3,4}" | grep -v pqact.conf.dev | xargs ) ;
do
   if [[ ! -f /usr/local/ldm/etc/${_file} ]]; then
      scp -qp ${_file} /usr/local/ldm/etc/
   fi
done
#if a remote CP site, copy over the filtered data configuration
if [ ${_myHost} == "dx1" -o ${_myHost} == "dx2" ] ; then
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
fi

# remove the extra configuration files
rm -f /usr/local/ldm/etc/ldmd.conf.*

/sbin/ldconfig

# create route-eth1, if it does not already exist.
if [ ${_myHost} == "cpsbn1" -o ${_myHost} == "cpsbn2" ] ; then
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
   
   # check for some AWIPS specific links for the CP devices
   for _dirs in data logs ; do
      if [[ -h /usr/local/ldm/${_dirs} && $(readlink /usr/local/ldm/${_dirs}) != "/data/ldm/${_dirs}" ]] ; then
         if ! rm -f /usr/local/ldm/${_dirs} ; then
            echo "ERROR: Failed to remove /usr/local/ldm/${_dirs}"
         else
            if ! ln -s /data/ldm/${_dirs} /usr/local/ldm/${_dirs} ; then
               echo "ERROR: Failed to create link from /usr/local/ldm/${_dirs} --> /data/ldm/${_dirs}"
            fi
         fi
      fi
   done
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
%dir /usr/local/ldm
%dir /usr/local/ldm/SOURCES
/usr/local/ldm/SOURCES/*

%attr(755,root,root) /etc/profile.d/awipsLDM.csh
%attr(755,root,root) /etc/ld.so.conf.d/awips2-ldm.conf
%attr(755,root,root) /etc/logrotate.d/ldm.log
%attr(755,root,root) /etc/init.d/ldmcp
