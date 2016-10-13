%define _ldm_version 6.13.3
%define _ldm_src_tar ldm-%{_ldm_version}.tar.gz
# ldm-%{_ldm_version}.tar.gz is tarred up ldm-%{_ldm_version}/src dir after
# ISG makes retrans changes
#
# AWIPS II LDM Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-ldm
Summary: AWIPS II LDM Distribution
Version: %{_component_version}.%{_component_release}
Release: %{_ldm_version}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Requires: awips2-qpid-lib
requires: awips2-python
requires: compat-gcc-34-g77
requires: pax, gcc, libxml2-devel
requires: libtool, libpng-devel
provides: awips2-ldm

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
/bin/mkdir -p %{_build_root}/awips2/ldm/SOURCES
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
/bin/mkdir -p %{_build_root}/var/spool/cron/
if [ $? -ne 0 ]; then
   exit 1
fi

_ldm_destination=%{_build_root}/awips2/ldm
_ldm_destination_source=${_ldm_destination}/SOURCES
_NATIVELIB_PROJECTS=( 'decrypt_file' )
_RPM_directory=%{_baseline_workspace}/rpms
_Installer_ldm=${_RPM_directory}/awips2.upc/Installer.ldm

# copy the ldm source to the destination directory.
/bin/cp ${_Installer_ldm}/src/%{_ldm_src_tar} ${_ldm_destination_source}
if [ $? -ne 0 ]; then
   exit 1
fi
# package nativelib projects
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

# Create patch tar files
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
/bin/cp cron/* %{_build_root}/var/spool/cron/
if [ $? -ne 0 ]; then
   exit 1
fi

%pre
DATE=`date -u +%Y%m%d`
# Preserve the user etc directory before upgrading
if [ -f /awips2/ldm/etc/ldmd.conf ]; then
   if [ -d /tmp/ldm ]; then
      rm -rf /tmp/ldm
   fi
   mkdir -p /tmp/ldm
   cp -rp /awips2/ldm/etc/ldmd.conf /tmp/ldm/ldmd.${DATE}
   cp -rp /awips2/ldm/etc/pqact.conf /tmp/ldm/pqact.${DATE}
fi

%post
_ldm_dir=/awips2/ldm
_ldm_root_dir=${_ldm_dir}/ldm-%{_ldm_version}
_myHost=`hostname`
_myHost=`echo ${_myHost} | cut -f1 -d'-'`

# Remove old ldm dir
rm -rf ${_ldm_root_dir}

cp ${_ldm_dir}/SOURCES/%{_ldm_src_tar} ${_ldm_dir}
cd ${_ldm_dir}
gunzip -c %{_ldm_src_tar} | pax -r '-s:/:/src/:'
if [ $? -ne 0 ]; then
   exit 1
fi
rm -f %{_ldm_src_tar}
if [ $? -ne 0 ]; then
   exit 1
fi

# build ldm
rm -f /awips2/ldm/runtime
cd ${_ldm_root_dir}/src
if [ $? -ne 0 ]; then
   exit 1
fi
./configure --disable-max-size --disable-root-actions --prefix=${_ldm_root_dir} CFLAGS='-g -O0' > configure.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: ldm configure has failed!"
   exit 1
fi
# Fix libtool incompatibility in source tar ball
rm -f libtool
ln -s /usr/bin/libtool libtool
make LDMHOME=/awips2/ldm > make.log 2>&1
make install LDMHOME=/awips2/ldm > install.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: make install has failed!"
   exit 1
fi
make root-actions LDMHOME=/awips2/ldm > root-actions.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: root-actions has failed!"
   exit 1
fi
# Unpack patch tar files
cd ${_ldm_dir}/SOURCES
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

# build decrypt_file
cd ${_ldm_dir}/SOURCES
/bin/tar -xf decrypt_file.tar
if [ $? -ne 0 ]; then
   echo "FATAL: failed to untar decrypt_file.tar!"
   exit 1
fi
#/bin/tar -xf edexBridge.tar
#if [ $? -ne 0 ]; then
#   echo "FATAL: failed to untar edexBridge.tar!"
#   exit 1
#fi
/bin/rm -f *.tar
if [ $? -ne 0 ]; then
   echo "FATAL: failed to remove decrypt_file.tar!"
   exit 1
fi
cd decrypt_file
if [ $? -ne 0 ]; then
   exit 1
fi
gcc -D_GNU_SOURCE -o decrypt_file decrypt_file.c
if [ $? -ne 0 ]; then
   echo "FATAL: failed to build decrypt_file!"
   exit 1
fi
/bin/mv decrypt_file ${_ldm_dir}/decoders/decrypt_file
if [ $? -ne 0 ]; then
   echo "FATAL: failed to move built decrypt_file to ldm decoders directory!"
   exit 1
fi
#cd ../edexBridge
#if [ $? -ne 0 ]; then
#   exit 1
#fi
#g++ edexBridge.cpp -I${_ldm_root_dir}/src/pqact \
#   -I${_ldm_root_dir}/include \
#   -I${_ldm_root_dir}/src \
#   -I/awips2/qpid/include \
#   -L${_ldm_root_dir}/lib \
#   -L/awips2/qpid/lib \
#   -l ldm -l xml2 -l qpidclient -l qpidmessaging -l qpidcommon -l qpidtypes -o edexBridge
#if [ $? -ne 0 ]; then
#   echo "FATAL: failed to build edexBridge!"
#   exit 1
#fi
#/bin/mv edexBridge ${_ldm_dir}/bin/edexBridge
#if [ $? -ne 0 ]; then
#   echo "FATAL: failed to move edexBridge to ldm bin directory!"
#   exit 1
#fi
cd ..

/sbin/ldconfig

/bin/rm -rf ${_ldm_dir}/SOURCES
if [ $? -ne 0 ]; then
   exit 1
fi

if [ ! -h /awips2/ldm/logs ]; then
  ln -s /awips2/ldm/var/logs /awips2/ldm/
fi
if [ ! -h /awips2/ldm/data ]; then
  ln -s /awips2/ldm/var/data /awips2/ldm/
fi
if getent passwd awips &>/dev/null; then
  /bin/chown -R awips:fxalpha ${_ldm_dir} /awips2/data_store
  cd /awips2/ldm/src/
  make install_setuids
else
  echo "--- Warning: group fxalpha does not exist"
  echo "--- you will need to check owner/group/permissions for /awips2/ldm"
  echo "tried to run 'chown -R awips:fxalpha /awips2/ldm; cd /awips2/ldm/src/; make install_setuids'"
  echo ""
fi

# Copy back local ldm
if [ -d /tmp/ldm/ ]; then
   cp -rp /tmp/ldm/pqact.* /awips2/ldm/etc/
   cp -rp /tmp/ldm/ldmd.* /awips2/ldm/etc/
fi

%preun
%postun
/sbin/ldconfig

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,awips,fxalpha,-)
%dir /awips2/ldm
%dir /awips2/ldm/SOURCES
/awips2/ldm/SOURCES/*
%attr(755,root,root) /etc/init.d/edex_ldm
%attr(600,awips,fxalpha) /var/spool/cron/awips
%attr(755,root,root) /etc/ld.so.conf.d/awips2-ldm.conf
%attr(755,root,root) /etc/logrotate.d/ldm.log
