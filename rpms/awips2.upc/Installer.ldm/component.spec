%define _ldm_version 6.13.11
%define _ldm_src_tar ldm-%{_ldm_version}.tar.gz
# ldm-%{_ldm_version}.tar.gz is tarred up ldm-%{_ldm_version}/src dir after
# ISG makes retrans changes
#
# AWIPS LDM Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-ldm
Summary: AWIPS LDM Distribution
Version: %{_component_version}
Release: %{_ldm_version}.%{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: %{_build_arch}
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Requires: awips2
Requires: awips2-qpid-lib
Requires: awips2-python
Requires: perl, pax, gcc, libtool, make
Requires: libxml2-devel, libpng-devel, boost-program-options, gcc-c++
Provides: awips2-ldm
BuildRequires: awips2-python
BuildRequires: awips2-qpid-lib, boost-program-options

%description
AWIPS LDM Distribution

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
_Installer_ldm=%{_baseline_workspace}/rpms/awips2.upc/Installer.ldm
_ldm_root_dir=/awips2/ldm

# copy the ldm source to the destination directory.
/bin/cp ${_Installer_ldm}/src/%{_ldm_src_tar} ${_ldm_destination_source}
if [ $? -ne 0 ]; then
   exit 1
fi

/bin/cp ${_Installer_ldm}/src/edexBridge.cpp ${_ldm_destination_source}
if [ $? -ne 0 ]; then
   exit 1
fi

#_edex_bin=%{_build_root}/awips2/edex/bin
#/bin/mkdir -p ${_edex_bin}
#if [[ $(uname -r |grep el6) ]];then
#  cp ${_Installer_ldm}/edexBridge.el6 ${_edex_bin}/edexBridge
#elif [[ $(uname -r |grep el7) ]];then
#  cp ${_Installer_ldm}/edexBridge.el7 ${_edex_bin}/edexBridge
#fi

# Create patch tar files
cd ${_Installer_ldm}/patch
if [ $? -ne 0 ]; then
   exit 1
fi
_PATCH_DIRS=( 'bin' 'decoders' 'etc' 'dev')
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

# Remove old ldm dir
rm -rf ${_ldm_root_dir}

cp ${_ldm_dir}/SOURCES/%{_ldm_src_tar} ${_ldm_dir}
cd ${_ldm_dir}
gunzip -c %{_ldm_src_tar} | pax -r '-s:/:/src/:'
if [ $? -ne 0 ]; then
   exit 1
fi

# build ldm
. /etc/profile.d/awips2.sh
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
#rm -f libtool
#ln -s /usr/bin/libtool libtool
make install LDMHOME=/awips2/ldm
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
_PATCH_DIRS=( 'decoders' 'etc' 'dev')
for patchDir in ${_PATCH_DIRS[*]};
do
   /bin/tar -xf ${patchDir}.tar -C ${_ldm_dir}
   if [ $? -ne 0 ]; then
      exit 1
   fi
done
_PATCH_DIRS=( 'bin' )
for patchDir in ${_PATCH_DIRS[*]};
do
   /bin/tar -xf ${patchDir}.tar -C ${_ldm_dir}/ldm-%{_ldm_version}/
   if [ $? -ne 0 ]; then
      exit 1
   fi
done
/bin/chmod a+x ${_ldm_dir}/bin/*

g++ edexBridge.cpp -I${_ldm_root_dir}/src/pqact \
   -I${_ldm_root_dir}/include \
   -I${_ldm_root_dir}/src \
   -I/awips2/qpid/include \
   -L${_ldm_root_dir}/lib \
   -L/awips2/qpid/lib \
   -l ldm -l xml2 -l qpidclient -l qpidmessaging -l qpidcommon -l qpidtypes -o edexBridge
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

/sbin/ldconfig > /dev/null 2>&1

if [ ! -h /awips2/ldm/logs ]; then
  ln -s /awips2/ldm/var/logs /awips2/ldm/
fi
if [ ! -h /awips2/ldm/data ]; then
  ln -s /awips2/ldm/var/data /awips2/ldm/
fi
if getent passwd awips &>/dev/null; then
  /bin/chown -R awips:fxalpha ${_ldm_dir}
  cd /awips2/ldm/src/
  make install_setuids > /dev/null 2>&1
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

su - awips -c "regutil /queue/size -s 2500M"

%preun
%postun
/sbin/ldconfig > /dev/null 2>&1

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,awips,fxalpha,-)
%dir /awips2/ldm
%dir /awips2/ldm/SOURCES
/awips2/ldm/SOURCES/*
%attr(755,root,root) /etc/init.d/edex_ldm
%attr(600,awips,fxalpha) /var/spool/cron/awips
#%attr(755,awips,fxalpha) /awips2/edex/bin/edexBridge
%attr(755,root,root) /etc/ld.so.conf.d/awips2-ldm.conf
%attr(755,root,root) /etc/logrotate.d/ldm.log
