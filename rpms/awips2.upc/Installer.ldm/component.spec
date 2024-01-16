%define _ldm_version 6.14.5
%define _ldm_src_tar ldm-%{_ldm_version}.tar.gz
#
# AWIPS II LDM Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-ldm
Summary: AWIPS II LDM Distribution
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
Requires: openssh-clients
Requires: awips2-qpid-proton%{?_isa}
Requires: perl
Requires: libxml2
Requires: libxml2-devel
Requires: pax
Requires: libpng
Requires: glibc-common
Requires: zlib
Requires: awips2-python
Requires: awips2-python-numpy
Requires: awips2-python-awips
Requires: awips2-qpid-proton-python
Requires: awips2-watchdog

BuildRequires: make
BuildRequires: gzip
BuildRequires: gcc-c++
BuildRequires: libtool
BuildRequires: zlib-devel
BuildRequires: xz-devel
BuildRequires: libxml2-devel
BuildRequires: libpng-devel
BuildRequires: perl
BuildRequires: glibc-devel
BuildRequires: awips2-qpid-proton

Provides: awips2-ldm

%description
AWIPS II LDM Distribution

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

_myHost=`hostname`
_myHost=`echo ${_myHost} | cut -f1 -d'-'`

# create the ldm directory
/bin/mkdir -p %{_build_root}/awips2/ldm/SOURCES
if [ $? -ne 0 ]; then
   exit 1
fi

/bin/mkdir -p %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi

/bin/mkdir -p %{_build_root}/etc/ld.so.conf.d
if [ $? -ne 0 ]; then
   exit 1
fi

/bin/mkdir -p %{_build_root}/etc/watchdog.d
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

# copy the ldm source to the ldm destination directory.
/bin/cp ${_Installer_ldm}/src/%{_ldm_src_tar} ${_ldm_destination_source}
if [ $? -ne 0 ]; then
   exit 1
fi
echo ${_ldm_destination_source}

	
#/bin/cp ${_Installer_ldm}/src/edexBridge.cpp ${_ldm_destination_source}
#if [ $? -ne 0 ]; then
#   exit 1
#fi

/bin/cp ${_Installer_ldm}/patch/watchdog.d/ldm_watchdog.sh %{_build_root}/etc/watchdog.d

#patch edex post
#patch -u pqact/filel.c -i ${_Installer_ldm}/patch/edex_post/edex_post.patch || exit 1

# apply pqact patch
#patch -u pqact/pqact.c -i ${_Installer_ldm}/patch/pqact/pqact.patch || exit 1

# Create patch tar files
cd ${_Installer_ldm}/patch
if [ $? -ne 0 ]; then
   exit 1
fi
_PATCH_DIRS=( 'bin' 'decoders' 'etc' 'dev' 'gempak' 'edex_post' 'pqact')
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

# Remove old ldm dir
rm -rf ${_ldm_root_dir}

%post
_ldm_dir=/awips2/ldm
_ldm_root_dir=${_ldm_dir}/ldm-%{_ldm_version}

cp ${_ldm_dir}/SOURCES/%{_ldm_src_tar} ${_ldm_dir}
cd ${_ldm_dir}
gunzip -c %{_ldm_src_tar} | pax -r '-s:/:/src/:'
if [ $? -ne 0 ]; then
   exit 1
fi

# Unpack patch tar files
cd ${_ldm_dir}/SOURCES
_PATCH_DIRS=( 'edex_post' 'pqact')
for patchDir in ${_PATCH_DIRS[*]};
do
   /bin/tar -xf ${patchDir}.tar
   if [ $? -ne 0 ]; then
      exit 1
   fi
done

# build ldm
. /etc/profile.d/awips2.sh
rm -f /awips2/ldm/runtime
cd ${_ldm_root_dir}/src
if [ $? -ne 0 ]; then
   exit 1
fi


#patch edex post
patch -u pqact/filel.c -i ${_ldm_dir}/SOURCES/edex_post/edex_post.patch || exit 1

# apply pqact patch
patch -u pqact/pqact.c -i ${_ldm_dir}/SOURCES/pqact/pqact.patch || exit 1

./configure --disable-max-size --disable-root-actions --prefix=${_ldm_root_dir} CFLAGS='-g -O0' > configure.log 2>&1
if [ $? -ne 0 ]; then
   echo "FATAL: ldm configure has failed!"
   exit 1
fi

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
_PATCH_DIRS=( 'decoders' 'etc' 'dev' 'gempak')
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
%attr(744,root,root) /etc/watchdog.d/ldm_watchdog.sh

%changelog
* Tue Mar 22 2022 Ron Anderson <ron.anderson@raytheon.com>
- Remove hostname filtering
* Tue Aug 31 2021 Matt Richardson <matthew.richardson@raytheon.com>
- Leveraged hostname logic to determine whether or not to include the watchdog scripts
* Wed Oct 07 2020 Tom Gurney <tom.gurney@raytheon.com>
- Added dependencies on awips2-python-dynamicserialize and awips2-python-numpy
  (required for the new Python edexBridge)
