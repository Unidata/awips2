%define contentdir /var/www
%define suexec_caller apache
%define mmn 20051115
%define vstring CentOS
%define distro CentOS

Summary: Pypies Apache HTTP Server
Name: awips2-httpd-pypies
Version: 2.2.3
# This Is Officially Release: 22%{?dist}
Release: 30%{?dist}
URL: http://httpd.apache.org/
Prefix: /awips2/httpd_pypies
Source0: http://www.apache.org/dist/httpd/httpd-%{version}.tar.gz
Source1: centos_index.html
Source3: httpd-pypies.logrotate
Source4: httpd-pypies.init
Source5: httpd.sysconf
Source8: centos_powered_by_rh.png
Source10: httpd.conf
Source11: ssl.conf
Source12: welcome.conf
Source13: manual.conf
Source14: proxy_ajp.conf
# Documentation
Source30: migration.xml
Source31: migration.css
Source32: html.xsl
Source33: README.confd
# build/scripts patches
Patch1: httpd-2.1.10-apctl.patch
Patch2: httpd-2.1.10-apxs.patch
Patch3: httpd-2.0.45-deplibs.patch
Patch4: httpd-2.1.10-disablemods.patch
Patch5: httpd-2.1.10-layout.patch
Patch6: httpd-2.2.2-ac260.patch
# Features/functional changes
Patch20: httpd-2.0.48-release.patch
Patch21: httpd-2.0.40-xfsz.patch
Patch22: httpd-2.1.10-pod.patch
Patch23: httpd-2.0.45-export.patch
Patch24: httpd-2.0.48-corelimit.patch
Patch25: httpd-2.0.54-selinux.patch
Patch26: httpd-2.2.3-proxysessid.patch
Patch27: httpd-2.2.3-proxypmatch.patch
Patch28: httpd-2.2.3-nbchunk.patch
# Bug fixes
Patch50: httpd-2.0.45-encode.patch
Patch54: httpd-2.2.0-authnoprov.patch
Patch55: httpd-2.2.3-proxyopt.patch
Patch56: httpd-2.2.3-proxyoride.patch
Patch57: httpd-2.0.52-logresline.patch
Patch58: httpd-2.2.3-ldappool.patch
Patch59: httpd-2.2.3-ssldynlock.patch
Patch60: httpd-2.0.52-escaperrs.patch
Patch61: httpd-2.2.3-eventdlock.patch
Patch62: httpd-2.2.3-hdrsedit.patch
Patch63: httpd-2.2.3-dummyreq.patch
# Security Fixes
Patch100: httpd-2.2.3-CVE-2006-5752.patch
Patch101: httpd-2.2.3-CVE-2007-1863.patch
Patch102: httpd-2.2.3-CVE-2007-3304.patch
Patch103: httpd-2.2.3-CVE-2007-3847.patch
Patch104: httpd-2.2.3-CVE-2007-5000.patch
Patch105: httpd-2.2.3-CVE-2007-4465.patch
Patch106: httpd-2.2.3-CVE-2007-6421.patch
Patch107: httpd-2.2.3-CVE-2007-6422.patch
Patch108: httpd-2.2.3-CVE-2007-6388.patch
Patch109: httpd-2.2.3-prftpcset.patch
Patch110: httpd-2.2.3-CVE-2007-3304-update.patch
Patch111: httpd-2.2.3-CVE-2008-2939.patch
# Rebases
Patch200: httpd-2.2.3-proxy229.patch
Patch201: httpd-2.2.3-cache229.patch
License: Apache Software License
Group: AWIPSII
BuildRoot: %{_tmppath}/%{name}-root
BuildRequires: autoconf, perl, pkgconfig, xmlto >= 0.0.11, findutils
BuildRequires: db4-devel, expat-devel, zlib-devel, libselinux-devel
BuildRequires: apr-devel >= 1.2.0, apr-util-devel >= 1.2.0, pcre-devel >= 5.0, 
Requires: /etc/mime.types, gawk, /usr/share/magic.mime, /usr/bin/find
Requires: initscripts >= 8.36
Requires: awips2-tools
Obsoletes: httpd-suexec
Prereq: /sbin/chkconfig, /bin/mktemp, /bin/rm, /bin/mv
Prereq: sh-utils, textutils, /usr/sbin/useradd
Provides: webserver
Provides: httpd-mmn = %{mmn}
Obsoletes: apache, secureweb, mod_dav, mod_gzip, stronghold-apache, stronghold-htdocs
Obsoletes: mod_put, mod_roaming, mod_jk
Conflicts: pcre < 4.0

%description
The Apache HTTP Server is a powerful, efficient, and extensible
web server.

%package devel
Group: AWIPSII
Summary: Development tools for the Apache HTTP server.
Obsoletes: secureweb-devel, apache-devel, stronghold-apache-devel
Requires: apr-devel, apr-util-devel, pkgconfig
Requires: httpd = %{version}-%{release}

%description devel
The httpd-devel package contains the APXS binary and other files
that you need to build Dynamic Shared Objects (DSOs) for the
Apache HTTP Server.

If you are installing the Apache HTTP server and you want to be
able to compile or develop additional modules for Apache, you need
to install this package.

%package manual
Group: AWIPSII
Summary: Documentation for the Apache HTTP server.
Requires: httpd = %{version}-%{release}
Obsoletes: secureweb-manual, apache-manual

%description manual
The httpd-manual package contains the complete manual and
reference guide for the Apache HTTP server. The information can
also be found at http://httpd.apache.org/docs/2.2/.

%package -n mod_ssl
Group: AWIPSII
Summary: SSL/TLS module for the Apache HTTP server
Epoch: 1
BuildRequires: openssl-devel, distcache-devel
Requires(post): openssl >= 0.9.7f-4, /bin/cat
Requires: httpd = 0:%{version}-%{release}, httpd-mmn = %{mmn}
Obsoletes: stronghold-mod_ssl

%description -n mod_ssl
The mod_ssl module provides strong cryptography for the Apache Web
server via the Secure Sockets Layer (SSL) and Transport Layer
Security (TLS) protocols.

%prep
%setup -q -n httpd-2.2.3
%patch1 -p1 -b .apctl
%patch2 -p1 -b .apxs
%patch3 -p1 -b .deplibs
%patch4 -p1 -b .disablemods
%patch5 -p1 -b .layout
%patch6 -p1 -b .ac260

%patch21 -p0 -b .xfsz
%patch22 -p1 -b .pod
%patch23 -p1 -b .export
%patch24 -p1 -b .corelimit
%patch25 -p1 -b .selinux
%patch26 -p1 -b .proxysessid
%patch27 -p1 -b .proxypmatch
%patch28 -p1 -b .nbchunk

# no -b to prevent droplets in install root
%patch50 -p1
%patch54 -p1 -b .authnoprov
%patch55 -p1 -b .proxyopt
%patch56 -p1 -b .proxyoride
%patch57 -p1 -b .logresline
%patch58 -p1 -b .ldappool
%patch59 -p1 -b .ssldynlock
%patch60 -p1 -b .escaperrs
%patch61 -p1 -b .eventdlock
%patch62 -p1 -b .hdrsedit
%patch63 -p1 -b .dummyreq

%patch100 -p1 -b .cve5752
%patch101 -p1 -b .cve1853
%patch102 -p1 -b .cve3304
%patch103 -p1 -b .cve3847
%patch104 -p1 -b .cve5000
%patch105 -p1 -b .cve4465
%patch106 -p1 -b .cve6421
%patch107 -p1 -b .cve6422
%patch108 -p1 -b .cve6388
%patch109 -p1 -b .prftpcset
%patch110 -p1 -b .cve3304-update
%patch111 -p1 -b .cve2939

# Rebases -- any changes to proxy/cache modules must come later:
%patch200 -p1 -b .proxy229
%patch201 -p1 -b .cache229

# Patch in vendor/release string
sed "s/@VENDOR@/%{vstring}/;s/@RELEASE@/%{release}/" < %{PATCH20} | patch -p1 -b -z .release

# Safety check: prevent build if defined MMN does not equal upstream MMN.
vmmn=`echo MODULE_MAGIC_NUMBER_MAJOR | cpp -include include/ap_mmn.h | sed -n '/^2/p'`
if test "x${vmmn}" != "x%{mmn}"; then
   : Error: Upstream MMN is now ${vmmn}, packaged MMN is %{mmn}.
   : Update the mmn macro and rebuild.
   exit 1
fi

: Building for '%{distro}' with MMN %{mmn} and vendor string '%{vstring}'

%build
# forcibly prevent use of bundled apr, apr-util, pcre
rm -rf srclib/{apr,apr-util,pcre}

# regenerate configure scripts
autoheader && autoconf || exit 1

# Limit size of CHANGES to recent history
echo '1,/Changes with Apache MPM/wq' | ed CHANGES

# Before configure; fix location of build dir in generated apxs
%{__perl} -pi -e "s:\@exp_installbuilddir\@:/awips2/httpd_pypies%{_libdir}/httpd/build:g" \
	support/apxs.in
# update location of migration guide in apachectl
%{__perl} -pi -e "s:\@docdir\@:%{_docdir}/%{name}-%{version}:g" \
	support/apachectl.in

# Build the migration guide
sed 's/@DISTRO@/%{distro}/' < $RPM_SOURCE_DIR/migration.xml > migration.xml
xmlto -x $RPM_SOURCE_DIR/html.xsl html-nochunks migration.xml
cp $RPM_SOURCE_DIR/migration.css . # make %%doc happy

CFLAGS="$RPM_OPT_FLAGS -fno-strict-aliasing"
SH_LDFLAGS="-Wl,-z,relro"
export CFLAGS SH_LDFLAGS

# Hard-code path to links to avoid unnecessary builddep
export LYNX_PATH=/usr/bin/links

function mpmbuild()
{
mpm=$1; shift
mkdir $mpm; pushd $mpm
../configure \
 	--prefix=/awips2/httpd_pypies%{_sysconfdir}/httpd \
 	--exec-prefix=/awips2/httpd_pypies%{_prefix} \
 	--bindir=/awips2/httpd_pypies%{_bindir} \
 	--sbindir=/awips2/httpd_pypies%{_sbindir} \
 	--mandir=/awips2/httpd_pypies/%{_mandir} \
	--libdir=/awips2/httpd_pypies%{_libdir} \
	--sysconfdir=/awips2/httpd_pypies%{_sysconfdir}/httpd/conf \
	--includedir=/awips2/httpd_pypies%{_includedir}/httpd \
	--libexecdir=/awips2/httpd_pypies%{_libdir}/httpd/modules \
	--datadir=/awips2/httpd_pypies%{contentdir} \
        --with-installbuilddir=/awips2/httpd_pypies%{_libdir}/httpd/build \
	--with-mpm=$mpm \
        --with-apr=%{_prefix} --with-apr-util=%{_prefix} \
	--enable-suexec --with-suexec \
	--with-suexec-caller=%{suexec_caller} \
	--with-suexec-docroot=/awips2/httpd_pypies%{contentdir} \
	--with-suexec-logfile=/awips2/httpd_pypies%{_localstatedir}/log/httpd/suexec.log \
	--with-suexec-bin=/awips2/httpd_pypies%{_sbindir}/suexec \
	--with-suexec-uidmin=500 --with-suexec-gidmin=100 \
        --enable-pie \
        --with-pcre \
	$*

make %{?_smp_mflags}
popd
}

# Build everything and the kitchen sink with the prefork build
mpmbuild prefork \
        --enable-mods-shared=all \
	--enable-ssl --with-ssl --enable-distcache \
	--enable-proxy \
        --enable-cache --enable-mem-cache \
        --enable-file-cache --enable-disk-cache \
        --enable-ldap --enable-authnz-ldap \
        --enable-cgid \
        --enable-authn-anon --enable-authn-alias

# For the other MPMs, just build httpd and no optional modules
mpmbuild worker --enable-modules=none
mpmbuild event --enable-modules=none

%install
rm -rf $RPM_BUILD_ROOT

# Classify ab and logresolve as section 1 commands, as they are in /usr/bin
mv docs/man/ab.8 docs/man/ab.1
mv docs/man/logresolve.8 docs/man/logresolve.1

pushd prefork
make DESTDIR=$RPM_BUILD_ROOT install
popd

# install alternative MPMs
install -m 755 worker/httpd $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/httpd.worker
install -m 755 event/httpd $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/httpd.event

# install conf file/directory
mkdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d
install -m 644 $RPM_SOURCE_DIR/README.confd \
    $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/README
for f in ssl.conf welcome.conf manual.conf proxy_ajp.conf; do
  install -m 644 $RPM_SOURCE_DIR/$f $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/$f
done

rm $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf/*.conf
install -m 644 $RPM_SOURCE_DIR/httpd.conf \
   $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf/httpd.conf

mkdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/sysconfig
install -m 644 $RPM_SOURCE_DIR/httpd.sysconf \
   $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/sysconfig/httpd

# for holding mod_dav lock database
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/lib/dav

# create a prototype session cache
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/cache/mod_ssl
touch $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/cache/mod_ssl/scache.{dir,pag,sem}

# create cache root
mkdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/cache/mod_proxy

# move utilities to /usr/bin
mv $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/{ab,htdbm,logresolve,htpasswd,htdigest} \
   $RPM_BUILD_ROOT/awips2/httpd_pypies%{_bindir}

# Make the MMN accessible to module packages
echo %{mmn} > $RPM_BUILD_ROOT/awips2/httpd_pypies%{_includedir}/httpd/.mmn

# docroot
mkdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/html
install -m 644 $RPM_SOURCE_DIR/centos_index.html \
	$RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/error/noindex.html

# remove manual sources
find $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/manual \( \
    -name \*.xml -o -name \*.xml.* -o -name \*.ent -o -name \*.xsl -o -name \*.dtd \
    \) -print0 | xargs -0 rm -f

# added for branding
install -m 644 %{SOURCE8} \
        $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/icons/powered_by_rh.png

# Strip the manual down just to English and replace the typemaps with flat files:
set +x
for f in `find $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/manual -name \*.html -type f`; do
   if test -f ${f}.en; then
      cp ${f}.en ${f}
      rm ${f}.*
   fi
done
set -x

# logs
rmdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/logs
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/log/httpd

# run
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies/var/run

mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies/var/lock/subsys/

# symlinks for /etc/httpd
ln -s ../..%{_localstatedir}/log/httpd $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/logs
ln -s ../..%{_localstatedir}/run $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/run
ln -s ../..%{_libdir}/httpd/modules $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/modules

# install service script.
mkdir -p ${RPM_BUILD_ROOT}/etc/init.d
install -m755 %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/configuration/etc/init.d/httpd-pypies \
    ${RPM_BUILD_ROOT}/etc/init.d

# install log rotation stuff
mkdir -p $RPM_BUILD_ROOT/etc/logrotate.d
install -m644 $RPM_SOURCE_DIR/httpd-pypies.logrotate \
	$RPM_BUILD_ROOT/etc/logrotate.d/httpd-pypies

# fix man page paths
sed -e "s|/usr/local/apache2/conf/httpd.conf|/etc/httpd/conf/httpd.conf|" \
    -e "s|/usr/local/apache2/conf/mime.types|/etc/mime.types|" \
    -e "s|/usr/local/apache2/conf/magic|/etc/httpd/conf/magic|" \
    -e "s|/usr/local/apache2/logs/error_log|/var/log/httpd/error_log|" \
    -e "s|/usr/local/apache2/logs/access_log|/var/log/httpd/access_log|" \
    -e "s|/usr/local/apache2/logs/httpd.pid|/var/run/httpd.pid|" \
    -e "s|/usr/local/apache2|/etc/httpd|" < docs/man/httpd.8 \
  > $RPM_BUILD_ROOT/awips2/httpd_pypies/%{_mandir}/man8/httpd.8

# Make ap_config_layout.h libdir-agnostic
sed -i '/.*DEFAULT_..._LIBEXECDIR/d;/DEFAULT_..._INSTALLBUILDDIR/d' \
    $RPM_BUILD_ROOT/awips2/httpd_pypies%{_includedir}/httpd/ap_config_layout.h

# Fix path to instdso in special.mk
sed -i '/instdso/s,top_srcdir,top_builddir,' \
    $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/httpd/build/special.mk

# Remove unpackaged files
rm -f $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/*.exp \
      $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/conf/mime.types \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/httpd/modules/*.exp \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/httpd/build/config.nice \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_bindir}/ap?-config \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/{checkgid,dbmmanage,envvars*} \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/htdocs/* \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_mandir}/man1/dbmmanage.* \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/cgi-bin/*

rm -rf $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/conf/{original,extra}

# Make suexec a+rw so it can be stripped.  %%files lists real permissions
chmod 755 $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/suexec

# build mod_wsgi.so
/bin/cp %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/src/mod_wsgi-3.3.tar.gz \
   %{_topdir}/BUILD
if [ $? -ne 0 ]; then
   exit 1
fi

pushd . > /dev/null
cd %{_topdir}/BUILD
if [ -d mod_wsgi-3.3 ]; then
   /bin/rm -rf mod_wsgi-3.3
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
/bin/tar -xvf mod_wsgi-3.3.tar.gz
if [ $? -ne 0 ]; then
   exit 1
fi
cd mod_wsgi-3.3
export CPPFLAGS="-I/awips2/python/include/python2.7"
export LDFLAGS="-L/awips2/python/lib"
./configure --with-python=/awips2/python/bin/python
if [ $? -ne 0 ]; then
   exit 1
fi
make
if [ $? -ne 0 ]; then
   exit 1
fi

# Install the module required by pypies.
install -m755 .libs/mod_wsgi.so \
    ${RPM_BUILD_ROOT}/awips2/httpd_pypies/etc/httpd/modules
    
cd ../
/bin/rm -f mod_wsgi-3.3.tar.gz
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/rm -rf mod_wsgi-3.3
if [ $? -ne 0 ]; then
   exit 1
fi
unset CPPFLAGS
unset LDFLAGS
popd > /dev/null

# Install the pypies configuration.
install -m644 %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/configuration/apache/pypies.conf \
    ${RPM_BUILD_ROOT}/awips2/httpd_pypies/etc/httpd/conf.d
mkdir -p ${RPM_BUILD_ROOT}/awips2/httpd_pypies/var/www/wsgi
install -m644 %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/configuration/apache/pypies.wsgi \
    ${RPM_BUILD_ROOT}/awips2/httpd_pypies/var/www/wsgi

# Install & Override the httpd configuration.
install -m644 %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/configuration/conf/httpd.conf \
    ${RPM_BUILD_ROOT}/awips2/httpd_pypies/etc/httpd/conf

%pre
# Add the "apache" user
/usr/sbin/useradd -c "Apache" -u 48 \
	-s /sbin/nologin -r -d /awips2/httpd_pypies%{contentdir} apache 2> /dev/null || :

%triggerpostun -- apache < 2.0, stronghold-apache < 2.0
/sbin/chkconfig --add httpd-pypies

# Prevent removal of index.html on upgrades from 1.3
%triggerun -- apache < 2.0, stronghold-apache < 2.0
if [ -r /awips2/httpd_pypies%{contentdir}/index.html -a ! -r /awips2/httpd_pypies%{contentdir}/index.html.rpmold ]; then
  mv /awips2/httpd_pypies%{contentdir}/index.html /awips2/httpd_pypies%{contentdir}/index.html.rpmold
fi

%post
# Register the httpd service
/sbin/chkconfig --add httpd-pypies

%preun
# this is not compatible with our environment. attempting to shutdown
# httpd-pypies causes 'yum groupremove' to abort and leaves us with
# an incomplete version of httpd-pypies that cannot be un-installed.
#if [ $1 = 0 ]; then
#	/sbin/service httpd-pypies stop > /dev/null 2>&1
#	/sbin/chkconfig --del httpd-pypies
#fi

%define sslcert /awips2/httpd_pypies%{_sysconfdir}/pki/tls/certs/localhost.crt
%define sslkey /awips2/httpd_pypies%{_sysconfdir}/pki/tls/private/localhost.key

%post -n mod_ssl
umask 077

if [ ! -f %{sslkey} ] ; then
/awips2/httpd_pypies%{_bindir}/openssl genrsa -rand /proc/apm:/proc/cpuinfo:/proc/dma:/proc/filesystems:/proc/interrupts:/proc/ioports:/proc/pci:/proc/rtc:/proc/uptime 1024 > %{sslkey} 2> /dev/null
fi

FQDN=`hostname`
if [ "x${FQDN}" = "x" ]; then
   FQDN=localhost.localdomain
fi

if [ ! -f %{sslcert} ] ; then
cat << EOF | /awips2/httpd_pypies%{_bindir}/openssl req -new -key %{sslkey} \
         -x509 -days 365 -set_serial $RANDOM \
         -out %{sslcert} 2>/dev/null
--
SomeState
SomeCity
SomeOrganization
SomeOrganizationalUnit
${FQDN}
root@${FQDN}
EOF
fi

%check
# Check the built modules are all PIC
if readelf -d $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/httpd/modules/*.so | grep TEXTREL; then
   : modules contain non-relocatable code
   exit 1
fi

# Verify that the same modules were built into the httpd binaries
./prefork/httpd -l | grep -v prefork > prefork.mods
for mpm in worker; do
  ./${mpm}/httpd -l | grep -v ${mpm} > ${mpm}.mods
  if ! diff -u prefork.mods ${mpm}.mods; then
    : Different modules built into httpd binaries, will not proceed
    exit 1
  fi
done

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,awips,fxalpha)

%doc ABOUT_APACHE README CHANGES LICENSE VERSIONING NOTICE
%doc migration.html migration.css

%dir /awips2/httpd_pypies
%dir /awips2/httpd_pypies/etc
%dir /awips2/httpd_pypies/etc/sysconfig
%dir /awips2/httpd_pypies/usr
%dir /awips2/httpd_pypies/usr/bin
%dir /awips2/httpd_pypies/%{_libdir}
%dir /awips2/httpd_pypies/usr/sbin
/awips2/httpd_pypies/usr/share
%dir /awips2/httpd_pypies/var
%dir /awips2/httpd_pypies/var/cache
%dir /awips2/httpd_pypies/var/lib
%dir /awips2/httpd_pypies/var/lock
%dir /awips2/httpd_pypies/var/log
%dir /awips2/httpd_pypies%{_sysconfdir}/httpd
%dir /awips2/httpd_pypies/var/run
%dir /awips2/httpd_pypies/var/lock/subsys/
%dir /awips2/httpd_pypies/var/www/wsgi
%config(noreplace) /awips2/httpd_pypies/var/www/wsgi/pypies.wsgi
/awips2/httpd_pypies%{_sysconfdir}/httpd/modules
/awips2/httpd_pypies%{_sysconfdir}/httpd/logs
/awips2/httpd_pypies%{_sysconfdir}/httpd/run
%dir /awips2/httpd_pypies%{_sysconfdir}/httpd/conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf/httpd.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/pypies.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/welcome.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/proxy_ajp.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf/magic

%config(noreplace) %{_sysconfdir}/logrotate.d/httpd-pypies
%config(noreplace) %{_sysconfdir}/init.d/httpd-pypies

%dir /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d
/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/README

%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/sysconfig/httpd

/awips2/httpd_pypies%{_bindir}/*
/awips2/httpd_pypies%{_sbindir}/ht*
/awips2/httpd_pypies%{_sbindir}/apachectl
/awips2/httpd_pypies%{_sbindir}/rotatelogs
%attr(4510,awips,fxalpha) /awips2/httpd_pypies%{_sbindir}/suexec

%dir /awips2/httpd_pypies%{_libdir}/httpd
%dir /awips2/httpd_pypies%{_libdir}/httpd/modules
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod*.so
%exclude /awips2/httpd_pypies%{_libdir}/httpd/modules/mod_ssl.so

%dir /awips2/httpd_pypies%{contentdir}
%dir /awips2/httpd_pypies%{contentdir}/cgi-bin
%dir /awips2/httpd_pypies%{contentdir}/html
%dir /awips2/httpd_pypies%{contentdir}/icons
%dir /awips2/httpd_pypies%{contentdir}/error
%dir /awips2/httpd_pypies%{contentdir}/error/include
/awips2/httpd_pypies%{contentdir}/icons/*
/awips2/httpd_pypies%{contentdir}/error/README
/awips2/httpd_pypies%{contentdir}/error/noindex.html
%config /awips2/httpd_pypies%{contentdir}/error/*.var
%config /awips2/httpd_pypies%{contentdir}/error/include/*.html

%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/log/httpd
%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/lib/dav
%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/cache/mod_proxy

/awips2/httpd_pypies/%{_mandir}/man?/*
%exclude /awips2/httpd_pypies/%{_mandir}/man8/apxs.8*

%files manual
%defattr(-,root,root)
/awips2/httpd_pypies%{contentdir}/manual
%config /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/manual.conf

%files -n mod_ssl
%defattr(-,root,root)
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod_ssl.so
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/ssl.conf
%attr(0700,apache,root) %dir /awips2/httpd_pypies%{_localstatedir}/cache/mod_ssl
%attr(0600,apache,root) %ghost /awips2/httpd_pypies%{_localstatedir}/cache/mod_ssl/scache.dir
%attr(0600,apache,root) %ghost /awips2/httpd_pypies%{_localstatedir}/cache/mod_ssl/scache.pag
%attr(0600,apache,root) %ghost /awips2/httpd_pypies%{_localstatedir}/cache/mod_ssl/scache.sem

%files devel
%defattr(-,root,root)
/awips2/httpd_pypies%{_includedir}/httpd
/awips2/httpd_pypies%{_sbindir}/apxs
/awips2/httpd_pypies/%{_mandir}/man8/apxs.8*
%dir /awips2/httpd_pypies%{_libdir}/httpd/build
/awips2/httpd_pypies%{_libdir}/httpd/build/*.mk
/awips2/httpd_pypies%{_libdir}/httpd/build/*.sh

%changelog
* Thu Jan 22 2009 Karanbir Singh <kbsingh@centos.org> 2.2.3-22.el5.centos
- Roll in CentOS branding changes

* Wed Nov 12 2008 Joe Orton <jorton@redhat.com> 2.2.3-22.el5
- add security fixes for CVE-2008-2939 (#468841)
- note that the mod_proxy 2.2.9 rebase fixed CVE-2008-2634

* Tue Oct 21 2008 Joe Orton <jorton@redhat.com> 2.2.3-21.el5
- avoid strict-aliasing warnings (#462877)

* Tue Oct 21 2008 Joe Orton <jorton@redhat.com> 2.2.3-20.el5
- mod_proxy: scoreboard access fix (#252024)

* Thu Sep 18 2008 Joe Orton <jorton@redhat.com> 2.2.3-19.el5
- mod_proxy: various backport fixes (#252024)

* Thu Sep 18 2008 Joe Orton <jorton@redhat.com> 2.2.3-17.el5
- fix mod_proxy symbol use

* Mon Sep 15 2008 Joe Orton <jorton@redhat.com> 2.2.3-16.el5
- mod_proxy*, mod_cache*: rebase to 2.2.9 (#252024, #249534,
  #439842)
- backport changes to make chunk filter non-blocking (#454098)

* Fri Sep 12 2008 Joe Orton <jorton@redhat.com> 2.2.3-15.el5
- mod_ldap: fix memory lifetime issues (#440259)
- mod_ssl: configure OpenSSL dynamic lock callbacks (#462044)
- escape the Request-Method in canned error responses (#445888)
- build the event MPM and fix a deadlock therein (#444643)
- mod_headers: support "RequestHeader edit" (#428253)
- use "OPTIONS *" rather than "GET /" in dummy connection (#367981)

* Thu Aug 14 2008 Joe Orton <jorton@redhat.com> 2.2.3-14.el5
- mod_proxy: add ProxyPassMatch support (#449159)

* Mon Jul 21 2008 Joe Orton <jorton@redhat.com> 2.2.3-13.el5
- mod_proxy_balancer: allow alternative string to match for
  stickysession parameter (#439218)
- fix dist tag in Release (#440615)

* Fri Jan 11 2008 Joe Orton <jorton@redhat.com> 2.2.3-12.el5_1.3
- further update to backport for CVE-2007-6421 (#427240)

* Fri Jan 11 2008 Joe Orton <jorton@redhat.com> 2.2.3-12.el5_1.2
- updated backport for CVE-2007-6421 (#427240)

* Mon Jan  7 2008 Joe Orton <jorton@redhat.com> 2.2.3-11.el5_1.1
- add security fixes for CVE-2007-6388, CVE-2007-6421
  and CVE-2007-6422 (#427240)
- add security fix for CVE-2007-4465, CVE-2007-5000 (#421631)
- add security fix for mod_proxy_ftp UTF-7 XSS (#427745)

* Mon Aug  6 2007 Joe Orton <jorton@redhat.com> 2.2.3-11.el5
- mark httpd.conf config(noreplace) (#247881)

* Fri Aug  3 2007 Joe Orton <jorton@redhat.com> 2.2.3-10.el5
- add security fix for CVE-2007-3847 (#250761)

* Wed Aug  1 2007 Joe Orton <jorton@redhat.com> 2.2.3-9.el5
- load mod_version by default (#247881)

* Tue Jun 26 2007 Joe Orton <jorton@redhat.com> 2.2.3-8.el5
- add 'ServerTokens Full-Release' config option (#240857)
- use init script in logrotate postrotate (#241680)
- fix mod_proxy option inheritance (#245719)
- fix ProxyErrorOverride to only affect 4xx, 5xx responses (#240024)
- bump logresolve line buffer length to 10K (#245763)
- add security fixes for CVE-2007-1863, CVE-2007-3304,
  and CVE-2006-5752 (#244666)

* Wed Nov 29 2006 Joe Orton <jorton@redhat.com> 2.2.3-6.el5
- fix path to instdso.sh in special.mk (#217677)
- fix detection of links in "apachectl fullstatus"

* Tue Sep 19 2006 Joe Orton <jorton@redhat.com> 2.2.3-5.el5
- rebuild

* Fri Aug 11 2006 Joe Orton <jorton@redhat.com> 2.2.3-3.el5
- use RHEL branding

* Thu Aug  3 2006 Joe Orton <jorton@redhat.com> 2.2.3-3
- init: use killproc() delay to avoid race killing parent

* Fri Jul 28 2006 Joe Orton <jorton@redhat.com> 2.2.3-2
- update to 2.2.3
- trim %%changelog to >=2.0.52

* Thu Jul 20 2006 Joe Orton <jorton@redhat.com> 2.2.2-8
- fix segfault on dummy connection failure at graceful restart (#199429)

* Wed Jul 19 2006 Joe Orton <jorton@redhat.com> 2.2.2-7
- fix "apxs -g"-generated Makefile
- fix buildconf with autoconf 2.60

* Wed Jul 12 2006 Jesse Keating <jkeating@redhat.com> - 2.2.2-5.1
- rebuild

* Wed Jun  7 2006 Joe Orton <jorton@redhat.com> 2.2.2-5
- require pkgconfig for -devel (#194152)
- fixes for installed support makefiles (special.mk et al)
- BR autoconf

* Fri Jun  2 2006 Joe Orton <jorton@redhat.com> 2.2.2-4
- make -devel package multilib-safe (#192686)

* Thu May 11 2006 Joe Orton <jorton@redhat.com> 2.2.2-3
- build DSOs using -z relro linker flag

* Wed May  3 2006 Joe Orton <jorton@redhat.com> 2.2.2-2
- update to 2.2.2

* Thu Apr  6 2006 Joe Orton <jorton@redhat.com> 2.2.0-6
- rebuild to pick up apr-util LDAP interface fix (#188073)

* Fri Feb 10 2006 Jesse Keating <jkeating@redhat.com> - (none):2.2.0-5.1.2
- bump again for double-long bug on ppc(64)

* Tue Feb 07 2006 Jesse Keating <jkeating@redhat.com> - (none):2.2.0-5.1.1
- rebuilt for new gcc4.1 snapshot and glibc changes

* Mon Feb  6 2006 Joe Orton <jorton@redhat.com> 2.2.0-5.1
- mod_auth_basic/mod_authn_file: if no provider is configured,
  and AuthUserFile is not configured, decline to handle authn
  silently rather than failing noisily.

* Fri Feb  3 2006 Joe Orton <jorton@redhat.com> 2.2.0-5
- mod_ssl: add security fix for CVE-2005-3357 (#177914)
- mod_imagemap: add security fix for CVE-2005-3352 (#177913)
- add fix for AP_INIT_* designated initializers with C++ compilers
- httpd.conf: enable HTMLTable in default IndexOptions
- httpd.conf: add more "redirect-carefully" matches for DAV clients

* Thu Jan  5 2006 Joe Orton <jorton@redhat.com> 2.2.0-4
- mod_proxy_ajp: fix Cookie handling (Mladen Turk, r358769)

* Fri Dec 09 2005 Jesse Keating <jkeating@redhat.com>
- rebuilt

* Wed Dec  7 2005 Joe Orton <jorton@redhat.com> 2.2.0-3
- strip manual to just English content

* Mon Dec  5 2005 Joe Orton <jorton@redhat.com> 2.2.0-2
- don't strip C-L from HEAD responses (Greg Ames, #110552)
- load mod_proxy_balancer by default
- add proxy_ajp.conf to load/configure mod_proxy_ajp
- Obsolete mod_jk
- update docs URLs in httpd.conf/ssl.conf

* Fri Dec  2 2005 Joe Orton <jorton@redhat.com> 2.2.0-1
- update to 2.2.0

* Wed Nov 30 2005 Joe Orton <jorton@redhat.com> 2.1.10-2
- enable mod_authn_alias, mod_authn_anon
- update default httpd.conf

* Fri Nov 25 2005 Joe Orton <jorton@redhat.com> 2.1.10-1
- update to 2.1.10
- require apr >= 1.2.0, apr-util >= 1.2.0

* Wed Nov  9 2005 Tomas Mraz <tmraz@redhat.com> 2.0.54-16
- rebuilt against new openssl

* Thu Nov  3 2005 Joe Orton <jorton@redhat.com> 2.0.54-15
- log notice giving SELinux context at startup if enabled
- drop SSLv2 and restrict default cipher suite in default
 SSL configuration

* Thu Oct 20 2005 Joe Orton <jorton@redhat.com> 2.0.54-14
- mod_ssl: add security fix for SSLVerifyClient (CVE-2005-2700)
- add security fix for byterange filter DoS (CVE-2005-2728)
- add security fix for C-L vs T-E handling (CVE-2005-2088)
- mod_ssl: add security fix for CRL overflow (CVE-2005-1268)
- mod_ldap/mod_auth_ldap: add fixes from 2.0.x branch (upstream #34209 etc)
- add fix for dummy connection handling (#167425)
- mod_auth_digest: fix hostinfo comparison in CONNECT requests
- mod_include: fix variable corruption in nested includes (upstream #12655)
- mod_ssl: add fix for handling non-blocking reads
- mod_ssl: fix to enable output buffering (upstream #35279)
- mod_ssl: buffer request bodies for per-location renegotiation (upstream #12355)

* Sat Aug 13 2005 Joe Orton <jorton@redhat.com> 2.0.54-13
- don't load by default: mod_cern_meta, mod_asis
- do load by default: mod_ext_filter (#165893)

* Thu Jul 28 2005 Joe Orton <jorton@redhat.com> 2.0.54-12
- drop broken epoch deps

* Thu Jun 30 2005 Joe Orton <jorton@redhat.com> 2.0.54-11
- mod_dav_fs: fix uninitialized variable (#162144)
- add epoch to dependencies as appropriate
- mod_ssl: drop dependencies on dev, make
- mod_ssl: mark post script dependencies as such

* Mon May 23 2005 Joe Orton <jorton@redhat.com> 2.0.54-10
- remove broken symlink (Robert Scheck, #158404)

* Wed May 18 2005 Joe Orton <jorton@redhat.com> 2.0.54-9
- add piped logger fixes (w/Jeff Trawick)

* Mon May  9 2005 Joe Orton <jorton@redhat.com> 2.0.54-8
- drop old "powered by Red Hat" logos

* Wed May  4 2005 Joe Orton <jorton@redhat.com> 2.0.54-7
- mod_userdir: fix memory allocation issue (upstream #34588)
- mod_ldap: fix memory corruption issue (Brad Nicholes, upstream #34618)

* Tue Apr 26 2005 Joe Orton <jorton@redhat.com> 2.0.54-6
- fix key/cert locations in post script

* Mon Apr 25 2005 Joe Orton <jorton@redhat.com> 2.0.54-5
- create default dummy cert in /etc/pki/tls
- use a pseudo-random serial number on the dummy cert
- change default ssl.conf to point at /etc/pki/tls
- merge back -suexec subpackage; SELinux policy can now be
  used to persistently disable suexec (#155716)
- drop /etc/httpd/conf/ssl.* directories and Makefiles
- unconditionally enable PIE support
- mod_ssl: fix for picking up -shutdown options (upstream #34452)

* Mon Apr 18 2005 Joe Orton <jorton@redhat.com> 2.0.54-4
- replace PreReq with Requires(pre) 

* Mon Apr 18 2005 Joe Orton <jorton@redhat.com> 2.0.54-3
- update to 2.0.54

* Tue Mar 29 2005 Joe Orton <jorton@redhat.com> 2.0.53-6
- update default httpd.conf:
 * clarify the comments on AddDefaultCharset usage (#135821)
 * remove all the AddCharset default extensions
 * don't load mod_imap by default
 * synch with upstream 2.0.53 httpd-std.conf
- mod_ssl: set user from SSLUserName in access hook (upstream #31418)
- htdigest: fix permissions of created files (upstream #33765)
- remove htsslpass

* Wed Mar  2 2005 Joe Orton <jorton@redhat.com> 2.0.53-5
- apachectl: restore use of $OPTIONS again

* Wed Feb  9 2005 Joe Orton <jorton@redhat.com> 2.0.53-4
- update to 2.0.53
- move prefork/worker modules comparison to %%check

* Mon Feb  7 2005 Joe Orton <jorton@redhat.com> 2.0.52-7
- fix cosmetic issues in "service httpd reload"
- move User/Group higher in httpd.conf (#146793)
- load mod_logio by default in httpd.conf
- apachectl: update for correct libselinux tools locations

* Tue Nov 16 2004 Joe Orton <jorton@redhat.com> 2.0.52-6
- add security fix for CVE CAN-2004-0942 (memory consumption DoS)
- SELinux: run httpd -t under runcon in configtest (Steven Smalley)
- fix SSLSessionCache comment for distcache in ssl.conf
- restart using SIGHUP not SIGUSR1 after logrotate
- add ap_save_brigade fix (upstream #31247)
- mod_ssl: fix possible segfault in auth hook (upstream #31848)
- add htsslpass(1) and configure as default SSLPassPhraseDialog (#128677)
- apachectl: restore use of $OPTIONS
- apachectl, httpd.init: refuse to restart if $HTTPD -t fails
- apachectl: run $HTTPD -t in user SELinux context for configtest
- update for pcre-5.0 header locations

* Sat Nov 13 2004 Jeff Johnson <jbj@redhat.com> 2.0.52-5
- rebuild against db-4.3.21 aware apr-util.

* Thu Nov 11 2004 Jeff Johnson <jbj@jbj.org> 2.0.52-4
- rebuild against db-4.3-21.

* Thu Sep 28 2004 Joe Orton <jorton@redhat.com> 2.0.52-3
- add dummy connection address fixes from HEAD
- mod_ssl: add security fix for CAN-2004-0885

* Tue Sep 28 2004 Joe Orton <jorton@redhat.com> 2.0.52-2
- update to 2.0.52

