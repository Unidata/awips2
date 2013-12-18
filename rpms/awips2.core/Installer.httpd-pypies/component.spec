%define contentdir /var/www
%define suexec_caller apache
%define mmn 20051115
%define vstring Red Hat
%define mpms worker event

Summary: Pypies Apache HTTP Server
Name: awips2-httpd-pypies
Version: 2.2.15
Release: 15.4.el6
URL: http://httpd.apache.org/
Source0: http://archive.apache.org/dist/httpd/httpd-%{version}.tar.gz
Source1: index.html
Source4: httpd-pypies.init
Source5: httpd.sysconf
Source10: httpd.conf
Source11: ssl.conf
Source12: welcome.conf
Source13: manual.conf
# Documentation
Source33: README.confd
# build/scripts patches
Patch1: httpd-2.1.10-apctl.patch
Patch2: httpd-2.1.10-apxs.patch
Patch3: httpd-2.2.9-deplibs.patch
Patch4: httpd-2.1.10-disablemods.patch
Patch5: httpd-2.1.10-layout.patch
# Features/functional changes
Patch20: httpd-2.2.14-release.patch
Patch21: httpd-2.2.11-xfsz.patch
Patch22: httpd-2.1.10-pod.patch
Patch23: httpd-2.0.45-export.patch
Patch24: httpd-2.2.11-corelimit.patch
Patch25: httpd-2.2.11-selinux.patch
Patch26: httpd-2.2.15-sslfips.patch
Patch27: httpd-2.2.15-modreqto2217.patch
# Bug fixes
Patch60: httpd-2.0.52-logresline.patch
Patch61: httpd-2.2.3-defpidlog.patch
Patch62: httpd-2.2.3-extfiltereos.patch
Patch63: httpd-2.2.3-graceful-ebadf.patch
Patch64: httpd-2.2.3-noxpad.patch
Patch65: httpd-2.2.3-pngmagic.patch
Patch67: httpd-2.2.14-ldapdyngrp.patch
Patch68: httpd-2.2.15-proxyconn.patch
Patch69: httpd-2.2.0-authnoprov.patch
Patch70: httpd-2.2.15-ssloidval.patch
Patch71: httpd-2.2.15-davputfail.patch
Patch72: httpd-2.2.15-expectnoka.patch
Patch73: httpd-2.2.15-pr49328.patch
Patch74: httpd-2.2.15-aboverflow.patch
Patch75: httpd-2.2.15-pr45444.patch
Patch76: httpd-2.2.15-ssldupkeys.patch
Patch77: httpd-2.2.15-ldapcache.patch
Patch78: httpd-2.2.3-pr41743.patch
Patch79: httpd-2.2.15-filterhdr.patch
Patch80: httpd-2.2.15-oomabort.patch
Patch81: httpd-2.2.15-sslpxycerts.patch
Patch82: httpd-2.2.15-sslproxyio.patch
Patch83: httpd-2.2.15-ajperror.patch
Patch84: httpd-2.2.15-sslsninotreq.patch
Patch85: httpd-2.2.15-sslbadcdev.patch
Patch86: httpd-2.2.15-proxyepsv.patch
Patch87: httpd-2.2.15-cachehardmax.patch
# Security fixes
Patch200: httpd-2.2.15-CVE-2010-1452.patch
Patch201: httpd-2.2.15-CVE-2011-3192ver3.patch
Patch202: httpd-2.2.15-CVE-2011-3368.patch
Patch203: httpd-2.2.15-CVE-2011-3348.patch
License: ASL 2.0
Group: AWIPSII
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: autoconf, perl, pkgconfig, findutils
BuildRequires: zlib-devel, libselinux-devel
BuildRequires: apr-devel >= 1.2.0, apr-util-devel >= 1.2.0, pcre-devel >= 5.0
Requires: initscripts >= 8.36, /etc/mime.types, system-logos >= 7.92.1-1
Requires: awips2-tools
Obsoletes: httpd-suexec
Requires(pre): /usr/sbin/useradd
Requires(post): chkconfig
Provides: webserver
Provides: mod_dav = %{version}-%{release}, httpd-suexec = %{version}-%{release}
Provides: httpd-mmn = %{mmn}
Obsoletes: apache, secureweb, mod_dav, mod_gzip, stronghold-apache
Obsoletes: stronghold-htdocs, mod_put, mod_roaming
Conflicts: pcre < 4.0
Requires: apr-util-ldap
requires: awips2-pypies

%description
The Apache HTTP Server is a powerful, efficient, and extensible
web server.

%package devel
Group: AWIPSII
Summary: Development interfaces for the Apache HTTP server
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
Summary: Documentation for the Apache HTTP server
Requires: httpd = %{version}-%{release}
Obsoletes: secureweb-manual, apache-manual
BuildArch: noarch

%description manual
The httpd-manual package contains the complete manual and
reference guide for the Apache HTTP server. The information can
also be found at http://httpd.apache.org/docs/2.2/.

%package -n mod_ssl
Group: AWIPSII
Summary: SSL/TLS module for the Apache HTTP Server
Epoch: 1
BuildRequires: openssl-devel
Requires(post): openssl >= 0.9.7f-4, /bin/cat
Requires(pre): httpd
Requires: httpd = 0:%{version}-%{release}, httpd-mmn = %{mmn}
Obsoletes: stronghold-mod_ssl

%description -n mod_ssl
The mod_ssl module provides strong cryptography for the Apache Web
server via the Secure Sockets Layer (SSL) and Transport Layer
Security (TLS) protocols.

%prep
%setup -q -n httpd-2.2.15
%patch1 -p1 -b .apctl
%patch2 -p1 -b .apxs
%patch3 -p1 -b .deplibs
%patch4 -p1 -b .disablemods
%patch5 -p1 -b .layout

%patch21 -p1 -b .xfsz
%patch22 -p1 -b .pod
%patch23 -p1 -b .export
%patch24 -p1 -b .corelimit
%patch25 -p1 -b .selinux
%patch26 -p1 -b .sslfips
%patch27 -p1 -b .modreqto2217

%patch60 -p1 -b .logresline
%patch61 -p1 -b .defpidlog
%patch62 -p1 -b .extfiltereos
%patch63 -p1 -b .graceful-ebadf
%patch64 -p1 -b .noxpad
%patch65 -p1 -b .pngmagic
%patch67 -p1 -b .ldapdyngrp
%patch68 -p1 -b .proxyconn
%patch69 -p1 -b .authnoprov
%patch70 -p1 -b .ssloidval
%patch71 -p1 -b .davputfail
%patch72 -p1 -b .expectnoka
%patch73 -p1 -b .pr49328
%patch74 -p1 -b .aboverflow
%patch75 -p1 -b .pr45444
%patch76 -p1 -b .ssldupkeys
%patch77 -p1 -b .ldapcache
%patch78 -p1 -b .pr41743
%patch79 -p1 -b .filterhdr
%patch80 -p1 -b .oomabort
%patch81 -p1 -b .sslpxycerts
%patch82 -p1 -b .sslproxyio
%patch83 -p1 -b .ajperror
%patch84 -p1 -b .sslsninotreq
%patch85 -p1 -b .sslbadcdev
%patch86 -p1 -b .proxyepsv
%patch87 -p1 -b .cachehardmax

%patch200 -p1 -b .cve1452
%patch201 -p1 -b .cve3192ver3
%patch202 -p1 -b .cve3368
%patch203 -p1 -b .cve3348

# Patch in vendor/release string
sed "s/@VENDOR@/%{vstring}/;s/@RELEASE@/%{release}/" < %{PATCH20} | patch -p1 -b -z .release

# Safety check: prevent build if defined MMN does not equal upstream MMN.
vmmn=`echo MODULE_MAGIC_NUMBER_MAJOR | cpp -include include/ap_mmn.h | sed -n '/^2/p'`
if test "x${vmmn}" != "x%{mmn}"; then
   : Error: Upstream MMN is now ${vmmn}, packaged MMN is %{mmn}.
   : Update the mmn macro and rebuild.
   exit 1
fi

: Building with MMN %{mmn} and vendor string '%{vstring}'

%build
# forcibly prevent use of bundled apr, apr-util, pcre
rm -rf srclib/{apr,apr-util,pcre}

# regenerate configure scripts
autoheader && autoconf || exit 1

# Before configure; fix location of build dir in generated apxs
%{__perl} -pi -e "s:\@exp_installbuilddir\@:%{_libdir}/httpd/build:g" \
	support/apxs.in

CFLAGS="$RPM_OPT_FLAGS -Wformat-security -fno-strict-aliasing"
SH_LDFLAGS="-Wl,-z,relro"
export CFLAGS SH_LDFLAGS

# Forcibly disable use of rsync to install (#557049)
export ac_cv_path_RSYNC=

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
 	--mandir=/awips2/httpd_pypies%{_mandir} \
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

make %{?_smp_mflags} EXTRA_CFLAGS="-Werror-implicit-function-declaration"
popd
}

# Build everything and the kitchen sink with the prefork build
mpmbuild prefork \
        --enable-mods-shared=all \
	--enable-ssl --with-ssl \
	--enable-proxy \
        --enable-cache \
        --enable-disk-cache \
        --enable-ldap --enable-authnz-ldap \
        --enable-cgid \
        --enable-authn-anon --enable-authn-alias \
        --disable-imagemap

# For the other MPMs, just build httpd and no optional modules
for f in %{mpms}; do
   mpmbuild $f --enable-modules=none
done

%install
rm -rf $RPM_BUILD_ROOT

# Classify ab and logresolve as section 1 commands, as they are in /usr/bin
mv docs/man/ab.8 docs/man/ab.1
mv docs/man/logresolve.8 docs/man/logresolve.1

pushd prefork
make DESTDIR=$RPM_BUILD_ROOT install
popd

# install alternative MPMs
for f in %{mpms}; do
  install -m 755 ${f}/httpd $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/httpd.${f}
done

# install conf file/directory
mkdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d
install -m 644 $RPM_SOURCE_DIR/README.confd \
    $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/README
for f in ssl.conf welcome.conf manual.conf; do
  install -m 644 -p $RPM_SOURCE_DIR/$f \
        $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/$f
done

rm $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf/*.conf
install -m 644 -p $RPM_SOURCE_DIR/httpd.conf \
   $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf/httpd.conf

mkdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/sysconfig
install -m 644 -p $RPM_SOURCE_DIR/httpd.sysconf \
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
install -m 644 -p $RPM_SOURCE_DIR/index.html \
        $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/error/noindex.html

# remove manual sources
find $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/manual \( \
    -name \*.xml -o -name \*.xml.* -o -name \*.ent -o -name \*.xsl -o -name \*.dtd \
    \) -print0 | xargs -0 rm -f

# Strip the manual down just to English and replace the typemaps with flat files:
set +x
for f in `find $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/manual -name \*.html -type f`; do
   if test -f ${f}.en; then
      cp ${f}.en ${f}
      rm ${f}.*
   fi
done
set -x

# Symlink for the powered-by-$DISTRO image:
ln -s ../../..%{_datadir}/pixmaps/poweredby.png \
        $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/icons/poweredby.png

# Set up /var directories
rmdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/logs
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/log/httpd
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/run/httpd
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/lock/subsys

pushd .
cd $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd
# symlinks for /etc/httpd
ln -s ../..%{_localstatedir}/log/httpd $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/logs
ln -s ../..%{_localstatedir}/run/httpd $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/run
ln -s ../..%{_libdir}/httpd/modules $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/modules
popd

# install service script
mkdir -p ${RPM_BUILD_ROOT}/etc/init.d
install -m755 %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/configuration/etc/init.d/httpd-pypies \
   ${RPM_BUILD_ROOT}/etc/init.d

# install cron job
mkdir -p ${RPM_BUILD_ROOT}/etc/cron.daily
install -m755 %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/configuration/etc/cron.daily/pypiesLogCleanup.sh \
   ${RPM_BUILD_ROOT}/etc/cron.daily

# fix man page paths
sed -e "s|/usr/local/apache2/conf/httpd.conf|/etc/httpd/conf/httpd.conf|" \
    -e "s|/usr/local/apache2/conf/mime.types|/etc/mime.types|" \
    -e "s|/usr/local/apache2/conf/magic|/etc/httpd/conf/magic|" \
    -e "s|/usr/local/apache2/logs/error_log|/var/log/httpd/error_log|" \
    -e "s|/usr/local/apache2/logs/access_log|/var/log/httpd/access_log|" \
    -e "s|/usr/local/apache2/logs/httpd.pid|/var/run/httpd/httpd.pid|" \
    -e "s|/usr/local/apache2|/etc/httpd|" < docs/man/httpd.8 \
  > $RPM_BUILD_ROOT/awips2/httpd_pypies%{_mandir}/man8/httpd.8

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
getent group apache >/dev/null || groupadd -g 48 -r apache
getent passwd apache >/dev/null || \
  useradd -r -u 48 -g apache -s /sbin/nologin \
    -d %{contentdir} -c "Apache" apache
exit 0

%post
# Register the httpd service
/sbin/chkconfig --add httpd-pypies

%preun
# this is not compatible with our environment. attempting to shutdown
# httpd-pypies causes 'yum groupremove' to abort and leaves us with
# an incomplete version of httpd-pypies that cannot be un-installed.
#if [ $1 = 0 ]; then
#	/sbin/service httpd stop > /dev/null 2>&1
#	/sbin/chkconfig --del httpd
#fi

%posttrans
/sbin/service httpd condrestart >/dev/null 2>&1 || :

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
for mpm in %{mpms}; do
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
%dir /awips2/httpd_pypies/var/log
%dir /awips2/httpd_pypies%{_sysconfdir}/httpd
%dir /awips2/httpd_pypies/var/run
%dir /awips2/httpd_pypies/var/www/wsgi
%config(noreplace) /awips2/httpd_pypies/var/www/wsgi/pypies.wsgi
/awips2/httpd_pypies%{_sysconfdir}/httpd/modules
/awips2/httpd_pypies%{_sysconfdir}/httpd/logs
/awips2/httpd_pypies%{_sysconfdir}/httpd/run
%dir /awips2/httpd_pypies%{_sysconfdir}/httpd/conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf/httpd.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/pypies.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/welcome.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf/magic

%{_sysconfdir}/cron.daily/pypiesLogCleanup.sh
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

%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/lock/subsys
%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/log/httpd
%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/run/httpd
%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/lib/dav
%attr(0700,awips,fxalpha) %dir /awips2/httpd_pypies%{_localstatedir}/cache/mod_proxy

/awips2/httpd_pypies/%{_mandir}/man1/*
/awips2/httpd_pypies/%{_mandir}/man8/*

%files manual
%defattr(-,root,root)
/awips2/httpd_pypies/%{contentdir}/manual
%config /awips2/httpd_pypies/%{_sysconfdir}/httpd/conf.d/manual.conf

%files -n mod_ssl
%defattr(-,root,root)
/awips2/httpd_pypies/%{_libdir}/httpd/modules/mod_ssl.so
%config(noreplace) /awips2/httpd_pypies/%{_sysconfdir}/httpd/conf.d/ssl.conf
%attr(0700,apache,root) %dir /awips2/httpd_pypies/%{_localstatedir}/cache/mod_ssl
%attr(0600,apache,root) %ghost /awips2/httpd_pypies/%{_localstatedir}/cache/mod_ssl/scache.dir
%attr(0600,apache,root) %ghost /awips2/httpd_pypies/%{_localstatedir}/cache/mod_ssl/scache.pag
%attr(0600,apache,root) %ghost /awips2/httpd_pypies/%{_localstatedir}/cache/mod_ssl/scache.sem

%files devel
%defattr(-,root,root)
/awips2/httpd_pypies/%{_includedir}/httpd
/awips2/httpd_pypies/%{_sbindir}/apxs
/awips2/httpd_pypies/%{_mandir}/man8/apxs.8*
%dir /awips2/httpd_pypies%{_libdir}/httpd/build
/awips2/httpd_pypies%{_libdir}/httpd/build/*.mk
/awips2/httpd_pypies%{_libdir}/httpd/build/*.sh

%changelog
* Thu Oct  6 2011 Joe Orton <jorton@redhat.com> - 2.2.15-15
- mod_proxy_ftp: fix handling of EPSV w/IPv6 localhost (#737960)
- core: add security fix for CVE-2011-3368 (#743659)
- mod_proxy_ajp: add security fix for CVE-2011-3348 (#738961)
- mod_cache: forward-port CacheMaxExpire "hard" option (#740242)

* Wed Sep 28 2011 Joe Orton <jorton@redhat.com> - 2.2.15-14
- update to byterange patch (#736592)

* Thu Sep  8 2011 Joe Orton <jorton@redhat.com> - 2.2.15-13
- add security fix for CVE-2011-3192 (#733063, #736592)

* Mon Aug  8 2011 Joe Orton <jorton@redhat.com> - 2.2.15-12
- mod_ssl: ignore SNI hints unless required by config (#714704)
- mod_ssl: fix segfault with bad SSLCryptoDevice argument (#729585)

* Tue Jul 26 2011 Joe Orton <jorton@redhat.com> - 2.2.15-11
- rebase mod_reqtimeout to 2.2.17 (#676634)
- mod_proxy_ajp: honour ProxyErrorOverride (#694939)
- mod_ssl: add fix for handling incomplete lines w/revproxy (#700074)
- mod_filter: fix matching against non-std response headers (#700075)
- core: abort() on malloc() failure (#700393)
- mod_ssl: fix startup crash w/client cert shared across vhosts (#720980)

* Tue Jun  7 2011 Joe Orton <jorton@redhat.com> - 2.2.15-10
- mod_filter: fix test against non-standard response headers (#700075)

* Fri Apr  8 2011 Joe Orton <jorton@redhat.com> - 2.2.15-9
- mod_ssl: complete fix for overlapping memcpy (#652335)

* Mon Mar 21 2011 Joe Orton <jorton@redhat.com> - 2.2.15-8
- mod_ssl: fix compat with FIPS-enabled OpenSSL (#684144)

* Thu Feb 24 2011 Joe Orton <jorton@redhat.com> - 2.2.15-7
- mod_ldap: fix caching with per-vhost directive use (#676635)
- mod_ssl: fix startup with duplicate SSL vhost configurations (#676831)
- prefork: ensure early child exit during graceful restart (#679476)

* Mon Jan 31 2011 Joe Orton <jorton@redhat.com> - 2.2.15-6
- ab: fail gracefully for OOM allocating stats structures (#645846)
- init script: use $STOP_DELAY as delay before SIGKILL of parent (#657480)
- stop multiple invocations of filter init functions (#631849)
- mod_ssl: avoid overlapping memcpy (#652335)
- mark httpd.conf as noreplace

* Fri Aug 13 2010 Joe Orton <jorton@redhat.com> - 2.2.15-5
- add security fix for CVE-2010-1452 (#618193)

* Wed Jun 23 2010 Joe Orton <jorton@redhat.com> - 2.2.15-4
- use init script to rotate logs (#606955)
- disable keepalive for 100-continue and error response (#606964)

* Tue May 25 2010 Joe Orton <jorton@redhat.com> - 2.2.15-3
- add "Satisfy All" for .htaccess in httpd.conf (#594981)
- adjust user/group creation in %%pre (#594395)
- mod_ssl: tweak OID() evaluation of unknown exts (#594980)

* Thu May  6 2010 Joe Orton <jorton@redhat.com> - 2.2.15-2
- init script fixes for LSB compliance (#546252)
 * exit code 2 for an unknown script argument
 * exit code 6 for a reload if config-test fails
 * exit code 7 for a reload on a stopped service
 * fixed help output to cover all options
- mod_dav: handle PUT failure more cleanly (#572911)

* Thu Mar 11 2010 Joe Orton <jorton@redhat.com> - 2.2.15-1
- update to 2.2.15 (#570465, #570442)
- fix version string (#572140)
- mod_ssl: use ASN1_STRING_print() in SSLRequire's OID() (#552942)
- prevent use of rsync during "make install" (#557049)
- load mod_version by default in httpd.conf

* Tue Feb  9 2010 Joe Orton <jorton@redhat.com> - 2.2.14-5
- mod_ssl: fix CVE-2009-3555 backport (#563119)
- mod_authnz_ldap: fix for dynamic group support
- mod_ssl: add SSLInsecureRenegotiation directive (#561435)

* Thu Jan 14 2010 Joe Orton <jorton@redhat.com> - 2.2.14-4
- mod_ssl: add further mitigation for CVE-2009-3555
- drop proxy_ajp.conf
- update httpd.conf: decrease Timeout to 1m, update LoadModule
  directives, bump worker to 4 proc/300clients
- fix hard-coded default pidfile to match default config (#547629)
- drop legacy X-Pad header from short responses (#526110)
- disable keepalive for Expect: 100-continue and error response (#533407)
- mod_ext_filter: fix spurious error log output (#479463)
- mod_rewrite: don't serialize logfile access (#493023)
- fix spurious error messages on graceful restart (#233955)
- mod_ssl: fix potential hang in renegotiation (#510515)
- mod_proxy_connect: support use SSL client connection (#523594)

* Wed Dec  9 2009 Joe Orton <jorton@redhat.com> - 2.2.14-3
- add 'ServerTokens Full-Release' config option (#477006)

* Tue Dec  8 2009 Joe Orton <jorton@redhat.com> - 2.2.14-2
- drop distcache support

* Thu Dec  3 2009 Joe Orton <jorton@redhat.com> - 2.2.14-1
- update to 2.2.14
- relax permissions on /var/run/httpd (#495780)
- Requires(pre): httpd in mod_ssl subpackage (#543275)
- add partial security fix for CVE-2009-3555 (#533125)

* Tue Sep  8 2009 Joe Orton <jorton@redhat.com> 2.2.13-2
- restart service in posttrans (#491567)

* Fri Aug 21 2009 Tomas Mraz <tmraz@redhat.com> - 2.2.13-2
- rebuilt with new openssl

* Tue Aug 18 2009 Joe Orton <jorton@redhat.com> 2.2.13-1
- update to 2.2.13

* Fri Jul 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.2.11-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Tue Jun 16 2009 Joe Orton <jorton@redhat.com> 2.2.11-9
- build -manual as noarch

* Tue Mar 17 2009 Joe Orton <jorton@redhat.com> 2.2.11-8
- fix pidfile in httpd.logrotate (thanks to Rainer Traut)
- don't build mod_mem_cache or mod_file_cache

* Tue Feb 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.2.11-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Thu Jan 22 2009 Joe Orton <jorton@redhat.com> 2.2.11-6
- Require: apr-util-ldap (#471898)
- init script changes: pass pidfile to status(), use status() in
  condrestart (#480602), support try-restart as alias for
  condrestart
- change /etc/httpd/run symlink to have destination /var/run/httpd,
  and restore "run/httpd.conf" as default PidFile (#478688)

* Fri Jan 16 2009 Tomas Mraz <tmraz@redhat.com> 2.2.11-5
- rebuild with new openssl

* Sat Dec 27 2008 Robert Scheck <robert@fedoraproject.org> 2.2.11-4
- Made default configuration using /var/run/httpd for pid file

* Thu Dec 18 2008 Joe Orton <jorton@redhat.com> 2.2.11-3
- update to 2.2.11
- package new /var/run/httpd directory, and move default pidfile
  location inside there

* Tue Oct 21 2008 Joe Orton <jorton@redhat.com> 2.2.10-2
- update to 2.2.10

* Tue Jul 15 2008 Joe Orton <jorton@redhat.com> 2.2.9-5
- move AddTypes for SSL cert/CRL types from ssl.conf to httpd.conf (#449979)

* Mon Jul 14 2008 Joe Orton <jorton@redhat.com> 2.2.9-4
- use Charset=UTF-8 in default httpd.conf (#455123)
- only enable suexec when appropriate (Jim Radford, #453697)

* Thu Jul 10 2008 Tom "spot" Callaway <tcallawa@redhat.com>  2.2.9-3
- rebuild against new db4 4.7

* Tue Jul  8 2008 Joe Orton <jorton@redhat.com> 2.2.9-2
- update to 2.2.9
- build event MPM too

* Wed Jun  4 2008 Joe Orton <jorton@redhat.com> 2.2.8-4
- correct UserDir directive in default config (#449815)

* Tue Feb 19 2008 Fedora Release Engineering <rel-eng@fedoraproject.org> - 2.2.8-3
- Autorebuild for GCC 4.3

* Tue Jan 22 2008 Joe Orton <jorton@redhat.com> 2.2.8-2
- update to 2.2.8
- drop mod_imagemap

* Wed Dec 05 2007 Release Engineering <rel-eng at fedoraproject dot org> - 2.2.6-4
 - Rebuild for openssl bump

* Mon Sep 17 2007 Joe Orton <jorton@redhat.com> 2.2.6-3
- add fix for SSL library string regression (PR 43334)
- use powered-by logo from system-logos (#250676)
- preserve timestamps for installed config files

* Fri Sep  7 2007 Joe Orton <jorton@redhat.com> 2.2.6-2
- update to 2.2.6 (#250757, #282761)

* Sun Sep  2 2007 Joe Orton <jorton@redhat.com> 2.2.4-10
- rebuild for fixed APR

* Wed Aug 22 2007 Joe Orton <jorton@redhat.com> 2.2.4-9
- rebuild for expat soname bump

* Tue Aug 21 2007 Joe Orton <jorton@redhat.com> 2.2.4-8
- fix License
- require /etc/mime.types (#249223)

* Thu Jul 26 2007 Joe Orton <jorton@redhat.com> 2.2.4-7
- drop -tools dependency on httpd (thanks to Matthias Saou)

* Wed Jul 25 2007 Joe Orton <jorton@redhat.com> 2.2.4-6
- split out utilities into -tools subpackage, based on patch
  by Jason Tibbs (#238257)

* Tue Jul 24 2007 Joe Orton <jorton@redhat.com> 2.2.4-5
- spec file cleanups: provide httpd-suexec, mod_dav; 
 don't obsolete mod_jk; drop trailing dots from Summaries
- init script
 * add LSB info header, support force-reload (#246944)
 * update description
 * drop 1.3 config check
 * pass $pidfile to daemon and pidfile everywhere

* Wed May  9 2007 Joe Orton <jorton@redhat.com> 2.2.4-4
- update welcome page branding

* Tue Apr  3 2007 Joe Orton <jorton@redhat.com> 2.2.4-3
- drop old triggers, old Requires, xmlto BR
- use Requires(...) correctly 
- use standard BuildRoot 
- don't mark init script as config file
- trim CHANGES further

* Mon Mar 12 2007 Joe Orton <jorton@redhat.com> 2.2.4-2
- update to 2.2.4
- drop the migration guide (#223605)

* Thu Dec  7 2006 Joe Orton <jorton@redhat.com> 2.2.3-8
- fix path to instdso.sh in special.mk (#217677)
- fix detection of links in "apachectl fullstatus"

* Tue Dec  5 2006 Joe Orton <jorton@redhat.com> 2.2.3-7
- rebuild for libpq soname bump

* Sat Nov 11 2006 Joe Orton <jorton@redhat.com> 2.2.3-6
- rebuild for BDB soname bump

* Mon Sep 11 2006 Joe Orton <jorton@redhat.com> 2.2.3-5
- updated "powered by Fedora" logo (#205573, Diana Fong)
- tweak welcome page wording slightly (#205880)

* Fri Aug 18 2006 Jesse Keating <jkeating@redhat.com> - 2.2.3-4
- rebuilt with latest binutils to pick up 64K -z commonpagesize on ppc*
  (#203001)

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

