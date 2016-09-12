%define contentdir /var/www
%define suexec_caller apache
%define mmn 20120211
%define oldmmnisa %{mmn}-%{__isa_name}-%{__isa_bits}
%define mmnisa %{mmn}%{__isa_name}%{__isa_bits}
%define vstring CentOS
%define mpms worker event

%define HTTP_FOSS_DIR "%{_baseline_workspace}/foss/%{HTTP_PACKAGE_NAME}/packaged/"
%define HTTP_PACKAGE_NAME "httpd-%{version}"
%define HTTP_PATCHES_TAR "%{HTTP_PACKAGE_NAME}-SOURCES.tar"
%define HTTP_PATCHES_RPM "httpd-%{version}-%{release}_7.src.rpm"
%define HTTP_SOURCE_TAR "%{HTTP_PACKAGE_NAME}.tar.gz"
%define RPMBUILD_PYPIES_DIR "%{_baseline_workspace}/rpmbuild/BUILD/httpd-pypies"
%define RPMBUILD_HTTP_DIR %RPMBUILD_PYPIES_DIR/%HTTP_PACKAGE_NAME


# Drop automatic provides for module DSOs
%{?filter_setup:
%filter_provides_in /awips2/httpd_pypies%{_libdir}/httpd/modules/.*\.so$
%filter_setup
}

Summary: Pypies Apache HTTP Server
Name: awips2-httpd-pypies
Version: 2.4.6
Release: 17.1%{?dist}
URL: http://httpd.apache.org/
Source0: http://www.apache.org/dist/httpd/httpd-%{version}.tar.bz2
Source1: centos-noindex.tar.gz
Source3: httpd.sysconf
Source4: httpd-ssl-pass-dialog
Source5: httpd.tmpfiles
Source6: httpd-pypies.service
Source7: action-graceful.sh
Source8: action-configtest.sh
Source10: httpd.conf
Source11: 00-base.conf
Source13: 00-lua.conf
Source14: 01-cgi.conf
Source15: 00-dav.conf
Source16: 00-proxy.conf
Source17: 00-ssl.conf
Source18: 01-ldap.conf
Source19: 00-proxyhtml.conf
Source20: userdir.conf
Source21: ssl.conf
Source22: welcome.conf
Source23: manual.conf
Source24: 00-systemd.conf
Source25: 01-session.conf
# Documentation
Source30: README.confd
# build/scripts patches
Patch1: httpd-2.4.1-apctl.patch
Patch2: httpd-2.4.3-apxs.patch
Patch3: httpd-2.4.1-deplibs.patch
Patch5: httpd-2.4.3-layout.patch
Patch6: httpd-2.4.3-apctl-systemd.patch
# Features/functional changes
Patch21: httpd-2.4.6-full-release.patch
Patch23: httpd-2.4.4-export.patch
Patch24: httpd-2.4.1-corelimit.patch
Patch25: httpd-2.4.1-selinux.patch
Patch26: httpd-2.4.4-r1337344+.patch
Patch27: httpd-2.4.2-icons.patch
Patch28: httpd-2.4.6-r1332643+.patch
Patch29: httpd-2.4.3-mod_systemd.patch
Patch30: httpd-2.4.4-cachehardmax.patch
Patch31: httpd-2.4.6-sslmultiproxy.patch
Patch32: httpd-2.4.6-r1537535.patch
Patch33: httpd-2.4.6-r1542327.patch
# Bug fixes
Patch51: httpd-2.4.3-sslsninotreq.patch
Patch55: httpd-2.4.4-malformed-host.patch
Patch56: httpd-2.4.4-mod_unique_id.patch
Patch57: httpd-2.4.6-ldaprefer.patch
Patch58: httpd-2.4.6-r1507681+.patch
Patch59: httpd-2.4.6-r1556473.patch
Patch60: httpd-2.4.6-r1553540.patch
# Security fixes
Patch200: httpd-2.4.6-CVE-2013-6438.patch
Patch201: httpd-2.4.6-CVE-2014-0098.patch
License: ASL 2.0
Group: AWIPSII
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: autoconf, perl, pkgconfig, findutils, xmlto, httpd-devel
BuildRequires: zlib-devel, libselinux-devel, lua-devel, cpp, gcc
BuildRequires: apr-devel >= 1.4.0, apr-util-devel >= 1.2.0, pcre-devel >= 5.0
BuildRequires: systemd-devel
Requires: /etc/mime.types, system-logos >= 7.92.1-1
Requires: awips2-tools, awips2-pypies
Obsoletes: httpd-suexec
Provides: webserver
Provides: mod_dav = %{version}-%{release}, httpd-suexec = %{version}-%{release}
Provides: httpd-mmn = %{mmn}, httpd-mmn = %{mmnisa}, httpd-mmn = %{oldmmnisa}
Requires(pre): /usr/sbin/useradd
Requires(preun): systemd-units
Requires(postun): systemd-units
Requires(post): systemd-units

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
Requires(post): openssl, /bin/cat
Requires(pre): httpd
Requires: httpd = 0:%{version}-%{release}, httpd-mmn = %{mmnisa}
Obsoletes: stronghold-mod_ssl

%description -n mod_ssl
The mod_ssl module provides strong cryptography for the Apache Web
server via the Secure Sockets Layer (SSL) and Transport Layer
Security (TLS) protocols.

%package -n mod_proxy_html
Group: AWIPSII
Summary: HTML and XML content filters for the Apache HTTP Server
Requires: httpd = 0:%{version}-%{release}, httpd-mmn = %{mmnisa}
BuildRequires: libxml2-devel
Epoch: 1
Obsoletes: mod_proxy_html < 1:2.4.1-2

%description -n mod_proxy_html
The mod_proxy_html and mod_xml2enc modules provide filters which can
transform and modify HTML and XML content.

%package -n mod_ldap
Group: AWIPSII
Summary: LDAP authentication modules for the Apache HTTP Server
Requires: httpd = 0:%{version}-%{release}, httpd-mmn = %{mmnisa}
Requires: apr-util-ldap

%description -n mod_ldap
The mod_ldap and mod_authnz_ldap modules add support for LDAP
authentication to the Apache HTTP Server.

%package -n mod_session
Group: AWIPSII
Summary: Session interface for the Apache HTTP Server
Requires: httpd = 0:%{version}-%{release}, httpd-mmn = %{mmnisa}

%description -n mod_session
The mod_session module and associated backends provide an abstract
interface for storing and accessing per-user session data.

%prep
%setup -q -n httpd-2.4.6
%patch1 -p1 -b .apctl
%patch2 -p1 -b .apxs
%patch3 -p1 -b .deplibs
%patch5 -p1 -b .layout
%patch6 -p1 -b .apctlsystemd

%patch21 -p1 -b .fullrelease
%patch23 -p1 -b .export
%patch24 -p1 -b .corelimit
%patch25 -p1 -b .selinux
%patch26 -p1 -b .r1337344+
%patch27 -p1 -b .icons
%patch28 -p1 -b .r1332643+
%patch29 -p1 -b .systemd
%patch30 -p1 -b .cachehardmax
%patch31 -p1 -b .sslmultiproxy
%patch32 -p1 -b .r1537535
%patch33 -p1 -b .r1542327
rm modules/ssl/ssl_engine_dh.c

%patch51 -p1 -b .sninotreq
%patch55 -p1 -b .malformedhost
%patch56 -p1 -b .uniqueid
%patch57 -p1 -b .ldaprefer
%patch58 -p1 -b .r1507681+
%patch59 -p1 -b .r1556473
%patch60 -p1 -b .r1553540

%patch200 -p1 -b .cve6438
%patch201 -p1 -b .cve0098

# Patch in the vendor string and the release string
sed -i '/^#define PLATFORM/s/Unix/%{vstring}/' os/unix/os.h
sed -i 's/@RELEASE@/%{release}/' server/core.c

# Prevent use of setcap in "install-suexec-caps" target.
sed -i '/suexec/s,setcap ,echo Skipping setcap for ,' Makefile.in

# Safety check: prevent build if defined MMN does not equal upstream MMN.
vmmn=`echo MODULE_MAGIC_NUMBER_MAJOR | cpp -include include/ap_mmn.h | sed -n '/^2/p'`
if test "x${vmmn}" != "x%{mmn}"; then
   : Error: Upstream MMN is now ${vmmn}, packaged MMN is %{mmn}
   : Update the mmn macro and rebuild.
   exit 1
fi

: Building with MMN %{mmn}, MMN-ISA %{mmnisa} and vendor string '%{vstring}'

%build
# forcibly prevent use of bundled apr, apr-util, pcre
rm -rf srclib/{apr,apr-util,pcre}

# regenerate configure scripts
autoheader && autoconf || exit 1

# Before configure; fix location of build dir in generated apxs
%{__perl} -pi -e "s:\@exp_installbuilddir\@:%{_libdir}/httpd/build:g" \
	support/apxs.in

export CFLAGS=$RPM_OPT_FLAGS
export LDFLAGS="-Wl,-z,relro,-z,now"

%ifarch ppc64
CFLAGS="$CFLAGS -O3"
%endif

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
        --enable-layout=Fedora \
        --with-installbuilddir=/awips2/httpd_pypies%{_libdir}/httpd/build \
	--with-mpm=$mpm \
        --with-apr=%{_prefix} --with-apr-util=%{_prefix} \
	--enable-suexec --with-suexec \
        --enable-suexec-capabilities \
	--with-suexec-caller=%{suexec_caller} \
	--with-suexec-docroot=/awips2/httpd_pypies%{contentdir} \
	--with-suexec-logfile=/awips2/httpd_pypies%{_localstatedir}/log/httpd/suexec.log \
	--with-suexec-bin=/awips2/httpd_pypies%{_sbindir}/suexec \
	--with-suexec-uidmin=500 --with-suexec-gidmin=100 \
        --enable-pie \
        --with-pcre \
        --enable-mods-shared=all \
	--enable-ssl --with-ssl --disable-distcache \
	--enable-proxy \
        --enable-cache \
        --enable-disk-cache \
        --enable-ldap --enable-authnz-ldap \
        --enable-cgid --enable-cgi \
        --enable-authn-anon --enable-authn-alias \
        --disable-imagemap  \
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

pushd prefork
make DESTDIR=$RPM_BUILD_ROOT install
popd

# install alternative MPMs
for f in %{mpms}; do
  install -m 755 ${f}/httpd $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/httpd.${f}
done

# Install systemd service files
mkdir -p $RPM_BUILD_ROOT%{_unitdir}
install -p -m 644 $RPM_SOURCE_DIR/httpd-pypies.service \
                  $RPM_BUILD_ROOT%{_unitdir}/httpd-pypies.service

# install conf file/directory
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d
install -m 644 $RPM_SOURCE_DIR/README.confd \
    $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/README
for f in 00-base.conf 00-lua.conf 01-cgi.conf 00-dav.conf \
         00-proxy.conf 00-ssl.conf 01-ldap.conf 00-proxyhtml.conf \
         01-ldap.conf 00-systemd.conf 01-session.conf; do
  install -m 644 -p $RPM_SOURCE_DIR/$f \
        $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/$f
done

for f in welcome.conf ssl.conf manual.conf userdir.conf; do
  install -m 644 -p $RPM_SOURCE_DIR/$f \
        $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/$f
done

# Split-out extra config shipped as default in conf.d:
#for f in autoindex; do
#  mv $RPM_SOURCE_DIR/docs/conf/extra/httpd-${f}.conf \
#        $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/${f}.conf
#done

# Extra config trimmed:
#rm -v docs/conf/extra/httpd-{ssl,userdir}.conf

#rm $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/conf/*.conf
install -m 644 -p $RPM_SOURCE_DIR/httpd.conf \
   $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/conf/httpd.conf

mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/sysconfig
install -m 644 -p $RPM_SOURCE_DIR/httpd.sysconf \
                  $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/sysconfig/httpd

# tmpfiles.d configuration
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_prefix}/lib/tmpfiles.d 
install -m 644 -p $RPM_SOURCE_DIR/httpd.tmpfiles \
   $RPM_BUILD_ROOT/awips2/httpd_pypies%{_prefix}/lib/tmpfiles.d/httpd.conf

# Other directories
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/lib/dav

# Create cache directory
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/cache/httpd \
         $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/cache/httpd/proxy \
         $RPM_BUILD_ROOT/awips2/httpd_pypies%{_localstatedir}/cache/httpd/ssl

# Make the MMN accessible to module packages
echo %{mmnisa} > $RPM_BUILD_ROOT/awips2/httpd_pypies%{_includedir}/httpd/.mmn

mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/rpm
cat > $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/rpm/macros.httpd <<EOF
%%_httpd_mmn /awips2/httpd_pypies%{mmnisa}
%%_httpd_apxs /awips2/httpd_pypies%{_bindir}/apxs
%%_httpd_modconfdir /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d
%%_httpd_confdir /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d
%%_httpd_contentdir /awips2/httpd_pypies%{contentdir}
%%_httpd_moddir /awips2/httpd_pypies%{_libdir}/httpd/modules
EOF

# Handle contentdir
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/noindex
tar xzf $RPM_SOURCE_DIR/centos-noindex.tar.gz \
        -C $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/noindex/ \
        --strip-components=1

rm -rf /awips2/httpd_pypies%{contentdir}/htdocs

# docroot
#mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/html
#install -m 644 -p $RPM_SOURCE_DIR/index.html \
#        $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/error/noindex.html

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

# Clean Document Root
#rm -v $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/html/*.html \
#      $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/cgi-bin/*

# Symlink for the powered-by-$DISTRO image:
ln -s ../noindex/images/poweredby.png \
        $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/icons/poweredby.png

# Set up /var directories
#rmdir $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sysconfdir}/httpd/logs
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

# install http-ssl-pass-dialog
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libexecdir}
install -m755 $RPM_SOURCE_DIR/httpd-ssl-pass-dialog \
	$RPM_BUILD_ROOT/awips2/httpd_pypies%{_libexecdir}/httpd-ssl-pass-dialog

# Install action scripts
mkdir -p $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libexecdir}/initscripts/legacy-actions/httpd
for f in graceful configtest; do
    install -p -m 755 $RPM_SOURCE_DIR/action-${f}.sh \
            $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libexecdir}/initscripts/legacy-actions/httpd/${f}
done

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
    -e "s|/usr/local/apache2/logs/httpd.pid|/run/httpd/httpd.pid|" \
    -e "s|/usr/local/apache2|/etc/httpd|" < docs/man/httpd.8 \
  > $RPM_BUILD_ROOT/awips2/httpd_pypies%{_mandir}/man8/httpd.8

# Make ap_config_layout.h libdir-agnostic
sed -i '/.*DEFAULT_..._LIBEXECDIR/d;/DEFAULT_..._INSTALLBUILDDIR/d' \
    $RPM_BUILD_ROOT/awips2/httpd_pypies%{_includedir}/httpd/ap_config_layout.h

# Fix path to instdso in special.mk
sed -i '/instdso/s,top_srcdir,top_builddir,' \
    $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/httpd/build/special.mk

# Remove unpackaged files
rm -vf \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/*.exp \
      $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/conf/mime.types \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/httpd/modules/*.exp \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_libdir}/httpd/build/config.nice \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_bindir}/{ap?-config,dbmmanage} \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/{checkgid,envvars*} \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/htdocs/* \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{_mandir}/man1/dbmmanage.* \
      $RPM_BUILD_ROOT/awips2/httpd_pypies%{contentdir}/cgi-bin/*

rm -rf $RPM_BUILD_ROOT/awips2/httpd_pypies/etc/httpd/conf/{original,extra}

# Make suexec a+rw so it can be stripped.  %%files lists real permissions
chmod 755 $RPM_BUILD_ROOT/awips2/httpd_pypies%{_sbindir}/suexec
# build mod_wsgi.so
_MOD_WSGI_VERSION=3.5
/bin/cp %{_baseline_workspace}/rpms/awips2.core/Installer.httpd-pypies/src/mod_wsgi-${_MOD_WSGI_VERSION}.tar.gz \
   %{_topdir}/BUILD
if [ $? -ne 0 ]; then
   exit 1
fi

pushd . > /dev/null
cd %{_topdir}/BUILD
if [ -d mod_wsgi-${_MOD_WSGI_VERSION} ]; then
   /bin/rm -rf mod_wsgi-${_MOD_WSGI_VERSION}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
/bin/tar -xvf mod_wsgi-${_MOD_WSGI_VERSION}.tar.gz
if [ $? -ne 0 ]; then
   exit 1
fi
cd mod_wsgi-${_MOD_WSGI_VERSION}
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
/bin/rm -f mod_wsgi-${_MOD_WSGI_VERSION}.tar.gz
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/rm -rf mod_wsgi-${_MOD_WSGI_VERSION}
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

%post
%systemd_post httpd-pypies.service

%preun
%systemd_preun httpd-pypies.service

%postun
%systemd_postun

# Trigger for conversion from SysV, per guidelines at:
# https://fedoraproject.org/wiki/Packaging:ScriptletSnippets#Systemd
%triggerun -- httpd < 2.2.21-5
# Save the current service runlevel info
# User must manually run systemd-sysv-convert --apply httpd
# to migrate them to systemd targets
/usr/bin/systemd-sysv-convert --save httpd-pypies.service >/dev/null 2>&1 ||:

# Run these because the SysV package being removed won't do them
#/sbin/chkconfig --del httpd-pypies >/dev/null 2>&1 || :

%posttrans
test -f /etc/sysconfig/httpd-disable-posttrans || \
  /bin/systemctl try-restart httpd-pypies.service >/dev/null 2>&1 || :

%define sslcert /awips2/httpd_pypies%{_sysconfdir}/pki/tls/certs/localhost.crt
%define sslkey /awips2/httpd_pypies%{_sysconfdir}/pki/tls/private/localhost.key

%post -n mod_ssl
umask 077

if [ -f %{sslkey} -o -f %{sslcert} ]; then
   exit 0
fi

/awips2/httpd_pypies%{_bindir}/openssl genrsa -rand /proc/apm:/proc/cpuinfo:/proc/dma:/proc/filesystems:/proc/interrupts:/proc/ioports:/proc/pci:/proc/rtc:/proc/uptime 2048 > %{sslkey} 2> /dev/null

FQDN=`hostname`
if [ "x${FQDN}" = "x" ]; then
   FQDN=localhost.localdomain
fi

cat << EOF | /awips2/httpd_pypies%{_bindir}/openssl req -new -key %{sslkey} \
         -x509 -sha256 -days 365 -set_serial $RANDOM -extensions v3_req \
         -out %{sslcert} 2>/dev/null
--
SomeState
SomeCity
SomeOrganization
SomeOrganizationalUnit
${FQDN}
root@${FQDN}
EOF

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
echo "cleaning"
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,awips,awips)

%doc ABOUT_APACHE README CHANGES LICENSE VERSIONING NOTICE

%dir /awips2/httpd_pypies
%dir /awips2/httpd_pypies/etc
%dir /awips2/httpd_pypies/etc/sysconfig
%dir /awips2/httpd_pypies/usr
%dir /awips2/httpd_pypies/usr/bin
/awips2/httpd_pypies/usr/bin/*
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
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/*.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf/magic

%{_sysconfdir}/cron.daily/pypiesLogCleanup.sh

%dir /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d
/awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/README

%exclude /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/ssl.conf
%exclude /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/manual.conf
%exclude /var/www/

%dir /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/*.conf
%exclude /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/00-ssl.conf
%exclude /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/00-proxyhtml.conf
%exclude /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/01-ldap.conf
%exclude /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/01-session.conf

%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/sysconfig/ht*
/awips2/httpd_pypies%{_prefix}/lib/tmpfiles.d/httpd.conf

%dir /awips2/httpd_pypies%{_libexecdir}/initscripts/legacy-actions/httpd
/awips2/httpd_pypies%{_libexecdir}/initscripts/legacy-actions/httpd/*

/awips2/httpd_pypies%{_sbindir}/ht*
/awips2/httpd_pypies%{_sbindir}/fcgistarter
/awips2/httpd_pypies%{_sbindir}/apachectl
/awips2/httpd_pypies%{_sbindir}/rotatelogs
%caps(cap_setuid,cap_setgid+pe) %attr(510,root,%{suexec_caller}) /awips2/httpd_pypies%{_sbindir}/suexec

%dir /awips2/httpd_pypies%{_libdir}/httpd
%dir /awips2/httpd_pypies%{_libdir}/httpd/modules
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod*.so
%exclude /awips2/httpd_pypies%{_libdir}/httpd/modules/mod_auth_form.so
%exclude /awips2/httpd_pypies%{_libdir}/httpd/modules/mod_ssl.so
%exclude /awips2/httpd_pypies%{_libdir}/httpd/modules/mod_*ldap.so
%exclude /awips2/httpd_pypies%{_libdir}/httpd/modules/mod_proxy_html.so
%exclude /awips2/httpd_pypies%{_libdir}/httpd/modules/mod_xml2enc.so
%exclude /awips2/httpd_pypies%{_libdir}/httpd/modules/mod_session*.so

%dir /awips2/httpd_pypies%{contentdir}
%dir /awips2/httpd_pypies%{contentdir}/icons
%dir /awips2/httpd_pypies%{contentdir}/error
%dir /awips2/httpd_pypies%{contentdir}/error/include
%dir /awips2/httpd_pypies%{contentdir}/noindex
/awips2/httpd_pypies%{contentdir}/icons/*
/awips2/httpd_pypies%{contentdir}/error/README
/awips2/httpd_pypies%{contentdir}/error/*.var
/awips2/httpd_pypies%{contentdir}/error/include/*.html
/awips2/httpd_pypies%{contentdir}/noindex/*

#%attr(0710,awips,awips) %dir /awips2/httpd_pypies/run/httpd
%attr(0700,awips,awips) %dir /awips2/httpd_pypies%{_localstatedir}/log/httpd
%attr(0700,awips,awips) %dir /awips2/httpd_pypies%{_localstatedir}/lib/dav
%attr(0700,awips,awips) %dir /awips2/httpd_pypies%{_localstatedir}/cache/httpd
%attr(0700,awips,awips) %dir /awips2/httpd_pypies%{_localstatedir}/cache/httpd/proxy

%{_unitdir}/*.service

%files manual
%defattr(-,root,root)
/awips2/httpd_pypies%{contentdir}/manual
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/manual.conf

%files -n mod_ssl
%defattr(-,root,root)
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod_ssl.so
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/00-ssl.conf
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.d/ssl.conf
%attr(0700,apache,root) %dir /awips2/httpd_pypies%{_localstatedir}/cache/httpd/ssl
/awips2/httpd_pypies%{_libexecdir}/httpd-ssl-pass-dialog

%files -n mod_proxy_html
%defattr(-,root,root)
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod_proxy_html.so
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod_xml2enc.so
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/00-proxyhtml.conf

%files -n mod_ldap
%defattr(-,root,root)
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod_*ldap.so
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/01-ldap.conf

%files -n mod_session
%defattr(-,root,root)
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod_session*.so
/awips2/httpd_pypies%{_libdir}/httpd/modules/mod_auth_form.so
%config(noreplace) /awips2/httpd_pypies%{_sysconfdir}/httpd/conf.modules.d/01-session.conf

%files devel
%defattr(-,root,root)
/awips2/httpd_pypies%{_includedir}/httpd
/awips2/httpd_pypies%{_bindir}/apxs
/awips2/httpd_pypies%{_mandir}/man1/apxs.1*
%dir /awips2/httpd_pypies%{_libdir}/httpd/build
/awips2/httpd_pypies%{_libdir}/httpd/build/*.mk
/awips2/httpd_pypies%{_libdir}/httpd/build/*.sh
/awips2/httpd_pypies%{_sysconfdir}/rpm/macros.httpd

%changelog
* Tue Jun 17 2014 Jim Perrin <jperrin@centos.org> - 2.4.6-17.el7.centos.1
- Remove index.html, add centos-noindex.tar.gz
- update welcome.conf with proper aliases
- change symlink for poweredby.png

* Thu Mar 20 2014 Jan Kaluza <jkaluza@redhat.com> - 2.4.6-17
- mod_dav: add security fix for CVE-2013-6438 (#1077907)
- mod_log_config: add security fix for CVE-2014-0098 (#1077907)

* Wed Mar  5 2014 Joe Orton <jorton@redhat.com> - 2.4.6-16
- mod_ssl: improve DH temp key handling (#1057687)

* Wed Mar  5 2014 Joe Orton <jorton@redhat.com> - 2.4.6-15
- mod_ssl: use 2048-bit RSA key with SHA-256 signature in dummy certificate (#1071276)

* Fri Jan 24 2014 Daniel Mach <dmach@redhat.com> - 2.4.6-14
- Mass rebuild 2014-01-24

* Mon Jan 13 2014 Joe Orton <jorton@redhat.com> - 2.4.6-13
- mod_ssl: sanity-check use of "SSLCompression" (#1036666)
- mod_proxy_http: fix brigade memory usage (#1040447)

* Fri Jan 10 2014 Joe Orton <jorton@redhat.com> - 2.4.6-12
- rebuild

* Thu Jan  9 2014 Joe Orton <jorton@redhat.com> - 2.4.6-11
- build with -O3 on ppc64 (#1051066)

* Tue Jan  7 2014 Joe Orton <jorton@redhat.com> - 2.4.6-10
- mod_dav: fix locktoken handling (#1004046)

* Fri Dec 27 2013 Daniel Mach <dmach@redhat.com> - 2.4.6-9
- Mass rebuild 2013-12-27

* Fri Dec 20 2013 Joe Orton <jorton@redhat.com> - 2.4.6-8
- use unambiguous httpd-mmn (#1029360)

* Fri Nov   1 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.6-7
- mod_ssl: allow SSLEngine to override Listen-based default (#1023168)

* Thu Oct  31 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.6-6
- systemd: Use {MAINPID} notation in service file (#969972)

* Thu Oct 24 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.6-5
- systemd: send SIGWINCH signal without httpd -k in ExecStop (#969972)

* Thu Oct 03 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.6-4
- expand macros in macros.httpd (#1011393)

* Mon Aug 26 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.6-3
- fix "LDAPReferrals off" to really disable LDAP Referrals

* Wed Jul 31 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.6-2
- revert fix for dumping vhosts twice

* Mon Jul 22 2013 Joe Orton <jorton@redhat.com> - 2.4.6-1
- update to 2.4.6
- mod_ssl: use revised NPN API (r1487772)

* Thu Jul 11 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.4-12
- mod_unique_id: replace use of hostname + pid with PRNG output (#976666)
- apxs: mention -p option in manpage

* Tue Jul  2 2013 Joe Orton <jorton@redhat.com> - 2.4.4-11
- add patch for aarch64 (Dennis Gilmore, #925558)

* Mon Jul  1 2013 Joe Orton <jorton@redhat.com> - 2.4.4-10
- remove duplicate apxs man page from httpd-tools

* Mon Jun 17 2013 Joe Orton <jorton@redhat.com> - 2.4.4-9
- remove zombie dbmmanage script

* Fri May 31 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.4-8
- return 400 Bad Request on malformed Host header

* Mon May 20 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.4-6
- htpasswd/htdbm: fix hash generation bug (#956344)
- do not dump vhosts twice in httpd -S output (#928761)
- mod_cache: fix potential crash caused by uninitialized variable (#954109)

* Thu Apr 18 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.4-5
- execute systemctl reload as result of apachectl graceful
- mod_ssl: ignore SNI hints unless required by config
- mod_cache: forward-port CacheMaxExpire "hard" option
- mod_ssl: fall back on another module's proxy hook if mod_ssl proxy
  is not configured.

* Tue Apr 16 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.4-4
- fix service file to not send SIGTERM after ExecStop (#906321, #912288)

* Tue Mar 26 2013 Jan Kaluza <jkaluza@redhat.com> - 2.4.4-3
- protect MIMEMagicFile with IfModule (#893949)

* Tue Feb 26 2013 Joe Orton <jorton@redhat.com> - 2.4.4-2
- really package mod_auth_form in mod_session (#915438)

* Tue Feb 26 2013 Joe Orton <jorton@redhat.com> - 2.4.4-1
- update to 2.4.4
- fix duplicate ownership of mod_session config (#914901)

* Fri Feb 22 2013 Joe Orton <jorton@redhat.com> - 2.4.3-17
- add mod_session subpackage, move mod_auth_form there (#894500)

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.4.3-16
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Tue Jan  8 2013 Joe Orton <jorton@redhat.com> - 2.4.3-15
- add systemd service for htcacheclean

* Tue Nov 13 2012 Joe Orton <jorton@redhat.com> - 2.4.3-14
- drop patch for r1344712

* Tue Nov 13 2012 Joe Orton <jorton@redhat.com> - 2.4.3-13
- filter mod_*.so auto-provides (thanks to rcollet)
- pull in syslog logging fix from upstream (r1344712)

* Fri Oct 26 2012 Joe Orton <jorton@redhat.com> - 2.4.3-12
- rebuild to pick up new apr-util-ldap

* Tue Oct 23 2012 Joe Orton <jorton@redhat.com> - 2.4.3-11
- rebuild

* Wed Oct  3 2012 Joe Orton <jorton@redhat.com> - 2.4.3-10
- pull upstream patch r1392850 in addition to r1387633

* Mon Oct  1 2012 Joe Orton <jorton@redhat.com> - 2.4.3-9.1
- restore "ServerTokens Full-Release" support (#811714)

* Mon Oct  1 2012 Joe Orton <jorton@redhat.com> - 2.4.3-9
- define PLATFORM in os.h using vendor string

* Mon Oct  1 2012 Joe Orton <jorton@redhat.com> - 2.4.3-8
- use systemd script unconditionally (#850149)

* Mon Oct  1 2012 Joe Orton <jorton@redhat.com> - 2.4.3-7
- use systemd scriptlets if available (#850149)
- don't run posttrans restart if /etc/sysconfig/httpd-disable-posttrans exists

* Mon Oct 01 2012 Jan Kaluza <jkaluza@redhat.com> - 2.4.3-6
- use systemctl from apachectl (#842736)

* Wed Sep 19 2012 Joe Orton <jorton@redhat.com> - 2.4.3-5
- fix some error log spam with graceful-stop (r1387633)
- minor mod_systemd tweaks

* Thu Sep 13 2012 Joe Orton <jorton@redhat.com> - 2.4.3-4
- use IncludeOptional for conf.d/*.conf inclusion

* Fri Sep 07 2012 Jan Kaluza <jkaluza@redhat.com> - 2.4.3-3
- adding mod_systemd to integrate with systemd better

* Tue Aug 21 2012 Joe Orton <jorton@redhat.com> - 2.4.3-2
- mod_ssl: add check for proxy keypair match (upstream r1374214)

* Tue Aug 21 2012 Joe Orton <jorton@redhat.com> - 2.4.3-1
- update to 2.4.3 (#849883)
- own the docroot (#848121)

* Mon Aug  6 2012 Joe Orton <jorton@redhat.com> - 2.4.2-23
- add mod_proxy fixes from upstream (r1366693, r1365604)

* Thu Jul 19 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.4.2-22
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Fri Jul  6 2012 Joe Orton <jorton@redhat.com> - 2.4.2-21
- drop explicit version requirement on initscripts

* Thu Jul  5 2012 Joe Orton <jorton@redhat.com> - 2.4.2-20
- mod_ext_filter: fix error_log warnings

* Mon Jul  2 2012 Joe Orton <jorton@redhat.com> - 2.4.2-19
- support "configtest" and "graceful" as initscripts "legacy actions"

* Fri Jun  8 2012 Joe Orton <jorton@redhat.com> - 2.4.2-18
- avoid use of "core" GIF for a "core" directory (#168776)
- drop use of "syslog.target" in systemd unit file

* Thu Jun  7 2012 Joe Orton <jorton@redhat.com> - 2.4.2-17
- use _unitdir for systemd unit file
- use /run in unit file, ssl.conf

* Thu Jun  7 2012 Joe Orton <jorton@redhat.com> - 2.4.2-16
- mod_ssl: fix NPN patch merge

* Wed Jun  6 2012 Joe Orton <jorton@redhat.com> - 2.4.2-15
- move tmpfiles.d fragment into /usr/lib per new guidelines
- package /run/httpd not /var/run/httpd
- set runtimedir to /run/httpd likewise

* Wed Jun  6 2012 Joe Orton <jorton@redhat.com> - 2.4.2-14
- fix htdbm/htpasswd crash on crypt() failure (#818684)

* Wed Jun  6 2012 Joe Orton <jorton@redhat.com> - 2.4.2-13
- pull fix for NPN patch from upstream (r1345599)

* Thu May 31 2012 Joe Orton <jorton@redhat.com> - 2.4.2-12
- update suexec patch to use LOG_AUTHPRIV facility

* Thu May 24 2012 Joe Orton <jorton@redhat.com> - 2.4.2-11
- really fix autoindex.conf (thanks to remi@)

* Thu May 24 2012 Joe Orton <jorton@redhat.com> - 2.4.2-10
- fix autoindex.conf to allow symlink to poweredby.png

* Wed May 23 2012 Joe Orton <jorton@redhat.com> - 2.4.2-9
- suexec: use upstream version of patch for capability bit support

* Wed May 23 2012 Joe Orton <jorton@redhat.com> - 2.4.2-8
- suexec: use syslog rather than suexec.log, drop dac_override capability

* Tue May  1 2012 Joe Orton <jorton@redhat.com> - 2.4.2-7
- mod_ssl: add TLS NPN support (r1332643, #809599)

* Tue May  1 2012 Joe Orton <jorton@redhat.com> - 2.4.2-6
- add BR on APR >= 1.4.0

* Fri Apr 27 2012 Joe Orton <jorton@redhat.com> - 2.4.2-5
- use systemctl from logrotate (#221073)

* Fri Apr 27 2012 Joe Orton <jorton@redhat.com> - 2.4.2-4
- pull from upstream:
  * use TLS close_notify alert for dummy_connection (r1326980+)
  * cleanup symbol exports (r1327036+)

* Fri Apr 27 2012 Joe Orton <jorton@redhat.com> - 2.4.2-3.2
- rebuild

* Fri Apr 20 2012 Joe Orton <jorton@redhat.com> - 2.4.2-3
- really fix restart

* Fri Apr 20 2012 Joe Orton <jorton@redhat.com> - 2.4.2-2
- tweak default ssl.conf
- fix restart handling (#814645)
- use graceful restart by default

* Wed Apr 18 2012 Jan Kaluza <jkaluza@redhat.com> - 2.4.2-1
- update to 2.4.2

* Fri Mar 23 2012 Joe Orton <jorton@redhat.com> - 2.4.1-6
- fix macros

* Fri Mar 23 2012 Joe Orton <jorton@redhat.com> - 2.4.1-5
- add _httpd_moddir to macros

* Tue Mar 13 2012 Joe Orton <jorton@redhat.com> - 2.4.1-4
- fix symlink for poweredby.png
- fix manual.conf

* Tue Mar 13 2012 Joe Orton <jorton@redhat.com> - 2.4.1-3
- add mod_proxy_html subpackage (w/mod_proxy_html + mod_xml2enc)
- move mod_ldap, mod_authnz_ldap to mod_ldap subpackage

* Tue Mar 13 2012 Joe Orton <jorton@redhat.com> - 2.4.1-2
- clean docroot better
- ship proxy, ssl directories within /var/cache/httpd
- default config:
 * unrestricted access to (only) /var/www
 * remove (commented) Mutex, MaxRanges, ScriptSock
 * split autoindex config to conf.d/autoindex.conf
- ship additional example configs in docdir

* Tue Mar  6 2012 Joe Orton <jorton@redhat.com> - 2.4.1-1
- update to 2.4.1
- adopt upstream default httpd.conf (almost verbatim)
- split all LoadModules to conf.modules.d/*.conf
- include conf.d/*.conf at end of httpd.conf
- trim %%changelog

* Mon Feb 13 2012 Joe Orton <jorton@redhat.com> - 2.2.22-2
- fix build against PCRE 8.30

* Mon Feb 13 2012 Joe Orton <jorton@redhat.com> - 2.2.22-1
- update to 2.2.22

* Fri Feb 10 2012 Petr Pisar <ppisar@redhat.com> - 2.2.21-8
- Rebuild against PCRE 8.30

* Mon Jan 23 2012 Jan Kaluza <jkaluza@redhat.com> - 2.2.21-7
- fix #783629 - start httpd after named

* Mon Jan 16 2012 Joe Orton <jorton@redhat.com> - 2.2.21-6
- complete conversion to systemd, drop init script (#770311)
- fix comments in /etc/sysconfig/httpd (#771024)
- enable PrivateTmp in service file (#781440)
- set LANG=C in /etc/sysconfig/httpd

* Fri Jan 13 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.2.21-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Tue Dec 06 2011 Jan Kaluza <jkaluza@redhat.com> - 2.2.21-4
- fix #751591 - start httpd after remote-fs

* Mon Oct 24 2011 Jan Kaluza <jkaluza@redhat.com> - 2.2.21-3
- allow change state of BalancerMember in mod_proxy_balancer web interface

* Thu Sep 22 2011 Ville Skyttä <ville.skytta@iki.fi> - 2.2.21-2
- Make mmn available as %%{_httpd_mmn}.
- Add .svgz to AddEncoding x-gzip example in httpd.conf.

* Tue Sep 13 2011 Joe Orton <jorton@redhat.com> - 2.2.21-1
- update to 2.2.21

* Mon Sep  5 2011 Joe Orton <jorton@redhat.com> - 2.2.20-1
- update to 2.2.20
- fix MPM stub man page generation

* Wed Aug 10 2011 Jan Kaluza <jkaluza@redhat.com> - 2.2.19-5
- fix #707917 - add httpd-ssl-pass-dialog to ask for SSL password using systemd

* Fri Jul 22 2011 Iain Arnell <iarnell@gmail.com> 1:2.2.19-4
- rebuild while rpm-4.9.1 is untagged to remove trailing slash in provided
  directory names

* Wed Jul 20 2011 Jan Kaluza <jkaluza@redhat.com> - 2.2.19-3
- fix #716621 - suexec now works without setuid bit

* Thu Jul 14 2011 Jan Kaluza <jkaluza@redhat.com> - 2.2.19-2
- fix #689091 - backported patch from 2.3 branch to support IPv6 in logresolve

* Fri Jul  1 2011 Joe Orton <jorton@redhat.com> - 2.2.19-1
- update to 2.2.19
- enable dbd, authn_dbd in default config

* Thu Apr 14 2011 Joe Orton <jorton@redhat.com> - 2.2.17-13
- fix path expansion in service files

* Tue Apr 12 2011 Joe Orton <jorton@redhat.com> - 2.2.17-12
- add systemd service files (#684175, thanks to Jóhann B. Guðmundsson)

* Wed Mar 23 2011 Joe Orton <jorton@redhat.com> - 2.2.17-11
- minor updates to httpd.conf
- drop old patches

* Wed Mar  2 2011 Joe Orton <jorton@redhat.com> - 2.2.17-10
- rebuild

* Wed Feb 23 2011 Joe Orton <jorton@redhat.com> - 2.2.17-9
- use arch-specific mmn

* Wed Feb 09 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.2.17-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Mon Jan 31 2011 Joe Orton <jorton@redhat.com> - 2.2.17-7
- generate dummy mod_ssl cert with CA:FALSE constraint (#667841)
- add man page stubs for httpd.event, httpd.worker
- drop distcache support
- add STOP_TIMEOUT support to init script

* Sat Jan  8 2011 Joe Orton <jorton@redhat.com> - 2.2.17-6
- update default SSLCipherSuite per upstream trunk

* Wed Jan  5 2011 Joe Orton <jorton@redhat.com> - 2.2.17-5
- fix requires (#667397)

* Wed Jan  5 2011 Joe Orton <jorton@redhat.com> - 2.2.17-4
- de-ghost /var/run/httpd

* Tue Jan  4 2011 Joe Orton <jorton@redhat.com> - 2.2.17-3
- add tmpfiles.d configuration, ghost /var/run/httpd (#656600)

* Sat Nov 20 2010 Joe Orton <jorton@redhat.com> - 2.2.17-2
- drop setuid bit, use capabilities for suexec binary

* Wed Oct 27 2010 Joe Orton <jorton@redhat.com> - 2.2.17-1
- update to 2.2.17

* Fri Sep 10 2010 Joe Orton <jorton@redhat.com> - 2.2.16-2
- link everything using -z relro and -z now

* Mon Jul 26 2010 Joe Orton <jorton@redhat.com> - 2.2.16-1
- update to 2.2.16

* Fri Jul  9 2010 Joe Orton <jorton@redhat.com> - 2.2.15-3
- default config tweaks:
 * harden httpd.conf w.r.t. .htaccess restriction (#591293)
 * load mod_substitute, mod_version by default
 * drop proxy_ajp.conf, load mod_proxy_ajp in httpd.conf
 * add commented list of shipped-but-unloaded modules
 * bump up worker defaults a little
 * drop KeepAliveTimeout to 5 secs per upstream
- fix LSB compliance in init script (#522074)
- bundle NOTICE in -tools
- use init script in logrotate postrotate to pick up PIDFILE
- drop some old Obsoletes/Conflicts

* Sun Apr 04 2010 Robert Scheck <robert@fedoraproject.org> - 2.2.15-1
- update to 2.2.15 (#572404, #579311)

