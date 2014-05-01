dnl $Id: aclocal.m4,v 1.27.2.1.2.1.2.1.2.16 2009/05/21 20:30:36 steve Exp $
dnl
dnl These are the local macros used by the ldm4 configure.in
dnl autoconf 1.6
dnl
dnl This is just like AC_HAVE_FUNCS except that the sense is reversed.
dnl
define(diversion_number, divnum)dnl
divert(-1)


AC_DEFUN([UD_PREFIX],
[
    AC_MSG_CHECKING(the installation prefix)
    case "${prefix-}" in
	NONE|'')
	    prefix=`cd $1; pwd`
	    ;;
	/*) ;;
	*)  prefix=`cd $prefix; pwd` ||
	    {
		AC_MSG_ERROR(invalid value for prefix: $prefix)
	    }
	    ;;
    esac
    AC_MSG_RESULT($prefix)
])


AC_DEFUN(UD_NO_FUNCS,
[for func in $1
do
changequote(,)dnl
trfunc=NO_`echo $func | tr '[a-z]' '[A-Z]'`
changequote([,])dnl
AC_FUNC_CHECK($func,,
AC_DEFINE_UNQUOTED($trfunc))dnl
done
])dnl
dnl
dnl This is just like AC_HAVE_HEADERS except that the sense is reversed.
dnl
AC_DEFUN(UD_NO_HEADERS,
[for hdr in $1
do
changequote(,)dnl
trhdr=NO_`echo $hdr | tr '[a-z]./' '[A-Z]__'`
changequote([,])dnl
AC_CHECK_HEADER($hdr,,
AC_DEFINE_UNQUOTED($trhdr))dnl
done
])dnl
dnl
dnl Check for existence of a file.
dnl doesnt work
dnl
AC_DEFUN(UD_FILE_CHECK,
[
AC_MSG_CHECKING(for $1)
if test -f $1; then
	AC_MSG_RESULT("found $1")
	ifelse([$2], , :, [
		echo ex "$2"
		$2
	])dnl
else
	AC_MSG_RESULT("$1 not found")
	ifelse([$3], , :, [
		echo ex2 "$3"
		$3
	])dnl
fi
])dnl
dnl
dnl
dnl
AC_DEFUN(UD_NO_FILE,
[for file in $1
do
AC_MSG_CHECKING(for $file)
changequote(,)dnl
trfile=NO_`echo $file | tr '[a-z]./' '[A-Z]__'`
changequote([,])dnl
if test -f $file ; then
	AC_MSG_RESULT(found)
else
	AC_DEFINE_UNQUOTED($trfile)
	AC_MSG_RESULT(not found)
fi
done
])dnl
dnl
dnl
dnl
AC_DEFUN(UD_FILE,
[for file in $1
do
AC_MSG_CHECKING(for $file)
changequote(,)dnl
trfile=`echo $file | tr '[a-z]./' '[A-Z]__'`
changequote([,])dnl
if test -f $file ; then    dnl NB: ultrix test(1) doesn't have -o or -c
	AC_DEFINE_UNQUOTED($trfile)
	AC_MSG_RESULT(found)
else
	AC_MSG_RESULT(not found)
fi
done
])dnl
dnl
dnl Check for POSIX signal interface. (Currently just checks for sigaction())
dnl
AC_DEFUN(UD_NO_POSIXSIGNALS,
[
AC_FUNC_CHECK(sigaction,,
AC_DEFINE(NO_POSIXSIGNALS))dnl
])dnl


dnl (derived from AC_PROGRAM_CHECK) 
dnl Check that $2 is a file or in the path,
dnl If so, set $1 to the full path and AC_SUBST
dnl
AC_DEFUN(UD_PATH_CHECK,
[
  # Extract the first word of `$2', so it can be a program name with args.
  set dummy $2; word=[$]2
  AC_MSG_CHECKING(for $word)
  IFS="${IFS= 	}"; saveifs="$IFS"; IFS="${IFS}:"
  # check if it is a full path sprc
  if test -f $word; then
      $1="$2"
  else
      for dir in $PATH; do
        test -z "$dir" && dir=.
        if test -f $dir/$word; then
          $1="$dir/$2"
          break
        fi
      done
  fi
  IFS="$saveifs"
AC_MSG_RESULT([$]$1)
AC_SUBST($1)dnl
])dnl


dnl dnl Ensure the Makefile uses the same compiler as configure
dnl dnl
dnl AC_DEFUN(UD_PROG_CC,
dnl [
dnl case `uname -s``uname -r` in
dnl     AIX*)
dnl 	cc=xlc;;
dnl     HP-UX*)
dnl 	cc=cc
dnl 	CPPFLAGS="${CPPFLAGS-} -Aa";;
dnl     IRIX*)
dnl 	cc=cc
dnl 	CPPFLAGS="${CPPFLAGS-} -D__STDC__";;
dnl     OSF1*|ULTRIX*)
dnl 	cc=cc
dnl 	CPPFLAGS="${CPPFLAGS-} -std";;
dnl     SunOS4*)
dnl 	cc=acc;;
dnl     *)  cc=cc;;
dnl esac
dnl UD_PATH_CHECK(CC, ${CC-$cc})
dnl case "$CC" in
dnl     /usr/ucb/cc*)
dnl 	AC_MSG_ERROR(Invalid C compiler: $CC.  See CC in INSTALL.)
dnl 	;;
dnl esac
dnl ])dnl

dnl
dnl Check for a Standard C compiler.  Prefer a native one over the
dnl GNU one to reduce the chance that the environment variable LIBS
dnl will have to be set to reference the GNU C runtime library.
dnl
AC_DEFUN(UD_PROG_CC,
[
    # Because we must have a C compiler, we treat an unset CC
    # the same as an empty CC.
    case "${CC}" in
	'')
	    case `uname` in
		AIX)
		    ccs='xlc c89 cc gcc'
		    ;;
		HP-UX)
		    ccs='c89 cc gcc'
		    ;;
		IRIX*)
		    # Couldn't compile using /bin/c89
		    ccs='cc gcc'
		    ;;
		SunOS)
		    case `uname -m` in
			i86pc)
			    ccs='cc gcc c89';;
			*)
			    ccs='c89 cc gcc acc';;
		    esac
		    ;;
		ULTRIX)
		    # The native ULTRIX C compiler isn't standard.
		    ccs='gcc cc'
		    ;;
		Linux)
		    ccs='gcc cc'
		    ;;
		*)  ccs='c89 cc gcc'
		    ;;
	    esac
	    for cc in $ccs; do
		AC_CHECK_PROG(CC, $cc, $cc)
		case "$CC" in
		    '') ;;
		    *)  break
			;;
		esac
	    done
	    case "${CC}" in
		'')
		    AC_MSG_ERROR("Could not find C compiler")
		    ;;
	    esac
	    ;;
	*)
	    AC_CHECKING(user-defined C compiler \"$CC\")
	    ;;
    esac
    #
    # On some systems, a discovered compiler nevertheless won't
    # work (due to licensing, for example); thus, we check the
    # compiler with a test program.
    # 
    AC_MSG_CHECKING(C compiler)
    AC_TRY_COMPILE(, ,
	AC_MSG_RESULT(works),
	AC_MSG_ERROR($CC failed to compile test program))
    AC_SUBST(CC)
    case "$CC" in
	*gcc*)
	    GCC=yes		# Expected by autoconf(1) macros
	    ;;
    esac
])


dnl
dnl Set the C-preprocessor flags as necessary.
dnl
AC_DEFUN(UD_CPPFLAGS,
[
    case `uname -s` in
	AIX)
	    AC_DEFINE(_XOPEN_SOURCE, 500)
	    AC_DEFINE(_XOPEN_SOURCE_EXTENDED, 1)
	    AC_DEFINE(_ALL_SOURCE, 1)
	    AC_DEFINE(BSD, 43)
	    ;;
	Darwin)
	    case `uname -r` in
		8*) AC_DEFINE(__DARWIN_UNIX03, 1)
		    ;;
		*)  AC_DEFINE(_DARWIN_C_SOURCE, 1)
		    AC_DEFINE(_XOPEN_SOURCE, 500)
		    AC_DEFINE(_XOPEN_SOURCE_EXTENDED, 1)
		    ;;
	    esac
	    ;;
	HP-UX)
	    AC_DEFINE(_XOPEN_SOURCE, 500)
	    AC_DEFINE(_XOPEN_SOURCE_EXTENDED, 1)
	    AC_DEFINE(_HPUX_SOURCE, 1)
	    ;;
	FreeBSD)
	    ;;
	IRIX*)
	    AC_DEFINE(_XOPEN_SOURCE, 500)
	    AC_DEFINE(_SGI_SOURCE, 1)
	    AC_DEFINE(_BSD_TYPES, 1)
	    ;;
	Linux)
	    AC_DEFINE(_XOPEN_SOURCE, 500)
	    AC_DEFINE(_BSD_SOURCE, 1)
	    ;;
	OSF1)
	    AC_DEFINE(_XOPEN_SOURCE, 500)
	    AC_DEFINE(_OSF_SOURCE, 1)
	    ;;
	SunOS)
	    AC_DEFINE(_XOPEN_SOURCE, 500)
	    AC_DEFINE(__EXTENSIONS__, 1)
	    #
            # The following is a hack to prevent SunOS 5.10's c89(1) from
            # defining c99(1) features and, consequently, failing to compile the
            # LDM package.
	    #
	    test `uname -r` = 5.10 && \
		CPPFLAGS="${CPPFLAGS+$CPPFLAGS }-U__C99FEATURES__"
	    ;;
    esac
    case "${CPPFLAGS}" in
	*NDEBUG*);;
	*) CPPFLAGS="${CPPFLAGS+$CPPFLAGS }-DNDEBUG";;
    esac
    AC_SUBST(CPPFLAGS)
])


dnl Check for mmap(2).
dnl
AC_DEFUN([UD_MMAP], [dnl
    AC_MSG_CHECKING(mmap());
    case `uname -s` in
    ULTRIX)
	AC_DEFINE(NO_MMAP)
	AC_MSG_RESULT(no)
	;;
    IRIX*)
	AC_DEFINE(_NOGROW)
	AC_MSG_RESULT(yes)
	;;
    OSF1)
	AC_DEFINE(_NOGROW)
	AC_DEFINE(TV_INT, int)
	AC_MSG_RESULT(yes)
	;;
    *)
	AC_DEFINE(_NOGROW)
	AC_MSG_RESULT(yes)
	;;
    esac
])


dnl
dnl Find the ar(1) program.
dnl
AC_DEFUN(UD_PROG_AR, [AC_PROGRAM_CHECK(AR, ar, ar, ar)])dnl


dnl
dnl Find a POSIX shell.
dnl
AC_DEFUN(UD_PROG_SH,
[dnl
    AC_MSG_CHECKING(for POSIX shell)
    case "$SH" in
    '')
	SH=/bin/sh
	case `uname -sv` in
	'AIX 4')
	    SH=/bin/bsh
	    ;;
	ULTRIX*)
	    SH=/usr/local/gnu/bin/bash
	    ;;
	esac
	;;
    esac
    AC_MSG_RESULT($SH)
    AC_SUBST(SH)
])dnl


dnl
dnl Find the perl(1) program.
dnl
AC_DEFUN(UD_PROG_PERL,
[dnl
    AC_PATH_PROGS(PERL, perl /usr/local/bin/perl)
    case "$PERL" in
	'') echo 1>&2 "Couldn't find perl(1) utility"
	    echo 1>&2 "Set environment variable PERL and rerun configure"
	    echo 1>&2 "    E.g. setenv PERL /opt/bin/perl"
	    AC_MSG_ERROR()
	    ;;
    esac
])dnl


dnl Set the fully-qualified domain name.
dnl
AC_DEFUN([UD_FQDN], [dnl
AC_MSG_CHECKING(fully-qualified domain name)
if test -z "$FQDN"; then
    name=`hostname`
    case "$name" in
    "") name=`uname -n`
	;;
    esac
    FQDN=
    case "$name" in
    *.*)FQDN=$name
	;;
    *)	domain=`domainname`
	case "$domain" in
	noname|"")
	    entry=`grep $name /etc/hosts | head -1`
	    if test -n "$entry"; then
		FQDN=`echo "$entry" | \
		    sed "s/.*\($name\.[[a-zA-Z0-9_.]]*\).*/\1/"`
	    fi
	    ;;
	*)  FQDN=${name}.${domain}
	    ;;
	esac
	;;
    esac
    case "$FQDN" in
    "") echo 1>&2 "Couldn't obtain fully-qualified domain-name"
	echo 1>&2 "Set environment variable FQDN and rerun configure"
	echo 1>&2 "    E.g. setenv FQDN foo.unidata.ucar.edu"
	exit 1
	;;
    esac
    AC_MSG_RESULT($FQDN)
    AC_SUBST(FQDN)dnl
fi
])


dnl Set the domain name.
dnl
AC_DEFUN([UD_DOMAINNAME], [dnl
AC_MSG_CHECKING(domain name)
if test -z "$DOMAINNAME"; then
    name=`hostname`
    case "$name" in
    *.*)
	changequote(,)dnl
	DOMAINNAME=`echo $name | sed 's/[^.]*\.//'`
	changequote([,])dnl
	;;
    *)	domain=`domainname 2>/dev/null`
	case "$domain" in
	noname|"")
	    entry=`grep $name /etc/hosts | head -1`
	    if test -n "$entry"; then
		DOMAINNAME=`echo "$entry" | \
		    sed "s/.*$name\.\([[a-zA-Z0-9_.]]*\).*/\1/"`
	    fi
	    ;;
	*)  DOMAINNAME=$domain
	    ;;
	esac
	;;
    esac
    case "$DOMAINNAME" in
    "") echo 1>&2 "Couldn't obtain domain-name"
	echo 1>&2 "Set environment variable DOMAINNAME and rerun configure"
	echo 1>&2 "    E.g. setenv DOMAINNAME unidata.ucar.edu"
	exit 1
	;;
    esac
    AC_MSG_RESULT($DOMAINNAME)
    AC_SUBST(DOMAINNAME)dnl
fi
])


dnl Set RPC and socket references.
dnl
AC_DEFUN([UD_NETWORKING],
[dnl
    AC_MSG_CHECKING(networking references)
    case `uname -sr` in
	"SunOS 5"*)
	    case "$LIBS" in
		*-lnsl*)
		    libs=
		    ;;
		*)  libs="-lnsl"
		    ;;
	    esac
	    case "$LIBS" in
		*-lsocket*)
		    ;;
		*)
		    libs="${libs:+$libs }-lsocket"
		    ;;
	    esac
	    AC_DEFINE(PORTMAP)
	    ;;
	"SunOS 4"*)
	    libs=
	    ;;
	BSD*)
	    libs="-lrpc"
	    ;;
	HP-UX\ ?.10.2*)
	    libs="-lnsl_s -lPW"
	    AC_DEFINE(PORTMAP)
	    ;;
	HP-UX\ ?.11*)
	    AC_DEFINE(PORTMAP)
	    ;;
	*)  libs=
	    ;;
    esac
    AC_MSG_RESULT($libs)
    LIBS="${LIBS:+$LIBS }$libs"
    unset libs
])


dnl dnl Set Berkeley socket references.
dnl dnl
dnl AC_DEFUN([UD_SOCKET],
dnl [dnl
dnl     AC_MSG_CHECKING(Berkeley socket references)
dnl     case `uname -s` in
dnl 	SunOS)
dnl 	    case `uname -r` in
dnl 		5*)	case "$LIBS" in
dnl 			*-lsocket*)
dnl 			    AC_MSG_RESULT()
dnl 			    ;;
dnl 			*)  LIBS="$LIBS -lsocket"
dnl 			    AC_MSG_RESULT(-lsocket)
dnl 			    ;;
dnl 		    esac
dnl 		    ;;
dnl 		*)  AC_MSG_RESULT()
dnl 		    ;;
dnl 	    esac
dnl 	    ;;
dnl 	*)  AC_MSG_RESULT()
dnl 	    ;;
dnl     esac
dnl ])


dnl dnl Set RPC references.
dnl dnl
dnl AC_DEFUN([UD_RPC],
dnl [dnl
dnl     AC_MSG_CHECKING(RPC references)
dnl     case `uname -s` in
dnl 	SunOS)
dnl 	    case `uname -r` in
dnl 		5*)
dnl 		    case "$LIBS" in
dnl 			*-lrpcsoc*)
dnl 			    AC_MSG_RESULT()
dnl 			    ;;
dnl 			*)  LIBS="$LIBS -R/usr/ucblib -L/usr/ucblib -lrpcsoc -lnsl"
dnl 			    AC_MSG_RESULT(-R/usr/ucblib -L/usr/ucblib -lrpcsoc -lnsl)
dnl 			    ;;
dnl 		    esac
dnl 		    AC_DEFINE(PORTMAP)
dnl 		    ;;
dnl 		*)  AC_MSG_RESULT()
dnl 		    ;;
dnl 	    esac
dnl 	    ;;
dnl 	BSD*)
dnl 	    LIBS="$LIBS -lrpc"
dnl 	    AC_MSG_RESULT(-lrpc)
dnl 	    ;;
dnl 	*)  AC_MSG_RESULT()
dnl 	    ;;
dnl     esac
dnl ])


dnl Determine whether or not to allow munging of argv.
dnl This is known to not work under HP-UX A.09.03, IRIX 4.0.5F,
dnl and SunOS 5.3.  -- Steve Emmerson 5/23/94
dnl
AC_DEFUN([UD_PROCTITLE], [dnl
    case `uname -s` in
    AIX|OSF1|ULTRIX)
	AC_DEFINE(UD_SETPROCTITLE)
	;;
    SunOS)
	case `uname -r` in
	4*) AC_DEFINE(UD_SETPROCTITLE)
	    ;;
	esac
	;;
    esac
])


dnl Set ulog parameters
dnl
AC_DEFUN([UD_ULOG], [dnl
    AC_MSG_CHECKING(ulog defines)
    case `uname -sr` in
	OSF1*|sn1036*|Linux*|Darwin*)
	    AC_DEFINE(NO_REPLACE_SYSLOG)
	    AC_MSG_RESULT(-DNO_REPLACE_SYSLOG)
	    ;;
	*)
	    AC_TRY_COMPILE(
		[#include <syslog.h>],
		[int i = syslog(0,0);],
		AC_DEFINE(SYSLOG_RETURNS_INT)
		AC_MSG_RESULT(-DSYSLOG_RETURNS_INT))
	    ;;
    esac

    unset ULOGNAME
    for usock in /dev/log /var/run/syslog; do
	if test -w $usock; then
	    ULOGNAME=$usock
	    AC_DEFINE_UNQUOTED(ULOGNAME, "$ULOGNAME")
	    AC_MSG_RESULT(-DULOGNAME=$ULOGNAME)
	    if ls -lL $usock | grep -q '^s'; then
		AC_DEFINE(LOGNAME_ISSOCK)
		AC_MSG_RESULT(-DLOGNAME_ISSOCK)
	    fi
	    break
	fi
    done

    if test -z "$ULOGNAME"; then
	if test -w /dev/conslog; then
	    AC_DEFINE(_DEV_CONSLOG)
	    AC_MSG_RESULT(-D_DEV_CONSLOG)
	else
	    AC_MSG_RESULT(no log device)
	fi
    fi
])


dnl Set syslog pid filename for hupsyslog
dnl
AC_DEFUN([UD_SYSLOG_PIDFILE], [dnl
    AC_MSG_CHECKING([system logging daemon PID file])
    for path in /etc/syslog.pid /var/run/syslog.pid /var/run/syslogd.pid \
	    /var/run/rsyslogd.pid; do
	if test -f $path; then
	    AC_DEFINE_UNQUOTED(SYSLOG_PIDFILE, "$path")
	    AC_MSG_RESULT($path)
	    break;
	fi
    done
])dnl

dnl
dnl See what compatability libs are needed.
dnl
AC_DEFUN(UD_LDMCOMPATLIBS,
[
    AC_BEFORE([UD_RPCSOC])
    dnl Check if anything needed from ansi or rpc subdirs
    AC_FUNC_CHECK(atexit,COMPATLIBS="",[
	    AC_DEFINE(NO_ATEXIT)
	    COMPATLIBS="ansi"
	    UD_NO_HEADERS(tzfile.h)
	    ])dnl
    AC_FUNC_CHECK(clnttcp_create,COMPATLIBS="",[
	    COMPATLIBS="$COMPATLIBS rpc"
	    CPPFLAGS="${CPPFLAGS-} -I$SRCDIR"
	    ])dnl
    AC_SUBST(COMPATLIBS)
])dnl


dnl Check if tirpc and behave appropriately. (deprecate)
dnl
AC_DEFUN(UD_LDMTIRPC,
[
AC_HEADER_CHECK(rpc/nettype.h, AC_DEFINE(TIRPC))dnl
])dnl
dnl
dnl Check if need to define PORTMAP
dnl
AC_DEFUN(UD_LDMPORTMAP,
[
AC_HEADER_EGREP(clntudp_create, rpc/clnt.h,,AC_DEFINE(PORTMAP))dnl
])dnl
dnl
dnl  Run configure in another directory
dnl  Useful when merging packages.
dnl
AC_DEFUN(UD_SUBCONFIG,
[
  if test -d $1; then
    if test -f $1/config.status; then
	    echo " * running config.status in $1"
	    (cd $1 ; ${CONFIG_SHELL-/bin/sh} ./config.status)
	else
	    echo "running configure in $1"
	    (cd $1 ; ${CONFIG_SHELL-/bin/sh} ./configure --prefix=${prefix})
    fi
  else
	echo "$1 not found"
  fi
])dnl
dnl
dnl  Set up LDMHOME
dnl
AC_DEFUN(UD_LDMHOME,
[
	AC_MSG_CHECKING(LDMHOME)
	case "$LDMHOME" in
	    '')	LDMHOME=`awk -F: '{if($''1~/ldm/) {print $''6;exit;}}' /etc/passwd`
		case "$LDMHOME" in
		    '') AC_MSG_ERROR(\$LDMHOME not set.  Set and re-execute configure script.)
			;;
		esac
		;;
	esac
	AC_SUBST(LDMHOME)
	AC_DEFINE_UNQUOTED(LDMHOME, "$LDMHOME")
	AC_MSG_RESULT($LDMHOME)
])dnl
dnl
dnl  Set up SRCDIR
dnl
AC_DEFUN(UD_SRCDIR,
[
	AC_MSG_CHECKING(LDM source directory)
	SRCDIR=${SRCDIR-`pwd`}
	AC_SUBST(SRCDIR)
	AC_MSG_RESULT($SRCDIR)
])dnl
dnl
dnl
dnl
AC_DEFUN(UD_TEST_CPP,
[AC_REQUIRE([UD_PROG_CPP])cat > conftest.c <<EOF
[$1]
EOF
dnl Some shells (Coherent) do redirections in the wrong order, so need
dnl the parens.
err=`eval "($CPP conftest.c >/dev/null) 2>&1"`
if test -z "$err"; then
  ifelse([$2], , :, [rm -rf conftest*
  $2
])
ifelse([$3], , , [else
  rm -rf conftest*
  $3
])dnl
fi
rm -f conftest*])dnl


AC_DEFUN(UD_PROG_CPP,
[dnl
    AC_REQUIRE([AC_PROG_CC]) dnl
    case `uname -sr` in
	'HP-UX B.11'*)
	    cpp="${CC-c89} -E"	dnl "-w" disables warnings
	    ;;
	*)
	    AC_REQUIRE([AC_PROG_CPP]) dnl
	    AC_MSG_CHECKING(the C preprocessor)
	    AC_TRY_CPP([#include <stdlib.h>],
		AC_MSG_RESULT(works),
		AC_MSG_ERROR([[$[]0: C preprocessor, \`$CPP', doesn't work]]))
	    ;;
    esac
])


dnl AC_DEFUN(UD_PROG_CPP,
dnl [dnl
dnl AC_MSG_CHECKING(how to run the C preprocessor)
dnl  AC_PROVIDE(AC_PROG_CPP)
dnl if test -z "$CPP"; then
dnl   # This must be in double quotes, not single quotes, because CPP may get
dnl   # substituted into the Makefile and ``${CC-cc}'' will simply confuse
dnl   # make.  It must be expanded now.
dnl   CPP="${CC-cc} -E"
dnl dnl On the NeXT, cc -E appears to run the code through the compiler's parser,
dnl dnl not just through cpp.
dnl   UD_TEST_CPP([#include <stdio.h>
dnl Syntax Error], , CPP=/lib/cpp)
dnl fi
dnl test ".${verbose}" != "." && echo "	setting CPP to $CPP"
dnl AC_MSG_RESULT($CPP)
dnl AC_SUBST(CPP)dnl
dnl ])dnl


AC_DEFUN(UD_AIX,
[AC_MSG_CHECKING(for AIX)
AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_PROGRAM_EGREP(yes,
[#ifdef _AIX
  yes
#endif
],[
  AC_MSG_RESULT(yes)
  AC_DEFINE(_XOPEN_SOURCE_EXTENDED)
  AC_DEFINE(_ALL_SOURCE)
  AC_DEFINE(BSD,43)
  ],
  AC_MSG_RESULT(no)
  )dnl
])dnl


AC_DEFUN(UD_HPUX,
[AC_MSG_CHECKING(for HP-UX)
AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_PROGRAM_EGREP(yes,
[#ifdef __hpux
  yes
#endif
],[
  AC_MSG_RESULT(yes)
dnl
dnl	Under HP-UX B.11.00, <rpc/rpc.h> includes <rpc/auth.h>, which includes
dnl	<sys/user.h>, which uses "kt_t", which isn't defined anywhere; so omit
dnl	<sys/user.h>
  AC_DEFINE(_SYS_USER_INCLUDED)
  case "$CPP" in
    /bin/c89*|c89*|/bin/cc*|cc*|/lib/cpp*|cpp*)
dnl 	HP-UX's C compiler emits a warning about <stdlib.h> when
dnl	HPUX_SOURCE is defined.  This fouls tests that check output
dnl	on standard error.  Consequently, ensure that warning messages
dnl	are suppressed.
      case "$CFLAGS" in
	*-w*);;
#	*) CFLAGS="${CFLAGS:+${CFLAGS} }-w" ;;	dnl not needed anymore
      esac;;
  esac
  case "$CFLAGS" in
dnl	In 64-bit mode, the "xnet" networking library must be used.
    *+DA2.0W*) LIBS="${LIBS}${LIBS+ }-lxnet";;
  esac
  ],
  AC_MSG_RESULT(no)
  )dnl
])dnl


AC_DEFUN(UD_MAPRGNS,
[AC_MSG_CHECKING(whether memory-mapping by region is appropriate)
AC_BEFORE([$0], [AC_COMPILE_CHECK])dnl
AC_BEFORE([$0], [AC_TEST_PROGRAM])dnl
AC_BEFORE([$0], [AC_HEADER_EGREP])dnl
case `uname` in
    AIX|Linux)
	AC_MSG_RESULT(yes)
	AC_DEFINE(_MAPRGNS)dnl
	;;
    *)	AC_MSG_RESULT(no)
	;;
esac
])dnl

AC_DEFUN(UD_SIG_ATOMIC_T,
[
    AC_MSG_CHECKING(for sig_atomic_t in signal.h)
    AC_EGREP_HEADER(sig_atomic_t, signal.h,
		    AC_MSG_RESULT(defined),
		    [
			AC_MSG_RESULT(not defined)
			AC_DEFINE(sig_atomic_t, int)
		    ]
		   )
])


dnl AC_DEFUN(UD_RPCSOC, [dnl
dnl LIBS_save="${LIBS}"
dnl LIBS="-R/usr/ucblib -L/usr/ucblib -lrpcsoc ${LIBS}"
dnl have_lib=""
dnl AC_COMPILE_CHECK([-lrpcsoc], , [main();], [have_lib="1"])dnl
dnl if test -z "${have_lib}"; then
dnl    LIBS="${LIBS_save}"
dnl fi
dnl ])dnl


dnl
dnl Change defaults to be compatible with Peter Neilley's "weather" program
dnl
AC_DEFUN(UD_NEILLEY_COMPAT, [dnl
   AC_SUBST(GDBMLIB)
   if test -z "$GDBMLIB"; then
       AC_CHECK_LIB(gdbm, gdbm_open, GDBMLIB=-lgdbm)
   fi
   if test -n "$GDBMLIB"; then
       AC_CHECK_HEADER(gdbm.h, AC_DEFINE(USE_GDBM))
   fi
   AC_DEFINE(DB_XPROD, 0)
   AC_DEFINE(DB_CONCAT)
])dnl
dnl
dnl Turn off DB support if not available
dnl
AC_DEFUN(UD_DB, [dnl
   if test -z "$GDBMLIB"; then
       AC_MSG_WARN("GDBMLIB not set")
       AC_CHECK_FUNC(dbm_open,
          ,
          AC_DEFINE(NO_DB)
          AC_MSG_WARN("pqact DBFILE action disabled")
       )
   fi
])dnl


dnl Check for yacc(1) utility.
dnl
AC_DEFUN([UD_PROG_YACC], [dnl
AC_PROGRAMS_CHECK(YACC, yacc bison)
case "${YACC-}" in
    '') AC_MSG_ERROR(
[yacc utility not found.  Modify PATH and/or set environment variable YACC.  If using GNU bison utility, then use \"-y\" option.  Then re-execute configure script.])
	;;
    *bison*)
	case "${YACC}" in
	    *-y*) ;;
	    *)  YACC="${YACC} -y" ;;
	esac
	;;
esac
AC_SUBST(YACC)
])


dnl Check for yacc(1) library.
dnl
AC_DEFUN([UD_LIB_YACC],
[
    case `uname` in
	Linux)
	    UD_DEFAULT(LD_YACC, )
	    ;;
	*)  UD_CHECK_LIB(LD_YACC, yyerror(""), , y, yacc, -ly)
	    ;;
    esac
])


dnl Set the value of a variable.  Use the environment if possible; otherwise
dnl set it to a default value.  Call the substitute routine.
dnl
AC_DEFUN([UD_DEFAULT], [dnl
    $1=${$1-"$2"}
    AC_SUBST([$1])
])


dnl Check for a library that contains a function.
dnl
dnl NB: Always checks default library and library directories first.  This
dnl obviates the need for a `-L...' reference, which can cause problems
dnl (e.g. a `-L/usr/lib -lsocket' reference under SunOS 5.2 can cause the
dnl wrong `-lm' to be loaded).
dnl
dnl This rule was changed (for some reason) to return `-lc' if the 
dnl function was in the default library.  This caused problems on
dnl an DecStation ULTRIX system when f77(1) was used to link a FORTRAN
dnl program: the C and FORTRAN libraries had duplicate definitions for
dnl some functions.  Consequently, we return to the practice of not
dnl deciding on `-lc'.
dnl
dnl UC_CHECK_LIB(varname, func, dir ..., lib ..., libname, example)
dnl
AC_DEFUN([UD_CHECK_LIB],
[dnl
AC_MSG_CHECKING(for $5 library)
    case "${$1+set}" in
    set)
	AC_MSG_RESULT($$1)
	;;
    *) AC_MSG_RESULT()
	LIBS_save=$LIBS
	found=no
	AC_MSG_CHECKING(for $2 in default library(s))
	AC_TRY_LINK(, $2;, 
		    [
			AC_MSG_RESULT(yes)
			$1=
			found=yes
		    ],
		    [
			AC_MSG_RESULT(no)
			for dir in '' $3; do
			    for lib in $4; do
				UD_LINK_REF(LIBS, $dir, $lib)
				AC_MSG_CHECKING(for $2 in $LIBS)
				AC_TRY_LINK(, $2;, 
					    [
						AC_MSG_RESULT(yes)
						$1=$LIBS
						found=yes
						break 2
					    ])
				AC_MSG_RESULT(no)
			    done
			done
		    ])
	LIBS=$LIBS_save
	case $found in
	    no)	
		AC_MSG_ERROR(
[$5 library not found.  Adjust library search-path (e.g., LD_LIBRARY_PATH) and/or set environment variable $1 (e.g., to \"$6\").  Then re-execute configure script.])
		;;
	esac
	;;
    esac
    AC_SUBST($1) dnl
])


dnl Form a library reference for the linker/loader
dnl
dnl On a SunOS 5 system, a `-R<dir>' is added in addition to a `-L<dir>'
dnl in order to make the utility independent of LD_LIBRARY_PATH (is this
dnl a good idea?) and to ensure that it'll run regardless of who
dnl executes it.
dnl
dnl UC_LINK_REF(varname, libdir, libname)
dnl
dnl Example: UC_LINK_REF(UC_LD_MATH, /upc/netcdf/lib, netcdf)
dnl
AC_DEFUN([UD_LINK_REF], [dnl
    case `uname -rs` in
	unicos*)
	    case "$2" in
		'') $1="-l $3";;
		*)  $1="-L $2 -l $3";;
	    esac
	    ;;
	SunOS\ 5*)
	    case "$2" in
		'') $1="-l$3";;
		*)  $1="-R$2 -L$2 -l$3";;
	    esac
	    ;;
	*)
	    case "$2" in
		'') $1="-l$3";;
		*)  $1="-L$2 -l$3";;
	    esac
	    ;;
    esac
])

dnl @synopsis TYPE_SOCKLEN_T
dnl
dnl Check whether sys/socket.h defines type socklen_t. Please note
dnl that some systems require sys/types.h to be included before
dnl sys/socket.h can be compiled.
dnl
dnl @version $Id: aclocal.m4,v 1.27.2.1.2.1.2.1.2.16 2009/05/21 20:30:36 steve Exp $
dnl @author Lars Brinkhoff <lars@nocrew.org>
dnl
AC_DEFUN([TYPE_SOCKLEN_T],
[AC_CACHE_CHECK([for socklen_t], ac_cv_type_socklen_t,
[
  AC_TRY_COMPILE(
  [#include <sys/types.h>
   #include <sys/socket.h>],
  [socklen_t len = 42; return 0;],
  ac_cv_type_socklen_t=yes,
  ac_cv_type_socklen_t=no)
])
  if test $ac_cv_type_socklen_t != yes; then
    AC_DEFINE(socklen_t, int)
  fi
])


dnl Setup for making a manual-page database.
dnl
AC_DEFUN(UD_MAKEWHATIS,
[
    #
    # NB: We always want to define WHATIS to prevent the
    # $(MANDIR)/$(WHATIS) make(1) target from being just $(MANDIR)/ and
    # conflicting with the (directory creation) target with the same name.
    #
    WHATIS=whatis
    case `uname -sr` in
	BSD/OS*|FreeBSD*|Darwin*)
	    # Can't generate a user-database -- only /usr/share/man/whatis.db.
	    MAKEWHATIS_CMD=
	    ;;
	'IRIX64 6.5'|'IRIX 6.5')
	    MAKEWHATIS_CMD='/usr/lib/makewhatis -M $(MANDIR) $(MANDIR)/whatis'
	    ;;
	'IRIX 6'*)
	    # Can't generate a user-database.
	    MAKEWHATIS_CMD=
	    ;;
	HP-UX*)
	    # Can't generate a user-database -- only /usr/lib/whatis.
	    MAKEWHATIS_CMD=
	    ;;
	'Linux '*)
	    # /usr/sbin/makewhatis doesn't work
	    MAKEWHATIS_CMD=
	    ;;
	ULTRIX*)
	    # Can't generate a user-database -- only /usr/lib/whatis.
	    MAKEWHATIS_CMD=
	    ;;
	*)
	    if test -r /usr/man/windex; then
		WHATIS=windex
	    fi
	    AC_CHECK_PROGS(prog, catman makewhatis /usr/lib/makewhatis)
	    case "$prog" in
		*catman*)
		    MAKEWHATIS_CMD=$prog' -w -M $(MANDIR)'
		    ;;
		*makewhatis*)
		    MAKEWHATIS_CMD=$prog' $(MANDIR)'
		    ;;
	    esac
	    ;;
    esac
    AC_SUBST(WHATIS)
    AC_SUBST(MAKEWHATIS_CMD)
    AC_MSG_CHECKING(for manual-page index command)
    AC_MSG_RESULT($MAKEWHATIS_CMD)
])

divert(diversion_number)dnl
