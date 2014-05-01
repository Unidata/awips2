/* config/ldmconfig.h.  Generated from ldmconfig.h.in by configure.  */
#ifndef LDM_CONFIG_H
#define LDM_CONFIG_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 500
#endif
#ifndef _XOPEN_SOURCE_EXTENDED
/* #undef _XOPEN_SOURCE_EXTENDED */
#endif

/* AIX: */
/* #undef _ALL_SOURCE */
/* #undef BSD */
/* Darwin: */
/* #undef __DARWIN_UNIX03 */
/* #undef _DARWIN_C_SOURCE */
/* HP-UX: */
/* #undef _HPUX_SOURCE */
/* IRIX: */
/* #undef _SGI_SOURCE */
/* #undef _BSD_TYPES */
/* FreeBSD: */
/* Linux: */
#define _BSD_SOURCE 1
/* OSF1: */
/* #undef _OSF_SOURCE */
/* SunOS: */
/* #undef __EXTENSIONS__ */

/* No database support in pqact/filel.c */
#define NO_DB 1
/* Concatenate products in pqact/filel.c. Mutually exclusive with DB_XPROD */
#define DB_CONCAT 1
/* XDR DB products in pqact/filel.c. Mutually exclusive with DB_CONCAT */
#define DB_XPROD 0
/* Use gdbm interface in pqact/filel.c; default is ndbm */
/* #undef USE_GDBM */
#define HAVE_ST_BLKSIZE 1
#define HAVE_TIMEGM 1
#define ULOGNAME "/dev/log"
#define LOGNAME_ISSOCK 1
/* #undef NO_ATEXIT */
/* #undef NO_FSYNC */
/* #undef NO_FTRUNCATE */
/* #undef NO_MEMCMP */
/* #undef NO_MEMMOVE */
/* #undef NO_MMAP */
/* #undef NO_POSIXSIGNALS */
/* #undef NO_RENAME */
#define NO_REPLACE_SYSLOG 1
/* #undef NO_SETENV */
/* #undef NO_SETEUID */
/* #undef NO_STRDUP */
/* #undef NO_STRERROR */
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SYSLOG_PIDFILE "/var/run/syslogd.pid"
/* #undef SYSLOG_RETURNS_INT */
/* #undef TV_INT */
/* #undef UD_SETPROCTITLE */
/* #undef _DEV_CONSLOG */
#define _MAPRGNS 1
#define _NOGROW 1
/* #undef _SYS_USER_INCLUDED */
/* #undef const */
/* #undef off_t */
/* #undef ptrdiff_t */
/* #undef sig_atomic_t */
/* #undef ssize_t */
/* #undef socklen_t */
#define LDMHOME "/usr/local/ldm"
#define LOG_LDM LOG_LOCAL0
#define LDM_PORT 388
#define LDM_PROG 300029

#define RESOLVER_TIME_THRESHOLD	10

/*
 * The following header-files are included here because on FreeBSD 
 * 4.10-RELEASE-p2, at least, their order is important.
 */
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#endif
