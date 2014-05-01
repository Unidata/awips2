/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ulog.h,v 1.84 2002/12/02 22:38:49 steve Exp $ */

#ifndef _ULOG_H_
#define _ULOG_H_
#include <syslog.h>
#include <stdarg.h>

/*
 * options to openulog() which are not options
 * to openlog()
 * Make sure these dont collide with ones in <syslog.h>
 * or the 4.2 compatibility section below
 */
#define LOG_NOTIME 0x200u		/* don't put on the timestamp */
#define LOG_LOCALTIME 0x100u  /* use localtime. default is gmt */

#define LOG_IDENT 0x400u		/* don't add the facility identifier */

/*
 * This set of #defines allows this to work even with a
 * 4.2 BSD style syslog.h, like on Ultrix 4.x
 */
#ifndef LOG_NFACILITIES
/* means this system doesn't have 4.3 BSD syslog */
#define LOG_NFACILITIES 0
#endif /* !LOG_NFACILITIES */

#ifndef LOG_MASK
#define	LOG_MASK(pri)	(1u << (unsigned)(pri))	/* mask for one priority */
#endif 

#ifndef LOG_PRIMASK
#define LOG_PRIMASK (LOG_MASK(LOG_EMERG) | LOG_MASK(LOG_ALERT) \
	| LOG_MASK(LOG_CRIT) | LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) \
	| LOG_MASK(LOG_NOTICE) | LOG_MASK(LOG_INFO) | LOG_MASK(LOG_DEBUG))
#endif

#ifndef LOG_FACMASK
#define LOG_FACMASK (~LOG_PRIMASK)
#endif

#ifndef LOG_USER
#define LOG_USER 0u
#endif

#ifndef LOG_LOCAL0
#define LOG_LOCAL0 0u
#endif

#ifndef LOG_CONS
#define LOG_CONS 0x20u
#endif

#ifndef LOG_NOWAIT
#define LOG_NOWAIT 0x40u
#endif

#ifndef LOG_UPTO
#define	LOG_UPTO(pri)	((1u << ((pri)+1u)) - 1u)	/* all priorities through pri */
#endif 
/* End 4.2 compatiblity section */


/*
 * The "facility" used by ldm applications.
 */
#ifndef LOG_LDM
#define LOG_LDM LOG_LOCAL0
#endif


/*
 * The 'ident' arg to openulog() and setulogident will be
 * copied into static space, truncatated to this length.
 */
#define LOG_IDENT_LEN 32

#ifdef __cplusplus
extern "C" {
#endif

int closeulog(void);
int openulog(
	const char *ident ,
	unsigned int options ,
	unsigned int facility , 
	const char *logfilename);
unsigned ulog_get_options();
void ulog_set_options(unsigned mask, unsigned values);
void setulogident(const char *ident);
void ulog(unsigned int pri, const char *fmt, ...);
int vulog(unsigned int pri, const char *fmt, va_list args);
unsigned int setulogmask(unsigned int pmask);
int toggleulogpri(unsigned int pri);
void rollulogpri(void);
unsigned int getulogmask(void);
int ulogIsVerbose(void);
int ulogIsDebug(void);
void serror(const char *fmt, ...);
void uerror(const char *fmt, ...);
void uwarn(const char *fmt, ...);
void unotice(const char *fmt, ...);
void uinfo(const char *fmt, ...);
void udebug(const char *fmt, ...);
const char *ubasename(const char *av0);
void _uassert( const char *ex, const char *file, int line);

#ifdef NO_STRERROR
extern char *
strerror(int);
#endif

#ifdef __cplusplus
}
#endif

/*
 * When we are using ulog, we want assert() messages to go via
 * the logger.
 */
#if defined(assert) && !defined(NDEBUG)
#undef assert
#if defined(__STDC__) || defined(__cplusplus)
#       define assert(EX) \
            (((int) (EX)) ? (void)0 :  _uassert(#EX, __FILE__, __LINE__))
#else
#       define assert(EX) \
            (((int) (EX)) ? (void)0 :  _uassert("EX", __FILE__, __LINE__))
#endif
#endif


#endif /* !_ULOG_H_ */
