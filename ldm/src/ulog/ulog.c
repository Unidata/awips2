/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ulog.c,v 1.99.16.1.2.9 2008/03/17 16:16:59 steve Exp $ */

/* 
 * Utility Functions to implement consistant logging mechanisms.
 * These provide interfaces to the syslogd(8) system if available.
 * 'syslog' code based on "@(#)syslog.c 5.20 (Berkeley) 1/19/89"
 * Copyright (c) 1983, 1988 Regents of the University of California.
 */

/*
 * If a file /dev/conslog exists, configure defines _DEV_CONSLOG.
 * We use this macro to handle SVR4 streams based logging.
 */

#include <ldmconfig.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <fcntl.h>
#if defined(__STDC__) || defined(_AIX)
#    include <stdarg.h>
#define STDC_ARGS 1
#else
#    include <varargs.h>
#endif
#include <errno.h>
#include <time.h>
#include <signal.h>
#ifndef NO_WAITPID
#    include <sys/wait.h>
#endif
#include "ulog.h"

#if defined(__CENTERLINE__) && defined(sun) && defined(__STDC__)
    /* Workaround for ObjectCenter 1.1 stdargs problem, Sun OS 4.1.x */
#    undef __builtin_va_arg_incr
#    define __builtin_va_arg_incr(list) (((list) += 1) -1)
#    undef va_start
#    define va_start(list, arg) list = (char *)&arg + sizeof(arg)
#endif

#if defined(NO_STRERROR) && !defined(lint) /* recividist unie */
    char *
    strerror(errnum)
    int errnum;
    {
        extern int sys_nerr;
        extern char *sys_errlist[];

        if(errnum < 0 || errnum >= sys_nerr) return NULL;
        /* else */
        return sys_errlist[errnum];
    }
#elif defined(sun) /* Sun OS 4.x */
    extern char *strerror(/* int */);
#endif /* NO_STRERROR */


#if (LOG_NFACILITIES > 0)
    /*
     * BSD 4.3 style, the logger attempts to open a connection to
     * a named fifo or an AF_UNIX socket called ULOGNAME. 
     */
#    ifdef _DEV_CONSLOG
#       define ULOGNAME        "/dev/conslog"
#    elif !defined(ULOGNAME)
#       define ULOGNAME        "/dev/log"      /* default */
#    endif
#endif

/*
 * BSD 4.2 style, the logger trys the AF_INET well known service
 * "syslog" at host LOGHOST. 
 */
#define LOGHOST "127.0.0.1"


/*
 * If all else fails, it attempts to write directly to CONSOLE.
 */ 
#define CONSOLE "/dev/console"


static const char*      logFilename = NULL;     /* NULL => use syslogd */
static int              logFd = -1;             /* fd for log */
static unsigned int     logOptions = 0;         /* set by openulog() */
static char             logIdent[LOG_IDENT_LEN+1] = "ulog";
                                                /* Default utility tag */
static unsigned int     logFacility = LOG_USER; /* default facility code */
static unsigned int     logMask =  LOG_UPTO(LOG_DEBUG);
                                                /* default mask */
#define MAX_LEN 2047
static size_t           maxLen = MAX_LEN;       /* maximum message length in
                                                 * bytes excluding terminating
                                                 * NUL */


/*
 * Close the log connection
 */
int
closeulog(void)
{
        int ret = 0;

        if(logFilename != NULL && *logFilename == '-') /* special case for stderr */
                logFilename = NULL;
        else if (logFd >= 0)
        {
                ret = close(logFd);
        }
        logFd = -1;

        return ret;
}


/*
 * Set the ident field.
 * Use this instead of closeulog() followed by openulog()
 * to change the ident in a child proc.
 */
void
setulogident(const char *ident)
{
        if (ident != NULL)
                strncpy(logIdent, ident, LOG_IDENT_LEN);
}


/* 
 * Like Berkeley 'openlog(3)' - initialize syslog system.
 * But: 
 *  Always attempts to open the log connection if there is none,
 *  even if the LOG_NDELAY option bit is cleared.
 *  Returns the descriptor (socket) of the logger or -1 on system call error.
 *  If ULOG_FACILITY_OVERRIDE environment variable is set, overrides facility.
 * 
 * N.B. multiple calls without an intervening 'closeulog()' simply reinitialize
 * ident, options, and facility.
 * The data referred to by 'ident' and 'filename' should have process lifetime.
 */
int
openulog(
        const char *ident,
        unsigned int options,
        unsigned int facility,
        const char *logfilename)
{
        setulogident(ident);

        logOptions = options | LOG_IDENT;

        /*
         * Check the environment for overriding facility.
         */
        {
                unsigned int facility_level = 0;
                const char *ufo = getenv("ULOG_FACILITY_OVERRIDE");
                if(ufo != NULL) {
                  facility_level = atoi(ufo);
                  switch (facility_level) {
                  case 0:
                    facility = LOG_LOCAL0;
                    break;
                  case 1:
                    facility = LOG_LOCAL1;
                    break;
                  case 2:
                    facility = LOG_LOCAL2;
                    break;
                  case 3:
                    facility = LOG_LOCAL3;
                    break;
                  case 4:
                    facility = LOG_LOCAL4;
                    break;
                  case 5:
                    facility = LOG_LOCAL5;
                    break;
                  case 6:
                    facility = LOG_LOCAL6;
                    break;
                  case 7:
                    facility = LOG_LOCAL7;
                    break;
                  default:
                    break;
                  }
                }
        }

        if (facility != 0 && (facility &~ LOG_FACMASK) == 0)
                logFacility = facility;

        if (logFd != -1)
                (void) closeulog();
        
        if(logfilename != NULL && *logfilename != 0)
        {
                        /* "-" means use stderr */
                if(logfilename[0] == '-' && logfilename[1] == 0)
                {
                        logFd = fileno(stderr);
                        logFilename = "-";
                }
                else
                {
                        logFd = open(logfilename, (O_WRONLY|O_CREAT|O_APPEND), 0664);
                        if(logFd != -1)
                                logFilename = logfilename; /* N.B. Not a copy */
                }

                /* if the open fail, fall thru and use syslogd */
        }

        if (logFd == -1)
        {
                /*
                 * On a given OS, the ULOGNAME is always
                 * a fifo, a STREAMS device,  or a unix domain socket.
                 * So, figure it out ahead of time.
                 */
#if defined(ULOGNAME)
#ifdef LOGNAME_ISSOCK
                logFd = socket(AF_UNIX, SOCK_DGRAM, 0);
                if (logFd == -1) {
                        fprintf(stderr,
                            "ulog: Couldn't open UNIX socket \"%s\": %s\n",
                            ULOGNAME, strerror(errno));
                }
                else {
                    struct sockaddr_un  address;

                    address.sun_family = AF_UNIX;
                    (void)strcpy(address.sun_path, ULOGNAME);

                    if (connect(logFd, (struct sockaddr*)&address, 
                            sizeof(address)) == -1) {
                        fprintf(stderr,
                            "ulog: Couldn't connect(2) to UNIX socket \"%s\": "
                            "%s\n", ULOGNAME, strerror(errno));

                        (void)close(logFd);
                        logFd = -1;
                    }
                }                       /* logFd socket open */
#else
                logFd = open(ULOGNAME, O_WRONLY);       /* System V */
                if (logFd == -1) {
                        fprintf(stderr,
                            "ulog: Couldn't open file \"%s\": %s\n",
                            ULOGNAME, strerror(errno));
                }
#endif /*LOGNAME_ISSOCK*/
#else
                logFd = udpopen(LOGHOST, "syslog"); /* UDP socket open */
                if (logFd == -1) {
                        fprintf(stderr,
                            "ulog: Couldn't open UDP socket \"%s\": %s\n",
                            ULOGNAME, strerror(errno));
                }
#endif /*ULOGNAME*/

                if (logFd != -1)
                    logFilename = NULL;
        }

        if (logFd == -1) {
                return -1;
        }

        /* else */

#ifdef FD_CLOEXEC /* only try to do this on systems that support it */
        /* set descriptor to "close on exec" */
        if(logFilename == NULL || logFd != 2) /* special case for stderr */
        {
            if (fcntl(logFd, F_SETFD, FD_CLOEXEC) == -1)
            {
                    fprintf(stderr, "ulog: fcntl(FD_CLOEXEC) failure: %s\n",
                        strerror(errno));

                    (void) close(logFd);
                    return -1;
            }
            /* else */
        }
#endif

        return logFd;
}


/*
 * Returns the current options of the logging facility.
 *
 * @return                 The current set of options.
 */
unsigned ulog_get_options()
{
    return logOptions;
}


/*
 * Sets an option of the logging facility.  This function can only be used
 * to adjust LOG_NOTIME, LOG_PID, and LOG_IDENT: all other options are ignored.
 *
 * @param mask            Bitwise-OR mask of the options to set.
 * @param values          The option values.
 */
void ulog_set_options(
    unsigned mask,
    unsigned values)
{
    mask &= (LOG_NOTIME | LOG_PID | LOG_IDENT);

    logOptions = (logOptions & ~mask) | (values & mask);
}


#ifndef LOG_PRI
#define LOG_PRI(p)      ((p) & LOG_PRIMASK)     /* extract priority */
#endif
#ifndef LOG_FAC
#define LOG_FAC(p)      (((p) & LOG_FACMASK) >> 3)      /* facility of pri */
#endif


#ifdef _DEV_CONSLOG
#include <stropts.h> /* putmsg, struct strbuf */
#include <sys/strlog.h> /* struct log_ctl */
#endif 


/*
 * Logs a message given in argument-list form (analogous to vsyslog()).
 *
 * This function is unsafe: calling it from a signal handler as a result of
 * an interruption of an unsafe function results in undefined behavior.
 *
 * Arguments:
 *      pri     The message priority: LOG_ERROR, LOG_WARNING, LOG_NOTICE,
 *              LOG_INFO, or LOG_DEBUG.
 *      fmt     The format for the message.  The arguments for the format are
 *              taken from "args".
 *      args    The argument-list structure.  The caller must have invoked the
 *              "va_start" macro on "args".  This function doesn't invoke the
 *              "va_end" macro on "args".  On return, the value of "args" is
 *              indeterminate.
 *
 * Returns:
 *      -1      Failure.  errno is set.
 *      else    The number of bytes written.
 */
#ifdef STDC_ARGS
int
vulog(unsigned int pri, const char *fmt, va_list args)
{
#else
int
vulog(pri, fmt, args)
    unsigned int        pri;
    const char*         fmt;
    va_list             args;
{
#endif /* STDC_ARGS */
    int         nwritten = 0;           /* no bytes written */

#if LOG_NFACILITIES > 0
    if ((unsigned int)LOG_FAC(pri) < LOG_NFACILITIES)
    {
#endif

#if MASKDEBUG
        printf("                   pri 0x%08x\n", pri);
        printf("          LOG_PRI(pri) 0x%08x\n", LOG_PRI(pri));
        printf("LOG_MASK(LOG_PRI(pri)) 0x%08x\n", LOG_MASK(LOG_PRI(pri)));
        printf("               logMask 0x%08x\n", logMask);
        printf("                result 0x%08x\n",
                (LOG_MASK(LOG_PRI(pri)) & logMask));
#endif

        if (LOG_MASK(LOG_PRI(pri)) & logMask)
        {
            char                tbuf[MAX_LEN+1];
            char*               cp;
            size_t              len;    /* message length in bytes excluding
                                         * terminating NUL */
#ifdef _DEV_CONSLOG
            struct strbuf       ctl, dat;
            struct log_ctl      lc;
#endif
            sigset_t            prevSigMask;
            {
                /*
                 * Because this function might be called by a signal handler
                 * and because this function calls functions that are not safe
                 * in the presence of asynchronous signals (e.g., gmtime(),
                 * strftime()), the signal mask is set to block most signals.
                 * This ensures that this function is not re-entered due to an
                 * asynchronous signal.
                 */
                sigset_t        set;

                (void)sigfillset(&set);
                (void)sigdelset(&set, SIGABRT);
                (void)sigdelset(&set, SIGFPE);
                (void)sigdelset(&set, SIGILL);
                (void)sigdelset(&set, SIGSEGV);
                (void)sigdelset(&set, SIGBUS);
                (void)sigprocmask(SIG_BLOCK, &set, &prevSigMask);
            }

#if LOG_NFACILITIES > 0
            /* set default facility if none specified */
            if ((pri & LOG_FACMASK) == 0)
                pri |= logFacility;
#endif

            /*
             * If this open fails, the message might eventually be written to
             * the console.
             */
            if (logFd == -1)
                (void)openulog(logIdent, logOptions, logFacility, logFilename);

            /*
             * Build the message
             */
            cp = tbuf;

            if (logFilename == NULL)
            {
                /*
                 * Logging to a server.
                 */
#ifndef _DEV_CONSLOG
                /*
                 * Using syslogd(8).  Encode the priority prefix.
                 */
                cp += sprintf(tbuf, "<%d>", pri);
#else
                /*
                 * Logging to /dev/conslog.  Set constant portion of necessary
                 * structures.
                 */
                lc.level = 0;
                lc.flags = SL_NOTIFY | SL_ERROR | SL_CONSOLE | SL_WARN | 
                    SL_NOTE;
                lc.pri = pri;

                ctl.len = ctl.maxlen = sizeof(lc);
                ctl.buf = (char*)&lc;

                dat.buf = tbuf;
#endif /* !_DEV_CONSLOG */
            }
            
            if (!(logOptions & LOG_NOTIME))
            {
                /*
                 * Encode the timestamp.
                 */
                time_t          now;
                struct tm       tm_now;
                struct tm*      tmp = NULL;

                (void)time(&now);

                /* N.B.: default for this package is to use gmt */
                if(!(logOptions & LOG_LOCALTIME))
                    tmp = gmtime(&now);
                /* else, also covers failure of gmtime call */
                if(!tmp)
                    tmp = localtime(&now);
                tm_now = *tmp;

                /* (void) sprintf(cp, "%.15s ", ctime(&now) + 4); */
                cp += strftime(cp, (size_t)(sizeof(tbuf)-(cp-tbuf)),
                    "%b %d %H:%M:%S ", &tm_now);

                errno = 0;              /* ultrix 4.0 mips trashes errno in
                                         * ctime */
            }                           /* include timestamp */

            /*
             * Encode the optional utility identifier and/or PID.
             */
            {
                int     addSpace = 0;

                if (logOptions & LOG_IDENT && *logIdent)
                {
                    /*
                     * Encode the utility identifier.
                     */
                    (void)strcpy(cp, logIdent);

                    cp += strlen(cp);
                    addSpace = 1;
                }

                if (logOptions & LOG_PID)
                {
                    /*
                     * Encode the PID.
                     */
                    cp += sprintf(cp, "[%ld]", (long)getpid());
                    addSpace = 1;
                }

                if (addSpace)
                {
                    /*
                     * Encode a space after the utility identifier.
                     */
                    *cp++ = ' ';
                }
            }

            /*
             * Add the priority-level.
             */
            {
                const struct {
                    const unsigned      pri;
                    const char*         tag;
                    const unsigned      len;
                }               tags[] = {
                    {LOG_ERR,           "ERROR: ",      7},
                    {LOG_WARNING,       "WARN: ",       6},
                    {LOG_NOTICE,        "NOTE: ",       6},
                    {LOG_INFO,          "INFO: ",       6},
                    {LOG_DEBUG,         "DEBUG: ",      7}};
                int             i;
                unsigned        level = LOG_PRI(pri);

                for (i = 0; i < sizeof(tags)/sizeof(tags[0]); i++)
                    if (tags[i].pri == level)
                        break;

                if (i < sizeof(tags)/sizeof(tags[0])) {
                    (void)memcpy(cp, tags[i].tag, tags[i].len);
                    cp += tags[i].len;
                }
                else {
                    (void)memcpy(cp, "UNKNOWN: ", 9);
                    cp += 9;
                }
            }

            /*
             * ASIDE: We don't do %m substitution.
             */

            /*
             * Encode the client's message.
             */
            len = vsnprintf(cp, (size_t)((MAX_LEN-1)-(cp-tbuf)), fmt, args);
            if (len < 1)
                len = snprintf(cp, (size_t)((MAX_LEN-1)-(cp-tbuf)), 
                    "ulog(): vsnprintf() failure: format \"%s\"", fmt);
            cp += len;
            if (cp[-1] != '\n')
                cp++;                   /* leave room for NL */

            len = (size_t)(cp - tbuf);  /* excludes terminating NUL */

            /*
             * Attempt to write the message until either successful or an
             * unrecoverable failure occurs.
             */
            for (;;)
            {
                /*
                 * Adjust "len" to maximum allowable message length.
                 */
                if (len > maxLen)
                    len = maxLen;

                 /*
                  * Ensure proper termination of the message.
                  */
                tbuf[len-1] = '\n';
                tbuf[len] = '\0';

#if TBUFDIAG
                (void)fputs(tbuf, stdout);
#endif

                if (logFd != -1) {
                    if (logFilename != NULL)
                    {
                        /*
                         * Write the message to a file.
                         */
                        nwritten = write(logFd, tbuf, len);
                    }
                    else
                    {
                        /*
                         * Write the message to a server.
                         */
#if defined(_DEV_CONSLOG)
                        /*
                         * Write to /dev/conslog.
                         */
                        dat.len = dat.maxlen = (int)len;
                        errno = 0;
                        nwritten = putmsg(logFd, &ctl, &dat, 0);
#else
                        /*
                         * Write to syslogd(8).
                         */
#    if !__linux
                        nwritten = (int)write(logFd, tbuf, len);
#    else
                        /*
                         * The Linux syslogd(8) wants to see the terminating NUL
                         * character.
                         */
                        nwritten = (int)write(logFd, tbuf, len+1);
#    endif
#endif
                    }                   /* write to a server */

                    if (-1 == nwritten)
                    {
                        /*
                         * A write-error occurred.
                         */
                        if (EMSGSIZE == errno || ERANGE == errno)
                        {
                            /*
                             * Message too long.  Decrease the maximum
                             * allowable length and try again.
                             */
                            maxLen = len - 1;
                            continue;
                        }
                        else if (EINTR != errno &&
                            EAGAIN != errno && EWOULDBLOCK != errno)
                        {
                            /*
                             * Severe error.  Close the connection.
                             */
                            (void)closeulog();
                        }
                    }                   /* write error */
                }                       /* have open file-descriptor */

                if (logFd == -1 && (logOptions & LOG_CONS))
                {
                    /*
                     * Regular logging failed and the client requested writing
                     * to the console as a last resort.
                     */
                    int fd = open(CONSOLE, O_WRONLY|O_NONBLOCK);

                    if (-1 == fd)
                    {
                        nwritten = -1;
                    }
                    else
                    {
                        if (tbuf[0] != '<')
                        {
                            cp = tbuf;
                        }
                        else
                        {
                            cp = strchr(tbuf, '>');

                            if (cp == NULL)
                                cp = tbuf;
                            else
                                cp++;
                        }

                        nwritten = write(fd, cp, len - (size_t)(cp - tbuf));

                        (void)close(fd);
                    }                   /* CONSOLE open */
                }                       /* write to console */

                /*
                 * If here, then the write was successful or an unrecoverable
                 * failure occurred.
                 */
                break;
            }                           /* write-attempt loop */

            /*
             * Return the signal mask to the value it had when this function was
             * entered.
             */
            (void)sigprocmask(SIG_SETMASK, &prevSigMask, NULL);
        }                               /* priority implies writing */
#if LOG_NFACILITIES > 0
    }                                   /* valid logging facility */
#endif

    return nwritten;
}


/*
 * analogous to syslog()
 */
#ifdef STDC_ARGS
void
ulog(unsigned int pri, const char *fmt, ...)
{
#else
/*VARARGS2*/
int
ulog(pri, fmt, va_alist)
        unsigned int pri;
        char *fmt;
        va_dcl
{
#endif /* STDC_ARGS */
    va_list args;
#ifdef STDC_ARGS
        va_start(args, fmt);
#else
        va_start(args);
#endif /* !STDC_ARGS */
        (void)vulog(pri, fmt, args);
        va_end(args);
}


#ifndef NO_REPLACE_SYSLOG
/*
 * replace syslog (you may not want to do this...)
 */
#ifdef STDC_ARGS
#ifdef SYSLOG_RETURNS_INT
int
#else
void
#endif
syslog(int pri, const char *fmt, ...)
{
#else
/*VARARGS2*/
#ifdef SYSLOG_RETURNS_INT
int
#else
void
#endif
syslog(pri, fmt, va_alist)
        unsigned int pri;
        char *fmt;
        va_dcl
{
#endif /* STDC_ARGS */
    va_list args;
#ifdef STDC_ARGS
        va_start(args, fmt);
#else
        va_start(args);
#endif /* !STDC_ARGS */
        (void)vulog(pri, fmt, args);
        va_end(args);
#ifdef SYSLOG_RETURNS_INT
        return 0;
#endif
}
#endif /* NO_REPLACE_SYSLOG */


/*
 * Set the ulog mask, return the old mask.
 * Analogous to setlogmask(), execept it actually does something.
 */
unsigned int
setulogmask(unsigned int pmask)
{
        unsigned int omask;
        
        omask = logMask;
        if (pmask != 0)
                logMask = pmask;
        return (omask);
}


/*
 * If the bit in the logMask corresponding to * pri is set,
 *    unset it.
 * Otherwise, set it.
 * Used for toggling the verbosity, eg
 * toggleulogpri(LOG_INFO);
 */
int
toggleulogpri(unsigned int pri)
{
        pri = LOG_MASK(pri);
        if(pri & logMask)
        {
                logMask &= ~pri;
                return 0;
        }
        /* else */
        logMask |= pri;
        return 1;
}


/*
 * Cycle through logging priorities: Silent, Verbose, and debug.
 */
void
rollulogpri(void)
{
        switch(logMask & (LOG_MASK(LOG_INFO)|LOG_MASK(LOG_DEBUG))) {

        case (LOG_MASK(LOG_INFO)|LOG_MASK(LOG_DEBUG)):
                logMask &= ~(LOG_MASK(LOG_INFO)|LOG_MASK(LOG_DEBUG));
                break;
        case LOG_MASK(LOG_INFO):
                logMask |= LOG_MASK(LOG_DEBUG);
                break;
        case LOG_MASK(LOG_DEBUG):
                logMask |= LOG_MASK(LOG_INFO);
                break;
        case 0:
                logMask |= LOG_MASK(LOG_INFO);
                break;
        }
}

/*
 * Get the ulog mask.
 * Useful for determining verbosity.
 */
unsigned int
getulogmask(void)
{
        return (logMask);
}


int
ulogIsVerbose(void)
{
        return (logMask & LOG_MASK(LOG_INFO)) != 0;
}


int
ulogIsDebug(void)
{
        return (logMask & LOG_MASK(LOG_DEBUG)) != 0;
}


/*
 * Log system call errors
 * Use where you would want to call perror(3).
 * Calling sequence is
 *      serror(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *         serror("shutting down");
 *         serror("can't open %s", file_name);
 *         serror("process %d in state %s",pid,state);
 */
#ifdef STDC_ARGS
void
serror(const char *fmt, ...)
#else
/*VARARGS1*/
void
serror(fmt, va_alist)
     char *fmt;
     va_dcl
#endif /* STDC_ARGS */
{
    va_list args;
#ifdef STDC_ARGS
        va_start(args ,fmt);
#else
        va_start(args);
#endif /* STDC_ARGS */
        if(errno != 0)
        {
                char buf[1024];
                int errnum = errno;             /* save real errno in case we wipe it out */
                char *cp;
                (void) vsnprintf(buf, sizeof(buf)-1, fmt, args);
                for(cp = buf; *cp != 0; )
                    cp++;
                strcat(cp, ": %s");
                ulog(LOG_ERR, buf, strerror(errnum));
                errno = 0;
        }
        else
        {
                (void) vulog(LOG_ERR, fmt, args);
        }
        va_end(args);
}


/*
 * Log program errors
 * Calling sequence is
 *      uerror(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *
 *         uerror("Inconsistant input %s", input);
 */
#ifdef STDC_ARGS
void
uerror(const char *fmt, ...)
#else
/*VARARGS1*/
void
uerror(fmt, va_alist)
     char *fmt;
     va_dcl
#endif /* STDC_ARGS */
{
    va_list args;
#ifdef STDC_ARGS
        va_start(args ,fmt);
#else
        va_start(args);
#endif /* STDC_ARGS */
        (void) vulog(LOG_ERR, fmt, args);
        va_end(args);
}


/*
 * Log program warnings
 * Calling sequence is
 *      uwarn(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *
 *         uwarn("Inconsistant input %s", input);
 */
#ifdef STDC_ARGS
void
uwarn(const char *fmt, ...)
#else
/*VARARGS1*/
void
uwarn(fmt, va_alist)
     char *fmt;
     va_dcl
#endif /* STDC_ARGS */
{
    va_list args;
#ifdef STDC_ARGS
        va_start(args ,fmt);
#else
        va_start(args);
#endif /* STDC_ARGS */
        (void) vulog(LOG_WARNING, fmt, args);
        va_end(args);
}


/*
 * Log "Normal but significant conditions"
 * Calling sequence is
 *      unotice(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *
 *         unotice("Shutting down on signal %s", s_signal(sig));
 */
#ifdef STDC_ARGS
void
unotice(const char *fmt, ...)
#else
/*VARARGS1*/
void
unotice(fmt, va_alist)
     char *fmt;
     va_dcl
#endif /* STDC_ARGS */
{
    va_list args;
#ifdef STDC_ARGS
        va_start(args ,fmt);
#else
        va_start(args);
#endif /* STDC_ARGS */
        (void) vulog(LOG_NOTICE, fmt, args);
        va_end(args);
}


/*
 * Log informational messages
 * Calling sequence is
 *      uinfo(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *
 *         uinfo("%s", info->ident);
 */
#ifdef STDC_ARGS
void
uinfo(const char *fmt, ...)
#else
/*VARARGS1*/
void
uinfo(fmt, va_alist)
     char *fmt;
     va_dcl
#endif /* STDC_ARGS */
{
    va_list args;
#ifdef STDC_ARGS
        va_start(args ,fmt);
#else
        va_start(args);
#endif /* STDC_ARGS */
        (void) vulog(LOG_INFO, fmt, args);
        va_end(args);
}


/*
 * Log debugging info.
 * Calling sequence is
 *      udebug(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *         udebug("entering myproc, arg = %d", arg);
 *
 * This one is a little different than the others in that it behaves diferently
 * when logging to a file than when logging to syslogd(8).
 */
#ifdef STDC_ARGS
void
udebug(const char *fmt, ...)
#else
/*VARARGS1*/
void
udebug(fmt, va_alist)
     char *fmt;
     va_dcl
#endif /* STDC_ARGS */
{
    va_list     args;
    int         noChange = logFilename == NULL || logFd == -1;
        
#ifdef STDC_ARGS
        va_start(args ,fmt);
#else
        va_start(args);
#endif /* STDC_ARGS */

        if (noChange)
        {
            (void) vulog(LOG_DEBUG, fmt, args);
        }
        else
        {
            unsigned    prevLogOptions = logOptions;

            logOptions &= ~(unsigned)(LOG_NOTIME | LOG_IDENT | LOG_PID);

            (void)vulog(LOG_DEBUG, fmt, args);

            logOptions = prevLogOptions;
        }

        va_end(args);
}

void
_uassert(
        const char *ex,
        const char *file,
        int line)
{
        uerror("assertion \"%s\" failed: file \"%s\", line %d\n",
                ex, file, line);
        abort();
}


#ifdef TIRPC
#include <tiuser.h>
extern int t_errno;
extern char *t_errlist[];
extern int t_nerr;

/*
 * Log tli call errors
 * Use where you would want to call t_error(3N).
 * Calling sequence is
 *      terror(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.
 */
#ifdef STDC_ARGS
void
terror(const char *fmt, ...)
#else
/*VARARGS1*/
void
terror(fmt, va_alist)
     char *fmt;
     va_dcl
#endif /* STDC_ARGS */
{
    va_list args;
#ifdef STDC_ARGS
        va_start(args ,fmt);
#else
        va_start(args);
#endif /* STDC_ARGS */
        if(t_errno != 0)
        {
                char buf[1024];
                int errnum = t_errno;   /* save real t_errno in case we wipe it out */
                char *cp;
                (void) vsnprintf(buf, sizeof(buf)-1, fmt, args);
                for(cp = buf; *cp != 0; cp++) /*EMPTY*/;
                strcat(cp, ": %s");
                ulog(LOG_ERR, buf,
                        (errnum > 0 && errnum < t_nerr) ? t_errlist[errnum] : "");
                t_errno = 0;
        }
        else
        {
                (void) vulog(LOG_ERR, fmt, args);
        }
        va_end(args);
}
#endif


/* strip off leading path */
const char *
ubasename(const char *av0)
{
        const char *logident;
#ifdef vms
#define SEP     ']'
#endif
#ifndef SEP
#define SEP     '/'
#endif
        if ((logident = strrchr(av0, SEP)) == NULL)
                logident = av0;
        else
            logident++;
        return logident;
}


#ifdef  TEST0
#include <stdlib.h>

static void
timecheck(void)
{
        time_t now;
        struct tm local[1];
        struct tm gmt[1];
        char cp[32];
        (void) time(&now);
        *local = *(localtime(&now));
        strftime(cp, sizeof(cp), "%b %d %H:%M:%S ", local);
        unotice("Local: %s (%02d)", cp, local->tm_hour);
        *gmt = *(gmtime(&now)); /* may dump core */
        strftime(cp, sizeof(cp), "%b %d %H:%M:%S ", gmt);
        unotice("  UTC: %s (%02d)", cp, gmt->tm_hour);
}

static void
ploop(int ii)
{
        ulog(LOG_ALERT, "%d Alert\n", ii);
        ulog(LOG_CRIT, "%d Crit\n", ii);
        ulog(LOG_ERR, "%d Err\n", ii);
        ulog(LOG_WARNING, "%d Warning\n", ii);
        ulog(LOG_NOTICE, "%d Notice\n", ii);
        ulog(LOG_INFO, "%d Info %d \"%m\" %d %s %f %d\n",
                ii, 1, 2, "string", 3.1415, 4 );
        ulog(LOG_DEBUG, "%d Debug\n", ii);
}

static void
usage(char *av0)
{
        fprintf(stderr, "Usage: %s [-l logfname]\n", av0);
        exit(1);
}

int
main(int ac, char *av[])
{
        char *logfname = 0;
        int logfd = -1;

        {
        extern int optind;
        extern int opterr;
        extern char *optarg;
        int ch;

        opterr = 1;

        while ((ch = getopt(ac, av, "l:")) != EOF)
                switch (ch) {
                case 'l':
                        logfname = optarg;
                        break;
                case '?':
                        usage(av[0]);
                        break;
                }
        }

        printf("ulog[%d]\n", getpid());

        logfd = openulog("ulog", (LOG_CONS|LOG_PID), LOG_LDM, logfname);
        if(logfd == -1)
                perror("openulog");

        ploop(0);
        (void) setulogmask(LOG_MASK(LOG_NOTICE));
        ploop(1);
        (void) setulogmask(LOG_UPTO(LOG_INFO));
        ploop(2);
        (void) setulogmask(LOG_UPTO(LOG_ALERT));
        ploop(3);
        (void) setulogmask((LOG_UPTO(LOG_DEBUG) & ~(LOG_UPTO(LOG_CRIT))));
        ploop(4);
        {
        int fd = open("/dev/kmem", O_RDWR, 0664);
        if(fd == -1)
                serror("serror %d %d %s %f %d",
                        1, 2, "string", 3.1415, 4 );
        }

        timecheck();

        /* cheating */
        logOptions |= LOG_LOCALTIME;

        timecheck();
        closeulog();

        return 0;
}
#endif

#ifdef TEST1

void
ploop2(ii)
int ii;
{
        unotice("%d run (notice)", ii);
        errno = 13;
        serror("error: filename");
        serror("noerror");
        uerror("error %d %d %s %f %d", 
                        1, 2, "string", 3.1415, 4 );
        uinfo("information");
        udebug("debug  %d %d %s %f %d",
                1, 2, "string", 3.1415, 4 );
        unotice("\t%d end (notice)", ii);
}


main()
{
        int logfd = -1;
        logfd = openulog("ulog", (LOG_CONS|LOG_PID), LOG_LOCAL0, 0);
        if(logfd == -1)
                perror("openulog");
        ploop2(1);
        closeulog();
        logfd = openulog("ulog", (LOG_CONS|LOG_PID), LOG_LOCAL0, "logfile");
        (void) setulogmask((LOG_UPTO(LOG_DEBUG) & ~(LOG_MASK(LOG_INFO))));
        if(logfd == -1)
                perror("openulog");
        ploop2(2);
        closeulog();
}
#endif
