/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: rtstats.c,v 1.2.10.1.2.7 2008/04/15 16:34:12 steve Exp $ */

/* 
 *
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <regex.h>
#include "ldm.h"
#include "atofeedt.h"
#include "globals.h"
#include "ldmprint.h"
#include "ulog.h"
#include "pq.h"
#include "paths.h"
#ifdef NO_SETENV
#include "setenv.h"
#endif /* NO_SETENV */
#include "RegularExpressions.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

#ifndef DEFAULT_INTERVAL
#define DEFAULT_INTERVAL 15
#endif

static volatile int intr = 0;
static volatile int stats_req = 0;

#ifndef DEFAULT_FEEDTYPE
#define DEFAULT_FEEDTYPE ANY
#endif

/* binstats.c */
extern int binstats(const prod_info *infop,
        const struct timeval *reftimep);

extern void dump_statsbins(void);
extern void syncbinstats(void);

unsigned remotePort = LDM_PORT;


/*
 */
/*ARGSUSED*/
static int
addtostats(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *notused)
{
        struct timeval tv;
        pq_ctimestamp(pq, &tv);
        if(tvIsNone(tv))
                tv = TS_ZERO;
        if(ulogIsVerbose())
                uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));
        binstats(infop, &tv);
        return 0;
}


static void
usage(const char *av0) /*  id string */
{
        (void)fprintf(stderr,
                "Usage: %s [options]\n\tOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-v           Verbose, tell me about each product\n");
        (void)fprintf(stderr,
                "\t-l logfile   Log to a file rather than syslog\n");
        (void)fprintf(stderr,
                "\t-f feedtype  Scan for data of type \"feedtype\", defaults to \"ANY - EXP\"\n");
                /* NB: above line used s_feedtypet(DEFAULT_FEEDTYPE)...however this looks ugly for ANY - EXP */
        (void)fprintf(stderr,
                "\t-p pattern   Interested in products matching \"pattern\" (default \".*\")\n") ;
        (void)fprintf(stderr,
                "\t-q queue     Default \"%s\"\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-o offset    The oldest product we will consider is \"offset\" secs before now (default: most recent in queue)\n");
        (void)fprintf(stderr,
                "\t-i interval  Poll queue after \"interval\" secs (default %d)\n",
                DEFAULT_INTERVAL);
        (void)fprintf(stderr,
                "\t-h hostname   The LDM server which will accept these statistics. Default is LOCALHOST\n");
        exit(1);
}


void
cleanup(void)
{
        unotice("Exiting"); 

        if(pq && !intr) 
                (void)pq_close(pq);

        (void) closeulog();
}


static void
signal_handler(int sig)
{
#ifdef SVR3SIGNALS
        /* 
         * Some systems reset handler to SIG_DFL upon entry to handler.
         * In that case, we reregister our handler.
         */
        (void) signal(sig, signal_handler);
#endif
        switch(sig) {
        case SIGINT :
                intr = 1;
                exit(0);
                /*NOTREACHED*/
        case SIGTERM :
                done = 1;       
                return;
        case SIGUSR1 :
                stats_req = 1;
                return;
        case SIGUSR2 :
                toggleulogpri(LOG_INFO);
                return;
        }
}


/*
 * register the signal_handler
 */
static void
set_sigactions(void)
{
        struct sigaction sigact;

        sigemptyset(&sigact.sa_mask);

        /* Ignore these */
        sigact.sa_flags = 0;
        sigact.sa_handler = SIG_IGN;
        (void) sigaction(SIGHUP, &sigact, NULL);
        (void) sigaction(SIGPIPE, &sigact, NULL);
        (void) sigaction(SIGALRM, &sigact, NULL);
        (void) sigaction(SIGCHLD, &sigact, NULL);

        /* Handle these */
#ifdef SA_RESTART       /* SVR4, 4.3+ BSD */
        /* usually, restart system calls */
        sigact.sa_flags |= SA_RESTART;
#endif
        sigact.sa_handler = signal_handler;
        (void) sigaction(SIGTERM, &sigact, NULL);
        (void) sigaction(SIGUSR1, &sigact, NULL);
        (void) sigaction(SIGUSR2, &sigact, NULL);

        /* Don't restart after interrupt */
        sigact.sa_flags = 0;
#ifdef SA_INTERRUPT     /* SunOS 4.x */
        sigact.sa_flags |= SA_INTERRUPT;
#endif
        (void) sigaction(SIGINT, &sigact, NULL);
}

int main(int ac, char *av[])
{
        const char *progname = ubasename(av[0]);
        char *logfname;
        prod_class_t clss;
        prod_spec spec;
        int status = 0;
        int interval = DEFAULT_INTERVAL;
        int fterr;
        int logoptions = (LOG_CONS|LOG_PID) ;
        int toffset = TOFFSET_NONE;
        extern const char *remote;

        logfname = "";

        remote = "localhost";

        if(set_timestamp(&clss.from) != ENOERR) /* corrected by toffset below */
        {
                int errnum = errno;
                fprintf(stderr, "Couldn't set timestamp: %s", 
                        strerror(errnum));
                exit(1);
        }
        clss.to = TS_ENDT;
        clss.psa.psa_len = 1;
        clss.psa.psa_val = &spec;
        spec.feedtype = DEFAULT_FEEDTYPE;
        spec.pattern = ".*";
        
        /*
         * Sigh, in order to read back from existing stats files,
         * we call mktime(3). Since the stats are in UTC,
         * and mktime uses "local" time, the local time for
         * this program must be UTC.
         */
        if(setenv("TZ", "UTC0",1))
        {
                int errnum = errno;
                fprintf(stderr, "%s: setenv: Couldn't set TZ: %s\n",
                        progname, strerror(errnum));
                exit(1);        
        }
        
        /*
         * Check the environment for some options.
         * May be overridden by command line switches below.
         */
        {
                const char *ldmpqfname = getenv("LDMPQFNAME");
                if(ldmpqfname != NULL)
                        pqfname = ldmpqfname;
        }

        {
        extern int opterr;
        extern char *optarg;
        int ch;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) | 
            LOG_MASK(LOG_NOTICE));

        opterr = 1;

        while ((ch = getopt(ac, av, "vxl:p:f:q:o:i:h:P:")) != EOF)
                switch (ch) {
                case 'v':
                        logmask |= LOG_MASK(LOG_INFO);
                        break;
                case 'x':
                        logmask |= LOG_MASK(LOG_DEBUG);
                        break;
                case 'l':
                        logfname = optarg;
                        break;
                case 'h':
                        remote = optarg;
                        break;
                case 'p':
                        spec.pattern = optarg;
                        break;
                case 'f':
                        fterr = strfeedtypet(optarg, &spec.feedtype) ;
                        if(fterr != FEEDTYPE_OK)
                        {
                                fprintf(stderr, "Bad feedtype \"%s\", %s\n",
                                        optarg, strfeederr(fterr)) ;
                                usage(progname);        
                        }
                        break;
                case 'q':
                        pqfname = optarg;
                        break;
                case 'o':
                        toffset = atoi(optarg);
                        if(toffset == 0 && *optarg != '0')
                        {
                                fprintf(stderr, "%s: invalid offset %s\n",
                                         av[0], optarg);
                                usage(av[0]);   
                        }
                        break;
                case 'P': {
                    char*       suffix = "";
                    long        port;

                    errno = 0;
                    port = strtol(optarg, &suffix, 0);

                    if (0 != errno || 0 != *suffix ||
                        0 >= port || 0xffff < port) {

                        (void)fprintf(stderr, "%s: invalid port %s\n",
                             av[0], optarg);
                        usage(av[0]);   
                    }

                    remotePort = (unsigned)port;

                    break;
                }
                case 'i':
                        interval = atoi(optarg);
                        if(interval == 0 && *optarg != '0')
                        {
                                fprintf(stderr, "%s: invalid interval %s",
                                        av[0], optarg);
                                usage(av[0]);
                        }
                        break;
                case '?':
                        usage(progname);
                        break;
                }

        (void) setulogmask(logmask);

        if (re_isPathological(spec.pattern))
        {
                fprintf(stderr, 
                        "Adjusting pathological regular-expression \"%s\"\n",
                        spec.pattern);
                re_vetSpec(spec.pattern);
        }
        status = regcomp(&spec.rgx,
                spec.pattern,
                REG_EXTENDED|REG_NOSUB);
        if(status != 0)
        {
                fprintf(stderr, "Bad regular expression \"%s\"\n",
                        spec.pattern);
                usage(av[0]);
        }

        }

        /*
         * Set up error logging.
         */
        (void) openulog(progname,
                logoptions, LOG_LDM, logfname);
        unotice("Starting Up (%d)", getpgrp());

        /*
         * register exit handler
         */
        if(atexit(cleanup) != 0)
        {
                serror("atexit");
                exit(1);
        }

        /*
         * set up signal handlers
         */
        set_sigactions();


        /*
         * Open the product que
         */
        status = pq_open(pqfname, PQ_READONLY, &pq);
        if(status)
        {
                if (PQ_CORRUPT == status) {
                    uerror("The product-queue \"%s\" is inconsistent\n",
                            pqfname);
                }
                else {
                    uerror("pq_open failed: %s: %s\n",
                            pqfname, strerror(status));
                }
                exit(1);
        }

        if(toffset == TOFFSET_NONE)
        {
                /*
                 * Be permissive with the time filter,
                 * jump now to the end of the queue.
                 */
                clss.from = TS_ZERO;
                (void) pq_last(pq, &clss, NULL);
        }
        else
        {
                /*
                 * Filter and queue position set by
                 * toffset.
                 */
                clss.from.tv_sec -= toffset;
                pq_cset(pq, &clss.from);
        }

        while(exitIfDone(0))
        {
                if(stats_req)
                {
                        dump_statsbins();
                        stats_req = 0;
                }

                status = pq_sequence(pq, TV_GT, &clss, addtostats, 0);

                switch(status) {
                case 0: /* no error */
                        continue; /* N.B., other cases sleep */
                case PQUEUE_END:
                        udebug("End of Queue");
                        break;
                case EAGAIN:
                case EACCES:
                        udebug("Hit a lock");
                        break;
                default:
                        uerror("pq_sequence failed: %s (errno = %d)",
                                strerror(status), status);
                        exit(1);
                        break;
                }

                syncbinstats();

                if(interval == 0)
                {
                        done = 1;
                        break;
                }

                pq_suspend(interval);
        }

        return 0;
}
