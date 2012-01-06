/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqmon.c,v 1.7.16.1.2.2 2007/02/09 21:35:02 steve Exp $ */

/* 
 *  Monitor a product queue by reporting some of its vital statistics.
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <regex.h>
#include "ldm.h"
#include "atofeedt.h"
#include "globals.h"
#include "ldmprint.h"
#include "ulog.h"
#include "pq.h"
#include "paths.h"
#include "md5.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

/* default "one trip" */
#ifndef DEFAULT_INTERVAL
#define DEFAULT_INTERVAL 0
#endif

#ifndef DEFAULT_FEEDTYPE
#define DEFAULT_FEEDTYPE ANY
#endif

static volatile int intr = 0;

static void
usage(const char *av0) /*  id string */
{
        (void)fprintf(stderr,
                "Usage: %s [options] [outputfile]\n\tOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-l logfile   Log to a file rather than stderr\n");
        (void)fprintf(stderr,
                "\t-q pqfname   (default \"%s\")\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-i interval  Poll queue after \"interval\" secs (default %d)\n",
                DEFAULT_INTERVAL);
        (void)fprintf(stderr,
                "\t             (\"interval\" of 0 means exit at end of queue)\n");
        (void)fprintf(stderr,
                "Output defaults to standard output\n");
        exit(1);
}


static void
cleanup(void)
{
        unotice("Exiting"); 

        if(!intr)
        {
                if(pq != NULL)  
                        (void)pq_close(pq);
        }
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
                intr = !0;
                exit(0);
        case SIGTERM :
                done = !0;      
                return;
        case SIGUSR1 :
                return;
        case SIGUSR2 :
                rollulogpri();
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
        sigact.sa_flags = 0;

        /* Ignore these */
        sigact.sa_handler = SIG_IGN;
        (void) sigaction(SIGHUP, &sigact, NULL);
        (void) sigaction(SIGPIPE, &sigact, NULL);
        (void) sigaction(SIGALRM, &sigact, NULL);
        (void) sigaction(SIGCHLD, &sigact, NULL);
        (void) sigaction(SIGCONT, &sigact, NULL); /* so won't be woken up for every product */

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


static void
hndlr_noop(int sig)
{
#ifndef NDEBUG
        switch(sig) {
        case SIGALRM :
                udebug("SIGALRM") ;
                return ;
        }
        udebug("hndlr_noop: unhandled signal: %d", sig) ;
#endif
        /* nothing to do, just wake up */
        return;
}


/*
 * Suspend yourself (sleep) until
 * one of the following events occurs:
 *   You receive a signal that you handle.
 *   "maxsleep" seconds elapse.
 *   If "maxsleep" is zero, you could sleep forever. 
 */
static int
xsuspend(unsigned int maxsleep)
{
        
        struct sigaction sigact, asavact;
        sigset_t mask, savmask;

        /* block ALRM while we set up */
        sigemptyset(&mask);
        if(maxsleep)
                sigaddset(&mask, SIGALRM);
        (void) sigprocmask(SIG_BLOCK, &mask, &savmask);

        /*
         * Set up handlers for ALRM, stashing old
         */
        sigemptyset(&sigact.sa_mask);
        sigact.sa_flags = 0;
        sigact.sa_handler = hndlr_noop;
        if(maxsleep)
        {
                /* set the alarm */
                (void) sigaction(SIGALRM, &sigact, &asavact);
                (void) alarm(maxsleep);
        }
        
        /*
         * Unblock the signals.
         */
        mask = savmask;
        if(maxsleep)
                sigdelset(&mask, SIGALRM);

        /* Nighty night... */
        (void) sigsuspend(&mask);

        /* Now we are back, restore state */
        if(maxsleep)
        {
                (void)alarm(0);
                (void) sigaction(SIGALRM, &asavact, NULL );
        }
        (void) sigprocmask(SIG_SETMASK, &savmask, NULL);

        return 0;
}



int
main(int ac, char *av[])
{
        const char *progname = ubasename(av[0]);
        char *logfname;
        int status = 0;
        int interval = DEFAULT_INTERVAL;
        int logoptions = (LOG_CONS|LOG_PID) ;
        int list_extents = 0;

        logfname = "";

        if(isatty(fileno(stderr)))
        {
                /* set interactive defaults */
                logfname = "-" ;
                logoptions = 0 ;
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
        extern int optind;
        extern int opterr;
        extern char *optarg;
        int ch;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE));

        opterr = 1;

        while ((ch = getopt(ac, av, "vxl:q:o:i:")) != EOF)
                switch (ch) {
                case 'v':
                        logmask |= LOG_MASK(LOG_INFO);
                        break;
                case 'x':
                        logmask |= LOG_MASK(LOG_DEBUG);
                        list_extents = 1;
                        break;
                case 'l':
                        logfname = optarg;
                        break;
                case 'q':
                        pqfname = optarg;
                        break;
                case 'i':
                        interval = atoi(optarg);
                        if(interval == 0 && *optarg != '0')
                        {
                                fprintf(stderr, "%s: invalid interval %s",
                                        progname, optarg);
                                usage(progname);
                        }
                        break;
                case '?':
                        usage(progname);
                        break;
                }

        (void) setulogmask(logmask);

        /* last arg, outputfname, is optional */
        if(ac - optind > 0)
        {
                const char *const outputfname = av[optind];
                if(freopen(outputfname, "a+b", stdout) == NULL)
                {
                        status = errno;
                        fprintf(stderr, "%s: Couldn't open \"%s\": %s\n",
                                progname, outputfname, strerror(status));
                }
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

        unotice("nprods nfree  nempty      nbytes  maxprods  maxfree  minempty    maxext  age");
        while(exitIfDone(1))
        {
            size_t nprods;
            size_t nfree;
            size_t nempty;
            size_t nbytes;
            size_t maxprods;
            size_t maxfree;
            size_t minempty;
            size_t maxbytes;
            double age_oldest;
            size_t maxextent;

            status = pq_stats(pq, &nprods,   &nfree,   &nempty, &nbytes,
                              &maxprods, &maxfree, &minempty, &maxbytes, 
                              &age_oldest, &maxextent);

            if (status) {
                uerror("pq_stats failed: %s (errno = %d)",
                       strerror(status), status);
                exit(1);
            }

            unotice("%6ld %5lu %7lu %11lu %9lu %8lu %9lu %9lu %.0f",
                    nprods,   nfree,   nempty, nbytes,
                    maxprods, maxfree, minempty, maxextent, age_oldest);
            if(list_extents) {
                status = pq_fext_dump(pq);
            }
            if (status) {
                uerror("pq_fext_dump failed: %s (errno = %d)",
                       strerror(status), status);
                exit(1);
            }
            
            if(interval == 0)
                break;
            xsuspend(interval);
            
        }

        exit(0);
        /*NOTREACHED*/
}
