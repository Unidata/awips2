/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqcheck.c,v 1.2.4.1.4.3.2.2 2004/11/09 18:01:56 steve Exp $ */

/* 
 *  Check a product-queue.
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

static const char *pqfname = DEFAULT_QUEUE;
static pqueue *pq = NULL;

static void
usage(const char *av0) /*  id string */
{
        (void)fprintf(stderr,
                "Usage: %s [options]\n\tOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-F           Force. Set the writer-counter to zero "
                "(creating it if necessary).\n");
        (void)fprintf(stderr,
                "\t-v           Verbose\n");
        (void)fprintf(stderr,
                "\t-l logfile   Log to a file rather than stderr\n");
        (void)fprintf(stderr,
                "\t-q pqfname   (default \"%s\")\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "Output defaults to standard output\n");
        exit(1);
}


static void
cleanup(void)
{
        unotice("Exiting"); 

        if(pq != NULL)  
                (void)pq_close(pq);

        (void) closeulog();
}


/*
 * Set signal handling.
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
}


/*
 * Returns:
 *      0       Success.  Write-count of product-queue is zero.
 *      1       System failure.  See error-message.
 *      2       Product-queue doesn't support a writer-counter.  Not possible
 *              if "-F" option used.
 *      3       Write-count of product-queue is greater than zero.  Not possible
 *              if "-F" option used.
 *      4       The product-queue is internally inconsistent.
 */
int main(int ac, char *av[])
{
        const char *progname = ubasename(av[0]);
        char *logfname;
        int status = 0;
        int logoptions = (LOG_CONS|LOG_PID) ;
        unsigned write_count;
        int force = 0;

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
            extern int opterr;
            extern char *optarg;
            int ch;
            int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
                LOG_MASK(LOG_NOTICE));

            opterr = 1;

            while ((ch = getopt(ac, av, "Fvxl:q:")) != EOF)
                    switch (ch) {
                    case 'F':
                            force = 1;
                            break;
                    case 'v':
                            logmask |= LOG_MASK(LOG_INFO);
                            break;
                    case 'x':
                            logmask |= LOG_MASK(LOG_DEBUG);
                            break;
                    case 'l':
                            logfname = optarg;
                            break;
                    case 'q':
                            pqfname = optarg;
                            break;
                    case '?':
                            usage(progname);
                            break;
                    }

            (void) setulogmask(logmask);
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
                return 1;
        }

        /*
         * set up signal handlers
         */
        set_sigactions();

        if (force) {
            /*
             * Add writer-counter capability to the file, if necessary, and set
             * the writer-counter to zero.
             */
            status = pq_clear_write_count(pqfname);
            if (status) {
                if (PQ_CORRUPT == status) {
                    uerror("The product-queue \"%s\" is inconsistent", pqfname);
                    return 4;
                }
                else {
                    uerror("pq_clear_write_count() failure: %s: %s",
                            pqfname, strerror(status));
                    return 1;
                }
            }
            write_count = 0;
        }
        else {
            /*
             * Get the writer-counter of the product-queue.
             */
            status = pq_get_write_count(pqfname, &write_count);
            if (status) {
                if (ENOSYS == status) {
                    uerror("Product-queue \"%s\" doesn't have a writer-counter",
                        pqfname);
                    return 2;
                }
                else if (PQ_CORRUPT == status) {
                    uerror("Product-queue \"%s\" is inconsistent", pqfname);
                    return 4;
                }
                else {
                    uerror("pq_get_write_count() failure: %s: %s",
                        pqfname, strerror(status));
                    return 1;
                }
            }
        }

        uinfo("The writer-counter of the product-queue is %u", write_count);

        return write_count == 0 ? 0 : 3;
}
