/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqcat.c,v 1.40.10.1.2.4 2008/04/15 16:34:09 steve Exp $ */

/* 
 *  dump a pq
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
#include "RegularExpressions.h"

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

static volatile int done = 0;
static volatile int intr = 0;
static volatile int stats_req = 0;
static int showProdOrigin = 0;

static const char *pqfname = DEFAULT_QUEUE;
static pqueue *pq = NULL;

static int nprods = 0;

static MD5_CTX *md5ctxp = NULL;

static void
dump_stats(void)
{
        unotice("Number of products %d", nprods);
}

/*
 */
/*ARGSUSED*/
static int
writeprod(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *notused)
{
        if(ulogIsVerbose())
        {
                if (showProdOrigin)
                {
                        uinfo("%s %s", s_prod_info(NULL, 0, infop, ulogIsDebug()), infop->origin);
                } else
                {
                        uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));
                }
        }

        if(md5ctxp != NULL) /* -c option */
        {
                signaturet check;
                MD5Init(md5ctxp);
                MD5Update(md5ctxp, datap, infop->sz);
                MD5Final((unsigned char*)check, md5ctxp);
                if(memcmp(infop->signature, check, sizeof(signaturet)) != 0)
                {
                        char sb[33]; char cb[33];
                        uerror("signature mismatch: %s != %s",
                                 s_signaturet(sb, sizeof(sb), infop->signature),
                                 s_signaturet(cb, sizeof(cb), check));
                }
        }

        if( write(STDOUT_FILENO, datap, infop->sz) !=
                        infop->sz)
        {
                int errnum = errno;
                serror( "data write failed") ;
                return errnum;
        }

        nprods++;

        return 0;
}

static int
tallyProds(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *notused)
{
        nprods++;

        return 0;
}

static void
usage(const char *av0) /*  id string */
{
        (void)fprintf(stderr,
                "Usage: %s [options] [outputfile]\n\tOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-v           Verbose, tell me about each product\n");
        (void)fprintf(stderr,
                "\t-l logfile   Log to a file rather than stderr\n");
        (void)fprintf(stderr,
                "\t-f feedtype  Scan for data of type \"feedtype\" (default \"%s\")\n", s_feedtypet(DEFAULT_FEEDTYPE));
        (void)fprintf(stderr,
                "\t-p pattern   Interested in products matching \"pattern\" (default \".*\")\n") ;
        (void)fprintf(stderr,
                "\t-q pqfname   (default \"%s\")\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-o offset    Set the \"from\" time \"offset\" secs before now\n");
        (void)fprintf(stderr,
                "\t             (default \"from\" the beginning of the epoch)\n");
        (void)fprintf(stderr,
                "\t-i interval  Poll queue after \"interval\" secs (default %d)\n",
                DEFAULT_INTERVAL);
        (void)fprintf(stderr,
                "\t             (\"interval\" of 0 means exit at end of queue)\n");
        (void)fprintf(stderr,
                "\t-c           Check, verify MD5 signature\n");
        (void)fprintf(stderr,
                "\t-O           Include product origin in verbose output\n");
        (void)fprintf(stderr,
                "\t             (valid only with -v option)\n");
        (void)fprintf(stderr,
                "\t-s           Check queue for sanity/non-corruption\n");
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
                if(md5ctxp != NULL)
                        free_MD5_CTX(md5ctxp);  

                if(pq != NULL)  
                        (void)pq_close(pq);
        }

        dump_stats();

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
                stats_req = !0;
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
        int logoptions = (LOG_CONS|LOG_PID) ;
        int queueSanityCheck = FALSE;

        logfname = "";

        if(isatty(fileno(stderr)))
        {
                /* set interactive defaults */
                logfname = "-" ;
                logoptions = 0 ;
        }

        clss.from = TS_ZERO; /* default dump the whole file */
        clss.to = TS_ENDT;
        clss.psa.psa_len = 1;
        clss.psa.psa_val = &spec;
        spec.feedtype = ANY;
        spec.pattern = ".*";
        
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
        int fterr;

        opterr = 1;

        while ((ch = getopt(ac, av, "cvxOsl:p:f:q:o:i:")) != EOF)
                switch (ch) {
                case 'c':
                        md5ctxp = new_MD5_CTX();
                        if(md5ctxp == NULL)
                                serror("new_md5_CTX failed");
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
                case 'p':
                        spec.pattern = optarg;
                        /* compiled below */
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
                        (void) set_timestamp(&clss.from);
                        clss.from.tv_sec -= atoi(optarg);
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
                case 'O':
                        showProdOrigin = TRUE;
                        break;
                case 's':
                        queueSanityCheck = TRUE;
                        break;
                case '?':
                        usage(progname);
                        break;
                }

        (void) setulogmask(logmask);

        if (re_isPathological(spec.pattern))
        {
                fprintf(stderr, "Adjusting pathological regular-expression: "
                    "\"%s\"\n", spec.pattern);
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

        /*
         * Set the cursor position to starting time
         */
        pq_cset(pq, &clss.from );

        while(!done)
        {
                if(stats_req)
                {
                        dump_stats();
                        stats_req = 0;
                }

                if (queueSanityCheck)
                  status = pq_sequence(pq, TV_GT, &clss, tallyProds, 0);
                else
                  status = pq_sequence(pq, TV_GT, &clss, writeprod, 0);

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

                if(interval == 0)
                        break;
                pq_suspend(interval);
                        
        }

        if (queueSanityCheck)
        {
          size_t queueProdCnt;
          size_t dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy9;
          double dummy8;
          status = pq_stats(pq, &queueProdCnt, &dummy1, &dummy2, &dummy3, &dummy4, 
                   &dummy5, &dummy6, &dummy7, &dummy8, &dummy9);
          if (status) {
                uerror("pq_stats failed: %s (errno = %d)",
                       strerror(status), status);
                exit(1);
          }

          if (nprods == queueProdCnt)
          {
            unotice("pqcat queueSanityCheck: Number of products tallied consistent with value in queue");
            exit(0);
          }
          else
          {
              uerror("pqcat queueSanityCheck: Product count doesn't match");
              uerror("products tallied: %d   Value in queue: %d", nprods, queueProdCnt);              exit(1);
          }
        }
        exit(0);
        /*NOTREACHED*/
}
