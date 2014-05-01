/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: notifyme.c,v 1.63.12.8 2008/04/15 16:34:07 steve Exp $ */

/* 
 * get notification
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <sys/socket.h>
#include <errno.h>
#include <regex.h>
#include "ldm5.h"
#include "globals.h"
#include "ldmprint.h"
#include "atofeedt.h"
#include "ulog.h"
#include "inetutil.h"
#include "ldm5_clnt.h"
#include "RegularExpressions.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

#ifndef DEFAULT_REMOTE
#define DEFAULT_REMOTE "localhost"
#endif
#ifndef DEFAULT_TIMEO
#define DEFAULT_TIMEO  25
#endif
#ifndef DEFAULT_TOTALTIMEO
#define DEFAULT_TOTALTIMEO  (12*DEFAULT_TIMEO)
#endif
#ifndef DEFAULT_FEEDTYPE
#define DEFAULT_FEEDTYPE ANY
#endif
#ifndef DEFAULT_PATTERN
#define DEFAULT_PATTERN ".*"
#endif


static const char *remote = DEFAULT_REMOTE; /* hostname of data remote */
static ldm_replyt reply = { OK };


/*
 * Called at exit.
 * This callback routine registered by atexit().
 */
static void
cleanup(void)
{
        unotice("exiting");

        /* TODO: sign off */

        (void) closeulog();
}


/*
 * Called upon receipt of signals.
 * This callback routine registered in set_sigactions() .
 */
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
        case SIGHUP :
                return;
        case SIGINT :
                /*FALLTHROUGH*/
        case SIGTERM :
                done = !0;
                return;
        case SIGUSR1 :
                return;
        case SIGUSR2 :
                toggleulogpri(LOG_INFO);
                return;
        case SIGPIPE :
                return;
        }
}


static void
set_sigactions(void)
{
#ifndef NO_POSIXSIGNALS
        struct sigaction sigact;

        sigact.sa_handler = signal_handler;
        sigemptyset(&sigact.sa_mask);
        sigact.sa_flags = 0;

        (void) sigaction(SIGHUP, &sigact, NULL);
        (void) sigaction(SIGINT, &sigact, NULL);
        (void) sigaction(SIGTERM, &sigact, NULL);
        (void) sigaction(SIGUSR1, &sigact, NULL);
        (void) sigaction(SIGUSR2, &sigact, NULL);
        (void) sigaction(SIGPIPE, &sigact, NULL);
#else
        (void) signal(SIGHUP, signal_handler);
        (void) signal(SIGINT, signal_handler);
        (void) signal(SIGTERM, signal_handler);
        (void) signal(SIGUSR1, signal_handler);
        (void) signal(SIGUSR2, signal_handler);
        (void) signal(SIGPIPE, signal_handler);
#endif
}


static void
usage(
        char *av0  /*  id string */
)
{
        (void)fprintf(stderr,
                "Usage: %s [options] \t\nOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-v             Verbose, report each notification\n");
        (void)fprintf(stderr,
                "\t-x             Debug mode\n");
        (void)fprintf(stderr,
                "\t-l logfile     Send log info to file (default uses syslogd)\n");
        (void)fprintf(stderr,
                "\t-h remote      Have \"remote\" send us data (default \"%s\")\n",
                DEFAULT_REMOTE);
        (void)fprintf(stderr,
                "\t-f feedtype    Interested in products from feed \"feedtype\" (default %s)\n", s_feedtypet(DEFAULT_FEEDTYPE));
        (void)fprintf(stderr,
                "\t-p pattern     Interested in products matching \"pattern\" (default \"%s\")\n", DEFAULT_PATTERN);
        (void)fprintf(stderr,
                "\t-o offset      Set the \"from\" time offset secs before now\n");
        (void)fprintf(stderr,
                "\t-t timeout     Set RPC timeout to \"timeout\" seconds (default %d)\n",
                        DEFAULT_TIMEO);
        (void)fprintf(stderr,
                "\t-T TotalTimeo  Give up after this many secs (default %d)\n",
                        DEFAULT_TOTALTIMEO);
        exit(1);
}


static prod_class clss;

/*
 * The RPC dispatch routine for this program.
 * Registered as a callback by svc_register() below.
 * Note that only NULLPROC and NOTIFICATION rpc procs are
 * handled by this program.
 */
static void
notifymeprog_5(struct svc_req *rqstp, SVCXPRT *transp)
{
        prod_info notice;

        switch (rqstp->rq_proc) {

        case NULLPROC:
                svc_sendreply(transp, (xdrproc_t)xdr_void, (caddr_t)NULL);
                return;

        case NOTIFICATION:
                (void) memset((char*)&notice, 0, sizeof(notice));
                if (!svc_getargs(transp, (xdrproc_t)xdr_prod_info,
                    (caddr_t)&notice))
                {
                        svcerr_decode(transp);
                        return;
                }

                (void)exitIfDone(0);

                /*
                 * Update the request filter with the timestamp
                 * we just recieved.
                 * N.B.: There can still be duplicates after
                 * a reconnect.
                 */
                clss.from = notice.arrival;
                timestamp_incr(&clss.from);


                /* 
                 * your code here, example just logs it 
                 */
                uinfo("%s", s_prod_info(NULL, 0, &notice, ulogIsDebug()));


                if(!svc_sendreply(transp, (xdrproc_t)xdr_ldm_replyt,
                    (caddr_t) &reply))
                {
                        svcerr_systemerr(transp);
                }

                (void)exitIfDone(0);

                if(!svc_freeargs(transp, xdr_prod_info, (caddr_t) &notice)) {
                        uerror("unable to free arguments");
                        exit(1);
                }


        default:
                svcerr_noproc(transp);
                return;
        }
}


int main(int ac, char *av[])
{
        char *logfname = 0;
        unsigned timeo = DEFAULT_TIMEO; 
        unsigned interval = DEFAULT_TIMEO; 
        unsigned TotalTimeo = DEFAULT_TOTALTIMEO;
        prod_spec spec;
        int status;
        prod_class_t *clssp;
        unsigned remotePort = LDM_PORT;

        if(set_timestamp(&clss.from) != 0)
        {
                fprintf(stderr, "Couldn't set timestamp\n");
                exit(1);
        }
        clss.to = TS_ENDT;
        clss.psa.psa_len = 1;
        clss.psa.psa_val = &spec;
        spec.feedtype = DEFAULT_FEEDTYPE;
        spec.pattern = DEFAULT_PATTERN;

        { /* Begin getopt block */
        extern int optind;
        extern int opterr;
        extern char *optarg;
        int ch;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE));
        int fterr;

        opterr = 1;

        while ((ch = getopt(ac, av, "vxl:f:o:t:h:P:p:T:")) != EOF)
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
                case 'p':
                        spec.pattern = optarg;
                        /* compiled below */
                        break;
                case 'f':
                        fterr = strfeedtypet(optarg, &spec.feedtype);
                        if(fterr != FEEDTYPE_OK)
                        {
                                fprintf(stderr, "Bad feedtype \"%s\", %s\n",
                                        optarg, strfeederr(fterr));
                                usage(av[0]);   
                        }
                        break;
                case 'o':
                        clss.from.tv_sec -= atoi(optarg);
                        break;
                case 'T':
                        TotalTimeo = atoi(optarg);
                        if(TotalTimeo == 0)
                        {
                                fprintf(stderr, "%s: invalid TotalTimeo %s", av[0], optarg);
                                usage(av[0]);   
                        }
                        break;
                case 't':
                        timeo = (unsigned)atoi(optarg);
                        if(timeo == 0 || timeo > 32767)
                        {
                                fprintf(stderr, "%s: invalid timeout %s", av[0], optarg);
                                usage(av[0]);   
                        }
                        break;
                case '?':
                        usage(av[0]);
                        break;
                }

        if(ac - optind > 0)
                usage(av[0]);

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

        (void) setulogmask(logmask);

        if(TotalTimeo < timeo)
        {
                fprintf(stderr, "TotalTimeo %u < timeo %u\n",
                         TotalTimeo, timeo);
                usage(av[0]);
        }

        } /* End getopt block */

        /*
         * initialize logger
         */
        (void) openulog(ubasename(av[0]),
                (LOG_CONS|LOG_PID), LOG_LDM, logfname);
        unotice("Starting Up: %s: %s",
                        remote,
                        s_prod_class(NULL, 0, &clss));

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
         * Try forever.
         */
        while (exitIfDone(0))
        {
                clssp = &clss;
                status = forn5(NOTIFYME, remote, &clssp,
                                timeo, TotalTimeo, notifymeprog_5);

                (void)exitIfDone(0);

                switch(status) {
                        /* problems with remote, retry */       
                case ECONNABORTED:
                case ECONNRESET:
                case ETIMEDOUT:
                case ECONNREFUSED:
                        sleep(interval);
                        break;
                case 0:
                        /* assert(done); */
                        break;
                default:
                        /* some wierd error */
                        done = 1;
                        exit(1);
                }
        }

        exit(0);
        /*NOTREACHED*/
}
