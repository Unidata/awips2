/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqsend.c,v 1.70.10.1.2.4 2008/04/15 16:34:10 steve Exp $ */

/* 
 * ldm client to ship files
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <regex.h>
#include "ldm.h"
#include "atofeedt.h"
#include "ldmprint.h"
#include "globals.h"
#include "inetutil.h"
#include "ulog.h"
#include "paths.h"
#include "pq.h"
#include "h_clnt.h"
#include "ldm5_clnt.h"
#include "RegularExpressions.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

#ifdef DEBUG_CSBD
#undef DBUFMAX
#define DBUFMAX 4096
#endif

#ifndef DEFAULT_TIMEOUT
#define DEFAULT_TIMEOUT 25
#endif
#ifndef DEFAULT_INTERVAL
#define DEFAULT_INTERVAL 15
#endif
#ifndef DEFAULT_TOTALTIMEOUT
#define DEFAULT_TOTALTIMEOUT 3600 /* give up after an hour */
#endif
#ifndef DEFAULT_FEEDTYPE
#define DEFAULT_FEEDTYPE ANY
#endif

static volatile int stats_req = 0;
static int TotalTimeo = DEFAULT_TOTALTIMEOUT;

static h_clnt hc;
        /* N.B.: assumes hostname doesn't change during program execution :-) */
static const char *remote = NULL; /* hostname of data remote */

struct sendstats {
        timestampt starttime;   /* stats start time */
        int nprods;     /* number of products sent */
        int nconnects;  /* number of connects */
        int ndisco;     /* number of disconnects */
        double downtime;        /* accumulated disconnect time */
        timestampt last_disco;  /* time of last disconnect */
        double last_downtime;   /* length of last disconnect */
#define INIT_MIN_LATENCY 2147483647.
        double min_latency;     /* min(shipped_from_here - info.arrival) */
        double max_latency;     /* max(shipped_from_here - info.arrival) */
};
typedef struct sendstats sendstats;

static sendstats stats;

prod_class clss;
static prod_class *clssp = NULL; /* set in main, possibly updated in shipprod() */


static void
update_last_downtime(const timestampt *nowp)
{
        /* update last_downtime */
        stats.last_downtime = d_diff_timestamp(nowp, &stats.last_disco);
        udebug("last_downtime %10.3f", stats.last_downtime);
}


static void
dump_stats(const sendstats *stp)
{
        char cp[24];
        sprint_timestampt(cp, sizeof(cp), &stp->starttime);
        unotice("> Up since:          %s", cp);

        if(stp->nconnects > 0)
        {
                sprint_timestampt(cp, sizeof(cp), &stp->last_disco);
                if(stp->last_downtime != 0.)
                        unotice(">  last disconnect:  %s for %10.3f seconds",
                                cp, stp->last_downtime);
                else
                        unotice(">  last disconnect:  %s", cp);
                if(stp->nprods)
                {
        unotice(">     nprods min_latency max_latency"); 
        unotice("> %10d  %10.3f  %10.3f", 
                stp->nprods, stp->min_latency, stp->max_latency);
                }
                else
                {
        unotice(">     nprods"); 
        unotice("> %10d", stp->nprods);
                }
        unotice(">  nconnects      ndisco  secs_disco"); 
        unotice("> %10d  %10d  %10.3f", 
                stp->nconnects, stp->ndisco, stp->downtime);
        }
        else
        {
                unotice("> Never connected");
        }
}


static void
usage(const char *av0) /*  id string */
{
        (void)fprintf(stderr,
                "Usage: %s [options]\n\tOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-v           Verbose, tell me about each product\n");
        (void)fprintf(stderr,
                "\t-l logfile   log to a file rather than stderr\n");
        (void)fprintf(stderr,
                "\t-h remote    remote service host, defaults to \"localhost\"\n");
        (void)fprintf(stderr,
                "\t-f feedtype  assert your feed type as \"feedtype\", defaults to \"%s\"\n", s_feedtypet(DEFAULT_FEEDTYPE));
        (void)fprintf(stderr,
                "\t-p pattern   Offer products matching \"pattern\" (default \".*\")\n");
        (void)fprintf(stderr,
                "\t             (Pattern is usually overridden by downstream subset)\n");
        (void)fprintf(stderr,
                "\t-q queue     default \"%s\"\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-o offset    set the \"from\" time offset secs before now\n");
        (void)fprintf(stderr,
                "\t-t timeout   set RPC timeout to \"timeout\" seconds (default %d)\n",
                        DEFAULT_TIMEOUT);
        (void)fprintf(stderr,
                "\t-i interval  Poll queue after \"interval\" secs (default %d)\n",
                DEFAULT_INTERVAL);
        (void)fprintf(stderr,
                "\t-T TotalTimeo Give up on remote after \"TotalTimeo\" secs (default %d)\n",
                DEFAULT_TOTALTIMEOUT);
        exit(1);
}


static void
cleanup(void)
{
        unotice("Exiting"); 

        if(done)
                close_h_clnt(&hc);

        if(pq && done)  
                (void)pq_close(pq);

        if(hc.state < H_CLNTED)
                stats.downtime += stats.last_downtime;

        dump_stats(&stats);

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
                exit(1);
        case SIGTERM :
                done = 1;       
                return;
        case SIGUSR1 :
                stats_req = 1;
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

        (void) sigemptyset(&sigact.sa_mask);
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


/*
 * Initialize hcp if necessary,
 * send the HIYA,
 * initialize clss with the response.
 */
static remote_state
sign_on(h_clnt *hcp,
        const char *remote,
        u_long  prog,
        u_long  vers,
        prod_class **clsspp,
        unsigned rpctimeo,
        int TotalTimeo,
        unsigned interval
)
{
        enum clnt_stat rpc_stat;
        ldm_replyt reply;
        int remaining = TotalTimeo;
        

#define MAXREFERRALS 8
        int loopcounter = 0; /* maximum levels of indirection */

        if(hcp->state < NAMED || !hcp->remote
                        || strcmp(hcp->remote, remote) != 0 )
        {
                close_h_clnt(hcp);
                (void) init_h_clnt(hcp,
                        remote, prog, vers, LDM_TCP);
        }

        set_h_timeout(hcp, rpctimeo);

        if(loopcounter++ > MAXREFERRALS)
        {
                uerror("too many levels (%d) of indirection", 
                        loopcounter -1);
                goto unwind_clnt;
        }

retry:  
        rpc_stat = hiya5(hcp, *clsspp, USE_H_TIMEO, &reply);

        (void)exitIfDone(1);

        switch (rpc_stat) {
        case RPC_TIMEDOUT:
        case RPC_PROGUNAVAIL:
        case RPC_PMAPFAILURE:
        case RPC_PROGNOTREGISTERED:
                if(remaining == TotalTimeo) /* only verbose 1st time */
                {
                        uerror("sign_on(%s): %s",
                                remote, s_hclnt_sperrno(&hc));
                }
                else
                        udebug("sign_on(%s): %s",
                                remote, s_hclnt_sperrno(&hc));
                remaining -= hc.elapsed.tv_sec;
                if(hc.elapsed.tv_usec > 500000)
                        remaining--;
                if(exitIfDone(1) && remaining > 0 &&
                    (unsigned)remaining > interval)
                {
                        (void) sleep(interval);
                        remaining -= interval;
                        (void)exitIfDone(1);
                        goto retry;
                }
                /* else */
                uerror("sign_on(%s): Giving Up after %d seconds",
                        remote, TotalTimeo - remaining);
                goto unwind_clnt;

        case RPC_SUCCESS:
                /* call succeeded, can use the reply */
                udebug("sign_on(%s) returns %s",
                        remote, s_ldm_errt(reply.code));
                break;

        case RPC_AUTHERROR:
        case RPC_UNKNOWNHOST:
        default:
                uerror("sign_on(%s): %d: %s",
                        remote, rpc_stat,
                                 s_hclnt_sperrno(&hc));
                done = 1;
                exit(1);
                /*NOTREACHED*/
        }

        switch (reply.code) {
                case OK:
                        break;
                case SHUTTING_DOWN:
                        uerror("%s is shutting down", remote);
                        rpc_stat = RPC_PROGUNAVAIL;
                        goto unwind_clnt;
                case DONT_SEND:
                case RESTART:
                case REDIRECT: /* TODO */
                default:
                        uerror("sign_on(%s): unexpected reply type %s",
                                 remote,
                                 s_ldm_errt(reply.code));
                        rpc_stat = RPC_CANTRECV;
                        goto unwind_clnt;
                case RECLASS:
                        *clsspp = reply.ldm_replyt_u.newclssp;
                        clss_regcomp(*clsspp);
                        /*
                         * N.B. This program trusts downstream
                         *  not ask for something that wasn't offered.
                         * N.B. Uses the downstream patterns.
                         */
                        unotice("sign_on(%s): reclass: %s",
                                remote,
                                s_prod_class(NULL, 0, *clsspp));
                        break;
        }
        

        return hcp->state;

unwind_clnt:
        close_h_clnt(hcp);
        
        return hcp->state;
}


#define CSBD_CHECK_REMAINING(remaining) \
        { \
                remaining -= hcp->elapsed.tv_sec; \
                if(hcp->elapsed.tv_usec > 500000) \
                        remaining -= 1; \
                if(remaining < 0) \
                        return RPC_TIMEDOUT; \
        }

/*
 * Ship a product using COMINGSOON/BLKDATA
 */
static enum clnt_stat
csbd(h_clnt *hcp, const prod_info *infop, const void *datap,
                unsigned rpctimeo, int remaining, ldm_replyt *replyp)
{
        enum clnt_stat rpc_stat;
        datapkt pkt;
        const char *ptr;
        u_int unsent;

        /* TODO: mtu */
        rpc_stat = comingsoon5(hcp, infop, DBUFMAX, rpctimeo, replyp);
        if(rpc_stat != RPC_SUCCESS)
                return rpc_stat;
        if(replyp->code != OK)
                return rpc_stat;
        /* else */
        CSBD_CHECK_REMAINING(remaining);

        pkt.signaturep = (signaturet *)&infop->signature; /* cast away const */
        for(unsent = infop->sz, ptr = datap, pkt.pktnum = 0;
                        unsent > 0;
                        unsent -= pkt.data.dbuf_len,
                                 ptr += pkt.data.dbuf_len, pkt.pktnum++)
        {
                pkt.data.dbuf_len = unsent < DBUFMAX ? unsent : DBUFMAX;
                pkt.data.dbuf_val = (char *)ptr; /* cast away const */
                rpc_stat = blkdata5(hcp, &pkt, rpctimeo, replyp);
                if(rpc_stat != RPC_SUCCESS)
                        return rpc_stat;
                CSBD_CHECK_REMAINING(remaining);
        }
        
        return rpc_stat;
}


/*
 */
/*ARGSUSED*/
static int
shipprod(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *hcp)
{
        enum clnt_stat rpc_stat;
        ldm_replyt reply;
        timestampt now;
        double latency;
        int remaining;

        set_timestamp(&now);
        latency = d_diff_timestamp(&now, &infop->arrival);
        remaining = TotalTimeo - (int)latency;

        if(infop->sz > DBUFMAX)
                rpc_stat = csbd(hcp, infop, datap, USE_H_TIMEO,
                         remaining, &reply);
        else
                rpc_stat = xhereis5(hcp, xprod, size, USE_H_TIMEO, &reply);     
                

        if(rpc_stat != RPC_SUCCESS)
        {
                uerror("ship: %s: %s",
                        clnt_sperrno(rpc_stat),
                        s_prod_info(NULL, 0, infop, ulogIsDebug()));
                return EIO;
        }
        /* else */
        switch (reply.code) {
                case OK:
                        break;
                case SHUTTING_DOWN:
                        uerror("%s is shutting down", remote);
                        rpc_stat = RPC_PROGUNAVAIL;
                        return EIO;
                case RECLASS:
                {
                        prod_class_t *want = reply.ldm_replyt_u.newclssp;
                        unotice("RECLASS: %s", s_prod_class(NULL, 0, want));
        
                        /* Paranoia. */
                        if(want == NULL
                                        || want->psa.psa_len == 0
                                        || want->psa.psa_val == NULL)
                        {
                                uerror("Bizarre RECLASS reply");
                                return EIO;
                        }
        
                        clss_regcomp(want);
                        /*
                         * N.B. This program trusts downstream
                         *  not ask for something that wasn't offered.
                         * N.B. Uses the downstream patterns.
                         */
                        if(clssp != &clss)
                                free_prod_class(clssp);
                        clssp = want;
        
                        if(!prodInClass(clssp, infop))
                                return ENOERR;  /* He doesn't want this one */
                        /* else, here we go */
                        break;
                }
                case RESTART:
                case REDIRECT: /* TODO */
                default:
                        uerror("csbd(%s): unexpected reply type %s",
                                 remote,
                                 s_ldm_errt(reply.code));
                        rpc_stat = RPC_CANTRECV;
                        return EIO;
                case DONT_SEND:
                        if(ulogIsVerbose())
                                uinfo(" dup: %s",
                                        s_prod_info(NULL, 0, infop,
                                                ulogIsDebug()));
                        return 0;
        }
        
        if(ulogIsVerbose())
                uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));
        stats.nprods++;
        if(latency < stats.min_latency)
                stats.min_latency = latency;
        if(latency > stats.max_latency)
                stats.max_latency = latency;
        return 0;
}


int main(int ac, char *av[])
{
        const char *progname = ubasename(av[0]);
        char *logfname;
        prod_spec spec;
        int status = 0;
        int interval = DEFAULT_INTERVAL;
        timestampt now;
        timestampt toffset = TS_NONE;

        logfname = "";
        remote = "localhost";

        /* init stats */
        if(set_timestamp(&stats.starttime) != ENOERR)
        {
                int errnum = errno;
                (void) fprintf(stderr, "Couldn't set timestamp: %s", 
                        strerror(errnum));
                exit(1);
        }
        stats.last_disco = stats.starttime;
        stats.min_latency = INIT_MIN_LATENCY;
        stats.last_downtime = 0.;

        clss.to = TS_ENDT;
        clss.psa.psa_len = 1;
        clss.psa.psa_val = &spec;
        spec.feedtype = ANY;
        spec.pattern = ".*";
        
        /* if called as something other than "pqsend",
                use it as the remote */
        if(strcmp(progname, "pqsend") != 0)
                remote = progname;

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
        int fterr;

        opterr = 1;

        while ((ch = getopt(ac, av, "vxl:h:f:p:q:o:t:T:i:")) != EOF)
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
                case 'q':
                        pqfname = optarg;
                        break;
                case 'p':
                        spec.pattern = optarg;
                        /* compiled below */
                        break;
                case 'f':
                        fterr = strfeedtypet(optarg, &spec.feedtype) ;
                        if(fterr != FEEDTYPE_OK)
                        {
                                (void) fprintf(stderr,
                                        "Bad feedtype \"%s\", %s\n",
                                        optarg, strfeederr(fterr)) ;
                                usage(progname);        
                        }
                        break;
                case 'o':
                        toffset.tv_sec = atoi(optarg);
                        if(toffset.tv_sec == 0 && *optarg != '0')
                        {
                                (void) fprintf(stderr,
                                         "%s: invalid offset %s\n",
                                         av[0], optarg);
                                usage(av[0]);   
                        }
                        toffset.tv_usec = 0;
                        break;
                case 't':
                        rpctimeo = (unsigned) atoi(optarg);
                        if(rpctimeo == 0)
                        {
                                (void) fprintf(stderr,
                                        "%s: invalid timeout \"%s\"\n",
                                        progname, optarg);
                                usage(progname);        
                        }
                        break;
                case 'i':
                        interval = atoi(optarg);
                        if(interval == 0 && *optarg != '0')
                        {
                                (void) fprintf(stderr,
                                        "%s: invalid interval %s",
                                        progname, optarg);
                                usage(progname);
                        }
                        break;
                case 'T':
                        TotalTimeo = atoi(optarg);
                        if(TotalTimeo == 0)
                        {
                                (void) fprintf(stderr,
                                        "%s: invalid Total timeout \"%s\"\n",
                                        progname, optarg);
                                usage(progname);        
                        }
                        break;
                case '?':
                        usage(progname);
                        break;
                }

        if((2 * rpctimeo) >= TotalTimeo)
        {
                (void) fprintf(stderr,
"%s: Total timeout %d too small for rpc timeout %d\n",
                        progname, TotalTimeo, (int)rpctimeo);
                usage(progname);
        }

        if(toffset.tv_sec > TotalTimeo)
        {
                (void) fprintf(stderr,
"%s: Total timeout %d too small for offset %d\n",
                        progname, TotalTimeo, (int)toffset.tv_sec);
                usage(progname);
        }

        if(tvIsNone(toffset))
        {
                toffset.tv_sec = TotalTimeo;
                toffset.tv_usec = 0;
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

        }

        /*
         * Set up error logging.
         * N.B. log ident is the remote
         */
        (void) openulog(remote,
                (LOG_CONS|LOG_PID), LOG_LDM, logfname);
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

        while(exitIfDone(1))
        {
                if(stats_req)
                {
                        dump_stats(&stats);
                        stats_req = 0;
                }

                (void)set_timestamp(&now);
                if(hc.state < H_CLNTED)
                {
                        update_last_downtime(&now);

                        if(clssp != &clss)
                        {
                                free_prod_class(clssp);
                                clssp = &clss;
                        }
        
                        /* Offer what we can */
                        clss.from = diff_timestamp(&now, &toffset);

                        if(sign_on(&hc, remote, LDMPROG, FIVE, &clssp,
                                        rpctimeo, TotalTimeo, interval)
                                         >= H_CLNTED)
                        {
                                (void)set_timestamp(&now);
                                stats.nconnects++;
                                update_last_downtime(&now);
                                stats.downtime += stats.last_downtime;

                                /* Check Totaltimeo */
                                if(d_diff_timestamp(&now, &clssp->from)
                                                 > TotalTimeo)
                                {
                                        clssp->from = now;
                                        clssp->from.tv_sec -= TotalTimeo;
                                        pq_cset(pq, &clssp->from);
                                }
                                pq_cset(pq, &clssp->from);
                        }
                        continue;
                }
                else
                {
                        status = pq_sequence(pq, TV_GT, clssp, shipprod, &hc);

                        (void)exitIfDone(1);

                        switch(status) {
                        case 0: /* no error */
                                continue; /* N.B., all other cases sleep */
                        case PQUEUE_END:
                                udebug("End of Queue");
                                break;
                        case EAGAIN:
                        case EACCES:
                                udebug("Hit a lock");
                                break;
                        case EIO:
                                stats.last_disco = now; /* actually, then */
                                (void)set_timestamp(&now);
                                udebug("RPC error");
                                stats.ndisco++;
                                update_last_downtime(&now);
                                break;
                        default:
                                uerror("pq_sequence failed: %s (errno = %d)",
                                        strerror(status), status);
                                exit(1);
                                break;
                        }

                }
                if(interval == 0)
                {
                        done = 1;
                        break;
                }
                pq_suspend(interval);
        }

        exit(0);
}
