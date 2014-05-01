/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqsurf.c,v 1.55.10.1.2.4 2008/04/15 16:34:10 steve Exp $ */

/* 
 * 
 */

/*
 * Need to create a queue before running this:
 * pqcreate -c -s 2M -S 13762 /usr/local/ldm/data/pqsurf.pq
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <sys/wait.h>
#include <assert.h>
#include <regex.h>
#include "ldm.h"
#include "atofeedt.h"
#include "globals.h"
#include "ldmprint.h"
#include "ulog.h"
#include "pq.h"
#include "paths.h"
#include "surface.h"
#include "RegularExpressions.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

extern int usePil;  /* 1/0 flag to signal use of AFOS like pil identifier */

#ifndef DEFAULT_INTERVAL
#define DEFAULT_INTERVAL 15
#endif

static volatile int intr = 0;
static volatile int stats_req = 0;

#ifndef DEFAULT_PATTERN
#define DEFAULT_PATTERN  "^S[AIMNP]"
#endif
#ifndef DEFAULT_FEEDTYPE
#define DEFAULT_FEEDTYPE  (IDS|DDS)
#endif

/* set in paths.h by configure */
#ifndef DEFAULT_SURF_OUTQUEUE
#define DEFAULT_SURF_OUTQUEUE "/usr/local/ldm/data/pqsurf.pq"
#endif
static char *opqfname = DEFAULT_SURF_OUTQUEUE;
static pqueue *opq = NULL;

/* set in paths.h by configure */
#ifndef DEFAULT_SURF_CONFFILE
#define DEFAULT_SURF_CONFFILE "/usr/local/ldm/etc/pqsurf.conf"
#endif

/* set in paths.h by configure */
#ifndef DEFAULT_SURF_DATADIR
#ifndef DEFAULT_DATADIR
#define DEFAULT_SURF_DATADIR "/usr/local/ldm"
#else
#define DEFAULT_SURF_DATADIR DEFAULT_DATADIR
#endif
#endif

#ifndef DEFAULT_PIPE_TIMEO
#define DEFAULT_PIPE_TIMEO 60
#endif

#ifndef DEFAULT_AGE
#define DEFAULT_AGE (1. + (double)(DEFAULT_INTERVAL)/3600.)
#endif

static pid_t act_pid;

/*
 * Set to non-root privilege if possible.
 * Do it in such a way that it is safe to fork.
 * TODO: this is duplicated from ../server/priv.c
 */
static void
endpriv(void)
{
        const uid_t euid = geteuid();
        const uid_t uid = getuid();

        /* if either euid or uid is unprivileged, use it */
        if(euid > 0)
                setuid(euid);
        else if(uid > 0)
                setuid(uid);

        /* else warn??? or set to nobody??? */
}

static pid_t
run_child(int argc, char *argv[])
{
        pid_t pid;

        if(ulogIsDebug())
        {
                char command[1024];
                char *cp = command;
                int ii = 0;
                command[0] = 0;

                while (ii < argc)
                {
                        strcpy(cp, argv[ii]);   
                        cp += strlen(argv[ii]);
                        if(++ii == argc)
                                break;
                        *cp++ = ' ';
                        *cp = 0;
                }
                udebug("exec'ing: \"%s\"", command);
        }

        pid = fork();
        if(pid == -1)
        {
                serror("run_child: fork failed");
                return pid;
        }

        if(pid == 0)
        {       /* child */

                (void)signal(SIGCHLD, SIG_DFL);
                (void)signal(SIGTERM, SIG_DFL);

                /* keep same descriptors as parent */

                /* don't let child get real privilege */
                endpriv();

                (void) execvp(argv[0], &argv[0]);
                serror("run_child: execvp: %s", argv[0]);
                _exit(127);
        }
        /* else, parent */

        return pid;
}


static int nprods = 0;
static int nsplit = 0;
static int ndups = 0;

static void
dump_stats(void)
{
        unotice("Number of products %d", nprods);
        unotice("Number of observations %d", nsplit);
        unotice("Number of dups %d", ndups);
}


/* defined in surf_split.c */
extern int surf_split(const prod_info *infop, const void *datap,
                int (*doit)(const prod_info *, const void *));

static int
doOne(const prod_info *infop, const void *datap)
{
        struct product prod;
        int status = ENOERR;

        if(ulogIsDebug())
                udebug("%s", s_prod_info(NULL, 0, infop, 1));
        
        prod.info = *infop;
        prod.data = (void *)datap; /* cast away const */

        nsplit++; /* ?? Do it here on only on success ?? */

        status = pq_insertNoSig(opq, &prod);
        if(status == ENOERR)
        {
                return status; /* Normal return */
        }

        /* else */
        if(status == PQUEUE_DUP)
        {
                ndups++;
                if(ulogIsVerbose())
                        uinfo("Product already in queue: %s",
                                s_prod_info(NULL, 0, &prod.info,
                                         ulogIsDebug()));
                return status;
        }

        /* else, error */
        uerror("pq_insert: %s\n", strerror(status));

        return status;
}


/*
 */
static int
split_prod(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *vp)
{
        size_t *nsp = (size_t *)vp;
        int ns;

        if(ulogIsVerbose())
                uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));

        ns = surf_split(infop, datap, doOne);

        nprods++;

        (void)kill(SIGCONT, act_pid);

        if(nsp != NULL && ns >= 0)
                *nsp = (size_t)ns;

        return 0;
}


static void
usage(const char *av0) /*  id string */
{
        (void)fprintf(stderr,
                "Usage: %s [options] [confilename]\t\nOptions:\n",
                av0);
        (void)fprintf(stderr,
                "\t-v           Verbose, log each match (SIGUSR2 toggles)\n");
        (void)fprintf(stderr,
                "\t-x           Debug mode\n");
        (void)fprintf(stderr,
                "\t-l logfile   Send log info to file (default uses syslogd)\n");
        (void)fprintf(stderr,
                "\t-d datadir   cd to \"datadir\" before interpreting filenames in\n");
        (void)fprintf(stderr,
                "\t             conffile (default %s)\n",
                DEFAULT_SURF_DATADIR);
        (void)fprintf(stderr,
                "\t-q queue     default \"%s\"\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-p pattern   Interested in products matching \"pattern\" (default \"%s\")\n", DEFAULT_PATTERN);
        (void)fprintf(stderr,
                "\t-f feedtype  Interested in products from feed \"feedtype\" (default %s)\n", s_feedtypet(DEFAULT_FEEDTYPE));
        (void)fprintf(stderr,
                "\t-i interval  loop, polling each \"interval\" seconds (default %d)\n", DEFAULT_INTERVAL);
        (void)fprintf(stderr,
                "\t-a age       Expire products older than \"age\" hours (default %.4f)\n", DEFAULT_AGE);
        (void)fprintf(stderr,
                "\t-t timeo     set write timeo for PIPE subprocs to \"timeo\" secs (default %d)\n", DEFAULT_PIPE_TIMEO);
        (void)fprintf(stderr,
                "\t-o offset    the oldest product we will consider is \"offset\" secs before now (default: most recent in output queue)\n");
        (void)fprintf(stderr,
                "\t-Q outQueue    default \"%s\"\n", DEFAULT_SURF_OUTQUEUE);
        (void)fprintf(stderr,
                "\t(default conffilename is %s)\n",
                DEFAULT_SURF_CONFFILE);
        exit(1);
}


static pid_t
reap_act(int options)
{
        pid_t wpid = 0;

        if (act_pid != 0) {
            int status = 0;

#ifndef NO_WAITPID
            wpid = waitpid(act_pid, &status, options);
#else
            if(options == 0)
                    wpid = wait(&status);
            /* customize here for older systems, use wait3 or whatever */
#endif
            if(wpid == -1)
            {
                    if(!(errno == ECHILD && act_pid == -1))
                    {
                             /* Only complain if relevant */
                            serror("waitpid");
                    }
                    return -1;
            }
            /* else */

            if(wpid != 0) 
            {
                    /* tag_pid_entry(wpid); */

#ifndef NO_WAITPID
                    if(WIFSTOPPED(status))
                    {
                            unotice("child %d stopped by signal %d",
                                    wpid, WSTOPSIG(status));
                    }
                    else if(WIFSIGNALED(status))
                    {
                            unotice("child %d terminated by signal %d",
                                    wpid, WTERMSIG(status));
                            /* DEBUG */
                            switch(WTERMSIG(status)) {
                            /*
                             * If a child dumped core,
                             * shut everything down.
                             */
                            case SIGQUIT:
                            case SIGILL:
                            case SIGTRAP: /* ??? */
                            case SIGABRT:
#if defined(SIGEMT)
                            case SIGEMT: /* ??? */
#endif
                            case SIGFPE: /* ??? */
                            case SIGBUS:
                            case SIGSEGV:
#if defined(SIGSYS)
                            case SIGSYS: /* ??? */
#endif
#ifdef SIGXCPU
                            case SIGXCPU:
#endif
#ifdef SIGXFSZ
                            case SIGXFSZ:
#endif
                                    act_pid = -1;
                                    exit(1);
                                    break;
                            }
                    }
                    else if(WIFEXITED(status))
                    {
                            if(WEXITSTATUS(status) != 0)
                                    unotice("child %d exited with status %d",
                                            wpid, WEXITSTATUS(status));
                            else
                                    udebug("child %d exited with status %d",
                                            wpid, WEXITSTATUS(status));
                            act_pid = -1;
                            exit(WEXITSTATUS(status));
                    }
#endif
            }
        }

        return wpid;
}


void
cleanup(void)
{
        unotice("Exiting"); 

        if(act_pid != -1)
        {
                (void)signal(SIGCHLD, SIG_IGN);
                kill(act_pid, SIGTERM);
                (void) reap_act(0);
        }

        if(opq != NULL)
        {
                off_t highwater = 0;
                size_t maxregions = 0;
                (void) pq_highwater(opq, &highwater, &maxregions);
                (void) pq_close(opq);
                opq = NULL;

                unotice("  Queue usage (bytes):%8ld",
                                        (long)highwater);
                unotice("           (nregions):%8ld",
                                        (long)maxregions);
        }

        if(pq != NULL)
        {
                (void) pq_close(pq);
                pq = NULL;
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
        case SIGCHLD :
                /* usually calls exit */
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

        /* Handle these */
#ifdef SA_RESTART       /* SVR4, 4.3+ BSD */
        /* usually, restart system calls */
        sigact.sa_flags |= SA_RESTART;
#endif
        sigact.sa_handler = signal_handler;
        (void) sigaction(SIGTERM, &sigact, NULL);
        (void) sigaction(SIGUSR1, &sigact, NULL);
        (void) sigaction(SIGUSR2, &sigact, NULL);
        (void) sigaction(SIGCHLD, &sigact, NULL);

        /* Don't restart after interrupt */
        sigact.sa_flags = 0;
#ifdef SA_INTERRUPT     /* SunOS 4.x */
        sigact.sa_flags |= SA_INTERRUPT;
#endif
        (void) sigaction(SIGINT, &sigact, NULL);
}


static int
expire(pqueue *epq, const unsigned interval, const double age)
{
        int status = ENOERR;
        static timestampt now;
        static prod_class eclss;
        static prod_spec spec;
        timestampt ts;
        timestampt cursor;
        double diff = 0.;
        double max_latency = 0.;
        size_t nr;

        if(eclss.psa.psa_val == 0)
        {
                /* first time */
                eclss.from = TS_ZERO;
                eclss.psa.psa_len = 1;
                eclss.psa.psa_val = &spec;
                spec.feedtype = ANY;
                spec.pattern = ".*";
                regcomp(&spec.rgx, spec.pattern, REG_EXTENDED|REG_NOSUB);
        }

        (void) set_timestamp(&now);
        if(d_diff_timestamp(&now, &eclss.to) < interval + age)
        {
                /* only run this routine every interval seconds */
                udebug("not yet");
                return ENOERR;
        }
        /* else */
        eclss.to = now;
        eclss.to.tv_sec -= age;

        if(ulogIsDebug())
        {
                char cp[64];
                sprint_timestampt(cp, sizeof(cp), &eclss.to);
                udebug("to %s", cp);
        }

        pq_cset(epq, &TS_ZERO);

        while(exitIfDone(0) && !stats_req)
        {
                nr = 0;
                status = pq_seqdel(epq, TV_GT, &eclss, 0, &nr, &ts);

                switch(status) {
                case ENOERR:
                        pq_ctimestamp(epq, &cursor);
                        diff = d_diff_timestamp(&cursor, &ts);
                        if(diff > max_latency)
                        {
                                max_latency = diff;
                                udebug("max_latency %.3f", max_latency);
                        }
                        
                        if(nr == 0)
                        {
                                diff = d_diff_timestamp(&cursor, &eclss.to);
                                udebug("diff %.3f", diff);
                                if(diff > interval + max_latency)
                                {
                                        udebug("heuristic depth break");
                                        break;
                                }

                        }
                        continue; /* N.B., other cases break and return */
                case PQUEUE_END:
                        udebug("expire: End of Queue");
                        break;
                case EAGAIN:
                case EACCES:
                        udebug("Hit a lock");
                        break;
#if defined(EDEADLOCK) && EDEADLOCK != EDEADLK
                case EDEADLOCK:
#endif
                case EDEADLK:
                        uerror("%s", strerror(status));
                        break;
                default:
                        uerror("pq_seqdel failed: %s (errno = %d)",
                                strerror(status), status);
                        break;
                }
                break;
        }
        return status;
}


int main(int ac, char *av[])
{
        const char *progname = ubasename(av[0]);
        char *logfname;
        prod_class_t clss;
        prod_spec spec;
        int status = 0;
        unsigned interval = DEFAULT_INTERVAL;
        int logoptions = (LOG_CONS|LOG_PID);
        double age = DEFAULT_AGE;
        /* these are containers for the pqact args */
        char *argv[16];
        int argc = 0;
        int toffset = TOFFSET_NONE;

        logfname = "";

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
        spec.pattern = DEFAULT_PATTERN;


        memset(argv, 0, sizeof(argv));
        argv[0] = "pqact";
        argc++;
        
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
        char *conffilename = DEFAULT_SURF_CONFFILE;
        char *datadir = DEFAULT_SURF_DATADIR;

        usePil = 1;
        opterr = 1;

        while ((ch = getopt(ac, av, "vxl:d:f:p:q:Q:o:i:a:t:")) != EOF)
                switch (ch) {
                case 'v':
                        argv[argc++] = "-v";
                        logmask |= LOG_MASK(LOG_INFO);
                        break;
                case 'x':
                        argv[argc++] = "-x";
                        logmask |= LOG_MASK(LOG_DEBUG);
                        break;
                case 'l':
                        argv[argc++] = "-l";
                        argv[argc++] = optarg;
                        logfname = optarg;
                        break;
                case 'd':
                        datadir = optarg;
                        break;
                case 'f':
                        fterr = strfeedtypet(optarg, &spec.feedtype);
                        if(fterr != FEEDTYPE_OK)
                        {
                                fprintf(stderr, "%s: %s: \"%s\"\n",
                                        av[0], strfeederr(fterr), optarg);
                                usage(progname);        
                        }
                        argv[argc++] = "-f";
                        argv[argc++] = optarg;
                        break;
                case 'p':
                        spec.pattern = optarg;
                        /* compiled below */
                        break;
                case 'q':
                        pqfname = optarg;
                        break;
                case 'Q':
                        opqfname = optarg;
                        break;
                case 'o':
                        toffset = atoi(optarg);
                        if(toffset == 0 && *optarg != '0')
                        {
                                fprintf(stderr, "%s: invalid offset %s\n",
                                         av[0], optarg);
                                usage(av[0]);   
                        }
                        argv[argc++] = "-o";
                        argv[argc++] = optarg;
                        break;
                case 'i':
                        interval = atoi(optarg);
                        if(interval == 0 && *optarg != '0')
                        {
                                fprintf(stderr, "%s: invalid interval \"%s\"\n",
                                        av[0], optarg);
                                usage(av[0]);
                        }
                        /* N.B. -i just used for input queue. */
                        break;
                case 'a':
                        age = atof(optarg);
                        if(age < 0.)
                        {
                            (void) fprintf(stderr,
                                        "age (%s) must be non negative\n",
                                        optarg);
                                usage(av[0]);   
                        }
                        break;
                case 't':
                        /* pipe_timeo */
                        argv[argc++] = "-t";
                        argv[argc++] = optarg;
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

        if(ac - optind == 1)
                conffilename = av[optind];

        argv[argc++] = "-d";
        argv[argc++] = datadir;
        argv[argc++] = "-q";
        argv[argc++] = opqfname;
        argv[argc++] = conffilename;

        age *= 3600.;

        }

        if(toffset != TOFFSET_NONE)
        {
                clss.from.tv_sec -= toffset;
        }
        else
        {
                clss.from.tv_sec -= (age - interval);
        }


        /*
         * Set up error logging.
         * N.B. log ident is the remote
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
         * Open the Output product que
         */
        status = pq_open(opqfname, PQ_DEFAULT, &opq);
        if(status)
        {
                if (PQ_CORRUPT == status) {
                    uerror("The output product-queue \"%s\" is inconsistent\n",
                            opqfname);
                }
                else {
                    uerror("pq_open failed: %s: %s\n",
                            opqfname, strerror(status));
                }
                exit(1);
        }


        act_pid = run_child(argc, argv);
        if(act_pid == (pid_t)-1)
                exit(1);

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
                /* Jump to the end of the queue */
                timestampt sav;
                sav = clss.from;
                clss.from = TS_ZERO;
                (void) pq_last(pq, &clss, NULL);
                clss.from = sav;
        }
        else
        {
                pq_cset(pq, &clss.from);
        }

        if(ulogIsVerbose())
        {
                char buf[1984];
                uinfo("%s",
                         s_prod_class(buf, sizeof(buf), &clss));
        }

        while(exitIfDone(0))
        {
                if(stats_req)
                {
                        dump_stats();
                        stats_req = 0;
                }

                status = pq_sequence(pq, TV_GT, &clss, split_prod, NULL);

                switch(status) {
                case 0: /* no error */
                        continue; /* N.B., other cases sleep */
                case PQUEUE_END:
                        udebug("surf: End of Queue");
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
                {
                        break;
                }


                (void) expire(opq, interval, age);

                pq_suspend(interval);

                (void) reap_act(WNOHANG);
        }

        /*
         * TODO: how can we determine that pqact has finished
         *       the work in opq?
         */
        sleep(5);

        exit(0);
}
