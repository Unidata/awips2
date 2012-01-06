/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqact.c,v 1.40.10.6.2.14 2008/09/04 20:09:52 steve Exp $ */

/* 
 * ldm server mainline program module
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <regex.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#ifndef NO_WAITPID
#include <sys/wait.h>
#endif 
#include "ldm.h"
#include "error.h"
#include "globals.h"
#include "paths.h" /* built by configure from paths.h.in */
#include "atofeedt.h"
#include "pq.h"
#include "palt.h"
#include "ldmprint.h"
#include "filel.h" /* pipe_timeo */
#include "state.h"
#include "timestamp.h"
#include "ulog.h"
#include "log.h"
#include "RegularExpressions.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

static volatile int     hupped = 0;
static char*            conffilename = 0;
static int              shmid = -1;
static int              semid = -1;
static key_t            key;
static key_t            semkey;
timestampt              oldestCursor;
timestampt              currentCursor;
int                     currentCursorSet = 0;

#ifndef DEFAULT_INTERVAL
#define DEFAULT_INTERVAL 15
#endif
#ifndef DEFAULT_FEEDTYPE
#define DEFAULT_FEEDTYPE ANY
#endif
#ifndef DEFAULT_PATTERN
#define DEFAULT_PATTERN ".*"
#endif

/*
 * Timeout used for PIPE actions,
 * referenced in filel.c
 */
#ifndef DEFAULT_PIPE_TIMEO
#define DEFAULT_PIPE_TIMEO 60
#endif /* !DEFAULT_PIPE_TIMEO */
int pipe_timeo = DEFAULT_PIPE_TIMEO;


/*
 * called at exit
 */
static void
cleanup(void)
{
    unotice("Exiting");

    if (done) {
        /*
         * We are not in the interrupt context, so these can
         * be performed safely.
         */
        fl_close_all();

        if (pq)
            (void)pq_close(pq);

        if (currentCursorSet) {
            timestampt  now;

            (void)set_timestamp(&now);
            unotice("Behind by %g s", d_diff_timestamp(&now, &currentCursor));

            if (stateWrite(&currentCursor) < 0) {
                log_add("Couldn't save insertion-time of last processed "
                    "data-product");
                log_log(LOG_ERR);
            }
        }

        while (reap(-1, WNOHANG) > 0)
            /*EMPTY*/;
    }

    if(shmid != -1) {
        unotice("Deleting shared segment.");
        shmctl(shmid, IPC_RMID, NULL);
    }
    if(semid != -1) {
        semctl(semid, 0, IPC_RMID);
    }

    (void)closeulog();
}


/*
 * called upon receipt of signals
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
                hupped = 1;
                return;
        case SIGINT :
                exit(0);
                /*NOTREACHED*/
        case SIGTERM :
                done = 1;
                return;
        case SIGUSR1 :
                /* TODO? stats */
                return;
        case SIGUSR2 :
                rollulogpri();
                return;
        case SIGALRM :
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
        (void) sigaction(SIGPIPE, &sigact, NULL);

        /* Handle these */
#ifdef SA_RESTART       /* SVR4, 4.3+ BSD */
        /* usually, restart system calls */
        sigact.sa_flags |= SA_RESTART;
        /*
         * NOTE: The OSF/1 operating system doesn't conform to the UNIX standard
         * in this regard: the SA_RESTART flag does not affect writes to regular
         * files or, apparently, pipes.  Consequently, interrupted writes must
         * be handled explicitly.  See the discussion of the SA_RESTART option
         * at http://www.opengroup.org/onlinepubs/007908799/xsh/sigaction.html
         */
#endif
        sigact.sa_handler = signal_handler;
        (void) sigaction(SIGHUP, &sigact, NULL);
        (void) sigaction(SIGTERM, &sigact, NULL);
        (void) sigaction(SIGUSR1, &sigact, NULL);
        (void) sigaction(SIGUSR2, &sigact, NULL);
        (void) sigaction(SIGALRM, &sigact, NULL);

        /* Don't restart after interrupt */
        sigact.sa_flags = 0;
#ifdef SA_INTERRUPT     /* SunOS 4.x */
        sigact.sa_flags |= SA_INTERRUPT;
#endif
        (void) sigaction(SIGINT, &sigact, NULL);
}


static void
usage(
        char *av0 /*  id string */
)
{
        (void)fprintf(stderr,
                "Usage: %s [options] [confilename]\t\nOptions:\n",
                av0);
        (void)fprintf(stderr,
                "\t-v           Verbose, log each match (SIGUSR2 cycles)\n");
        (void)fprintf(stderr,
                "\t-x           Debug mode (SIGUSR2 cycles)\n");
        (void)fprintf(stderr,
                "\t-l logfile   Send log info to file (default uses syslogd)\n");
        (void)fprintf(stderr,
                "\t-d datadir   cd to \"datadir\" before interpreting filenames in\n");
        (void)fprintf(stderr,
                "\t             conffile (default %s)\n",
                DEFAULT_DATADIR);
        (void)fprintf(stderr,
                "\t-q queue     default \"%s\"\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-p pattern   Interested in products matching \"pattern\" (default \"%s\")\n", DEFAULT_PATTERN);
        (void)fprintf(stderr,
                "\t-f feedtype  Interested in products from feed \"feedtype\" (default %s)\n", s_feedtypet(DEFAULT_FEEDTYPE));
        (void)fprintf(stderr,
                "\t-i interval  loop, polling each \"interval\" seconds (default %d)\n", DEFAULT_INTERVAL);
        (void)fprintf(stderr,
                "\t-t timeo     set write timeo for PIPE subprocs to \"timeo\" secs (default %d)\n", DEFAULT_PIPE_TIMEO);
        (void)fprintf(stderr,
                "\t-o offset    the oldest product we will consider is \"offset\" secs before now (default: most recent in queue)\n");
        (void)fprintf(stderr,
                "\t(default conffilename is %s)\n",
                DEFAULT_CONFFILENAME);
        exit(1);
        /*NOTREACHED*/
}


int
main(int ac, char *av[])
{
        int status = 0;
        char *logfname = 0;
        /* data directory, conffile paths may be relative */
        char *datadir = DEFAULT_DATADIR;
        int interval = DEFAULT_INTERVAL;
        prod_spec spec;
        prod_class_t clss;
        int toffset = TOFFSET_NONE;
        int loggingToStdErr = 0;
        unsigned queue_size = 5000;

        conffilename = DEFAULT_CONFFILENAME;

        spec.feedtype = DEFAULT_FEEDTYPE;
        spec.pattern = DEFAULT_PATTERN;

        if(set_timestamp(&clss.from) != ENOERR) /* corrected by toffset below */
        {
                int errnum = errno;
                fprintf(stderr, "Couldn't set timestamp: %s", 
                        strerror(errnum));
                exit(1);
                /*NOTREACHED*/
        }
        clss.to = TS_ENDT;
        clss.psa.psa_len = 1;
        clss.psa.psa_val = &spec;

        /*
         * Check the environment for some options.
         * May be overridden by command line switches below.
         */
        {
                const char *ldmpqfname = getenv("LDMPQFNAME");
                if(ldmpqfname != NULL)
                        pqfname = ldmpqfname;
        }
        /*
         * deal with the command line, set options
         */
        {
        extern int optind;
        extern int opterr;
        extern char *optarg;

        int ch;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE));
        int fterr;

        opterr = 1;

        while ((ch = getopt(ac, av, "vxel:d:f:q:o:p:i:t:")) != EOF)
                switch (ch) {
                case 'v':
                        logmask |= LOG_UPTO(LOG_INFO);
                        break;
                case 'x':
                        logmask |= LOG_MASK(LOG_DEBUG);
                        break;
                case 'e':
                        key = ftok("/etc/rc.d/rc.local",'R');
                        semkey = ftok("/etc/rc.d/rc.local",'e');
                        shmid = shmget(key, sizeof(edex_message) * queue_size, 0666 | IPC_CREAT);
                        semid = semget(semkey, 2, 0666 | IPC_CREAT);
                        break;
                case 'l':
                        logfname = optarg;
                        break;
                case 'd':
                        datadir = optarg;
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
                case 'i':
                        interval = atoi(optarg);
                        if(interval == 0 && *optarg != '0')
                        {
                                fprintf(stderr, "%s: invalid interval %s\n", av[0], optarg);
                                usage(av[0]);   
                        }
                        break;
                case 't':
                        pipe_timeo = atoi(optarg);
                        if(pipe_timeo == 0 && *optarg != 0)
                        {
                                fprintf(stderr, "%s: invalid pipe_timeo %s", av[0], optarg);
                                usage(av[0]);   
                        }
                        break;
                case 'p':
                        spec.pattern = optarg;
                        break;
                case '?':
                        usage(av[0]);
                        break;
                }
        if(ac - optind == 1)
                conffilename = av[optind];
        (void) setulogmask(logmask);
        }

        /*
         * Initialize logging.
         */
        loggingToStdErr = STDERR_FILENO == openulog(
            ubasename(av[0]), (LOG_CONS|LOG_PID), LOG_LDM, logfname);

        unotice("Starting Up");

        /*
         * Initialze the previous-state module for this process.
         */
        if (stateInit(conffilename) < 0) {
            log_add("Couldn't initialize previous-state module");
            log_log(LOG_ERR);
            exit(EXIT_FAILURE);
            /*NOTREACHED*/
        }

        /*
         * The standard input stream is redirected to /dev/null because this
         * program doesn't use it and doing so prevents child processes
         * that mistakenly read from it from terminating abnormally.
         */
        if (NULL == freopen("/dev/null", "r", stdin))
        {
            err_log_and_free(
                ERR_NEW1(0, NULL,
                    "Couldn't redirect stdin to /dev/null: %s",
                    strerror(errno)),
                ERR_FAILURE);
            exit(EXIT_FAILURE);
            /*NOTREACHED*/
        }

        /*
         * The standard output stream is redirected to /dev/null because this
         * program doesn't use it and doing so prevents child processes
         * that mistakenly write to it from terminating abnormally.
         */
        if (NULL == freopen("/dev/null", "w", stdout))
        {
            err_log_and_free(
                ERR_NEW1(0, NULL,
                    "Couldn't redirect stdout to /dev/null: %s",
                    strerror(errno)),
                ERR_FAILURE);
            exit(EXIT_FAILURE);
            /*NOTREACHED*/
        }

        /*
         * If the standard error stream isn't being used for logging, then it's
         * redirected to /dev/null to prevent child processes that mistakenly
         * write to it from terminating abnormally.
         */
        if (!loggingToStdErr && NULL == freopen("/dev/null", "w", stderr))
        {
            err_log_and_free(
                ERR_NEW1(0, NULL, "Couldn't redirect stderr to /dev/null: %s",
                    strerror(errno)),
                ERR_FAILURE);
            exit(EXIT_FAILURE);
            /*NOTREACHED*/
        }

        /*
         * Inform the "filel" module about the number of available file
         * descriptors.  File descriptors are reserved for stdin, stdout,
         * stderr, the product-queue, the configuration-file, and (possibly) 
         * logging.
         */
        if (0 != set_avail_fd_count(openMax() - (5 + (!loggingToStdErr))))
        {
            uerror("Couldn't set number of available file-descriptors");
            unotice("Exiting");
            exit(1);
            /*NOTREACHED*/
        }

        /*
         * Inform the "filel" module of the shared memory segment
         */
        if (shmid != -1 && semid != -1)
        {
            set_shared_space(shmid, semid, queue_size);
        }

        /*
         * Compile the pattern.
         */
        if (re_isPathological(spec.pattern))
        {
                uerror("Adjusting pathological regular-expression: \"%s\"",
                    spec.pattern);
                re_vetSpec(spec.pattern);
        }
        status = regcomp(&spec.rgx, spec.pattern, REG_EXTENDED|REG_NOSUB);
        if(status != 0)
        {
                uerror("Can't compile regular expression \"%s\"",
                        spec.pattern);
                unotice("Exiting");
                exit(1);
                /*NOTREACHED*/
        }

        /*
         * register exit handler
         */
        if(atexit(cleanup) != 0)
        {
                serror("atexit");
                unotice("Exiting");
                exit(1);
                /*NOTREACHED*/
        }

        /*
         * set up signal handlers
         */
        set_sigactions();

        /*
         * Read in (compile) the configuration file.  We do this first so
         * its syntax may be checked without opening a product queue.
         */
        if ((status = readPatFile(conffilename)) < 0) {
                exit(1);
                /*NOTREACHED*/
        }
        else if (status == 0) {
            unotice("Configuration-file \"%s\" has no entries. "
                "You should probably not start this program instead.",
                conffilename);
        }

        /*
         * Open the product queue
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
                /*NOTREACHED*/
        }

        if(toffset != TOFFSET_NONE)
        {
            /*
             * Filter and queue position set by "toffset".
             */
            clss.from.tv_sec -= toffset;
            pq_cset(pq, &clss.from);
        }                               /* no time-offset specified */
        else
        {
            int needFromTime = 1;

            /*
             * Try getting the time of the last, successfully-processed
             * data-product from the previous invocation.
             */
            status = stateRead(&clss.from);

            if (status == 0) {
                timestampt      now;

                (void)set_timestamp(&now);

                if (tvCmp(now, clss.from, <)) {
                    log_start("Time of last processed data-product from "
                        "previous execution is in future.  Adjusting...");
                    log_log(LOG_WARNING);
                }
                else {
                    char        buf[80];

                    (void)strftime(buf, sizeof(buf), "%Y-%m-%d %T",
                        gmtime(&clss.from.tv_sec));
                    unotice("Starting from insertion-time %s.%06lu UTC", buf,
                        (unsigned long)clss.from.tv_usec);
                    pq_cset(pq, &clss.from);

                    needFromTime = 0;
                }
            }
            else if (status == -2) {
                /*
                 * Previous-state information doesn't exist.
                 */
                log_add(
                    "Previous-state information doesn't exist.  Continuing...");
                log_log(LOG_WARNING);
            }
            else if (status == -3) {
                /*
                 * Previous-state I/O error.
                 */
                log_add("Previous-state I/O error.  Continuing...");
                log_log(LOG_WARNING);
            }

            if (needFromTime) {
                /*
                 * Be permissive with the time filter,
                 * jump now to the end of the queue.
                 */
                clss.from = TS_ZERO;
                (void)pq_last(pq, &clss, NULL);
            }
        }

        if(ulogIsVerbose())
        {
                char buf[1984];
                uinfo("%s", s_prod_class(buf, sizeof(buf), &clss));
        }

        /*
         * Change directories if datadir was specified
         */
        if(datadir != NULL && *datadir != 0) 
        {
                /* change to data directory */
                if (chdir(datadir) == -1)
                {
                        serror("cannot chdir to %s", datadir);
                        exit(4);
                        /*NOTREACHED*/
                }
        }


        /*
         *  Do special pre main loop actions in pattern/action file
         *  N.B. Deprecate.
         */
        dummyprod("_BEGIN_");


        /*
         * Main loop
         */
        for (;;) {
            if (hupped) {
                unotice("Rereading configuration file %s", conffilename);
                (void) readPatFile(conffilename);
                hupped = 0;
            }

            status = pq_sequence(pq, TV_GT, &clss, processProduct, 0);

            if (status == 0) {
                /*
                 * A data-product was processed.
                 */
                timestampt       oldestCursor;

                pq_ctimestamp(pq, &currentCursor);
                currentCursorSet = 1;

                if (pq_getOldestCursor(pq, &oldestCursor) == 0 &&
                        tvEqual(oldestCursor, currentCursor)) {
                    timestampt  now;

                    (void)set_timestamp(&now);
                    uwarn("Processed oldest product in queue: %g s",
                        d_diff_timestamp(&now, &currentCursor));
                }

                (void)exitIfDone(0);
            }
            else {
                /*
                 * No data-product was processed.
                 */
                if (status == PQUEUE_END) {
                    udebug("End of Queue");

                    if (interval == 0)
                        break;
                }
                else if (status == EAGAIN || status == EACCES) {
                    udebug("Hit a lock");
                    /*
                     * Close the least recently used file descriptor.
                     */
                    close_lru(FL_NOTRANSIENT);
                }
                else if (status == EDEADLK
#if defined(EDEADLOCK) && EDEADLOCK != EDEADLK
                    || status == EDEADLOCK
#endif
                ) {
                    uerror("%s", strerror(status));
                    /*
                     * Close the least recently used file descriptor.
                     */
                    close_lru(FL_NOTRANSIENT);
                }
                else {
                    uerror("pq_sequence failed: %s (errno = %d)",
                        strerror(status), status);
                    exit(1);
                    /*NOTREACHED*/
                }

                (void)pq_suspend(interval);
                (void)exitIfDone(0);
            }                           /* data-product not processed */

            /*
             * Perform a non-blocking sync on all open file descriptors.
             */
            fl_sync(-1, FALSE);

            /*
             * Wait on any children which might have terminated.
             */
            while (reap(-1, WNOHANG) > 0)
                /*EMPTY*/;
        }                               /* main loop */

        return 0;
}
