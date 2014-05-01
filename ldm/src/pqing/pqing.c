/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqing.c,v 1.78.10.2.2.3 2007/02/12 20:38:54 steve Exp $ */
static char version[] =
"$Revision: 1.78.10.2.2.3 $ built "__DATE__" "__TIME__;

#include <ldmconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <limits.h> /* PATH_MAX */
#ifndef PATH_MAX
#define PATH_MAX 255
#endif /* !PATH_MAX */
#include <sys/types.h>
#include <sys/time.h>
#include <rpc/rpc.h>
#include <errno.h>

#include "ldm.h"
#include "ulog.h"
#include "globals.h"
#include "inetutil.h"
#include "feed.h"
#include "atofeedt.h"
#include "ldmprint.h"
#include "wmo_message.h"
#include "afos_message.h"
#include "faa604_message.h"
#include "paths.h"
#include "pq.h"
#include "md5.h"
#include "timestamp.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

#if NET
#define RETRY_DELAY     (10)            /* delay factor between retries */
#define MAX_RETRIES     (30)
#endif /* NET */

/* set by command line */
char *baud = NULL;
char *rawfname = NULL;
char *parity = NULL;
enum { CHK_UNSET, CHK_CHECK, CHK_DONT} chkflag = CHK_UNSET;
static feedtypet feedtype = NONE; /* deduce from av[0]  */
static const char *progname = NULL;
static char *logpath = "";
static char feedfname[PATH_MAX];
static char myname[HOSTNAMESIZE];
static const char *pqpath = DEFAULT_QUEUE;
extern int usePil;  /* 1/0 flag to signal use of AFOS like pil identifier */
int useNex=1; /* 1/0 flag to retype nexrad products as NEXRAD */
/* skipLeadingCtlString: used in computing checksum, default is to skip */
static int skipLeadingCtlString = 1; 

static int ifd = -1; 

static volatile int intr = 0;
static volatile int stats_req = 0;

static void (*prod_stats)(void) = wmo_stats;
static unsigned long ndups = 0;

static MD5_CTX *md5ctxp = NULL;

#if NET
static int port_error = 0;      /* indicate sigpipe condition was raised */
#       ifndef DEFAULT_RESET_SECS
#       define DEFAULT_RESET_SECS 600
#       endif
static int reset_secs = DEFAULT_RESET_SECS;
#endif

/*
 * called at exit
 */
static void
cleanup(void)
{
        unotice("Exiting"); 
        if(!intr)
        {
                /* We are not in the interrupt context */

                if(md5ctxp != NULL)
                {
                        free_MD5_CTX(md5ctxp);  
                }

                if(pq != NULL)
                {
                        off_t highwater = 0;
                        size_t maxregions = 0;
                        (void) pq_highwater(pq, &highwater, &maxregions);
                        (void) pq_close(pq);
                        pq = NULL;

                        if(feed_close)
                                (*feed_close)(ifd);
                        ifd = -1;
                        unotice("  Queue usage (bytes):%8ld",
                                                (long)highwater);
                        unotice("           (nregions):%8ld",
                                                (long)maxregions);
                        unotice("  Duplicates rejected:%8lu", ndups);
                }
                (*prod_stats)();
                (*feed_stats)();
        }
        (void) closeulog();
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
        case SIGINT :
                intr = !0;
                exit(0);
        case SIGTERM :
                done = !0;
                return;
        case SIGPIPE :
#if NET
                if(INPUT_IS_SOCKET)
                {
                        port_error = !0;
                }
#endif
                return;
        case SIGUSR1 :
                stats_req = !0;
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
        sigact.sa_flags = 0;

        /* Ignore these */
        sigact.sa_handler = SIG_IGN;
        (void) sigaction(SIGHUP, &sigact, NULL);
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
        (void) sigaction(SIGPIPE, &sigact, NULL);
}


static void
usage(
        char *av0 /*  id string */
)
{
        (void)fprintf(stderr,
                "Usage: %s [options] feedname\t\nOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-v           Verbose, tell me about each product\n");
        (void)fprintf(stderr,
                "\t-r rawfile   Stash 'raw' data in \"rawfile\"\n");
        (void)fprintf(stderr,
                "\t-l logfile   Default logs to syslogd\n");
        (void)fprintf(stderr,
                "\t-f type      Claim to be feedtype \"type\", one of \"hds\", \"ddplus\", ...\n");
        (void)fprintf(stderr,
                "\t-b baud      Set baudrate for tty feed.\n");
        (void)fprintf(stderr,
                "\t-q queue     default \"%s\"\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-p [even|odd|none]  Set parity for tty feed.\n");
        (void)fprintf(stderr,
                "\t-c           Enable checksum or parity check on non tty feed\n");
        (void)fprintf(stderr,
                "\t-n           Disable checksum or parity check on tty feed\n");
        (void)fprintf(stderr,
                "\t-i           Do not include a PIL-like /p identifier for suitable products\n");
        (void)fprintf(stderr,
                "\t-5           Skip leading control string in calculating checksum\n");
        (void)fprintf(stderr,
                "\t-N           Do not place NEXRAD products in feedtype NEXRAD\n");
        (void)fprintf(stderr,
                "\t             (WMO products only)\n");
#if NET
        (void)fprintf(stderr,
                "\t-P port      Get input via TCP connection to host \"feedname\" at \"port\"\n");
        (void)fprintf(stderr,
                "\t-T timeout   Idle timeout before TCP reconnect, in seconds\n");
        (void)fprintf(stderr,
                "\t             (defaults to %d, 0 disables timeout)\n",
                        DEFAULT_RESET_SECS);
#endif
        exit(1);
}


/*
 * Determine if a product starts with the string 
 * "\r\r\n<sequnceNumber>\r\r\n".  If it doesn't, return.  If it does, 
 * return a pointer to the start of the product, skipping over those 
 * leading control chars.
 *
 * A sequence number is expected to be any string of at most MAX_SEQ_NUM_LEN 
 * digits with possibly leading or trailing blanks included in that count.  
 * However, the only check done here is to see that the sequence number  
 * consists of MAX_SEQ_NUM_LEN or fewer characters. No other checks are 
 * performed.
 */
char *
wmo_prod(const char *prod)
{
#define PART1_SIZE 4
#define PART2_SIZE 4
#define MAX_SEQ_NUM_LEN 4

  char part1[PART1_SIZE] = {'', '\r', '\r', '\n'};
  char part2[PART2_SIZE] = {'\r', '\r', '\n', '\0'}; /* '\0' is for strstr */
  char *startPart2;
  int seqNumLength;

  /*
   * If part1 is not at start of product, return 
   */
  if ((strncmp(part1, prod, PART1_SIZE)) != 0)
    return 0;

  /*
   * If part2 doesn't occur somewhere after part1, return 
   */
  if ((startPart2 = strstr (prod+PART1_SIZE, part2)) == 0)
    return 0;

  /*
   * Pick out substring between part1 and part2 that contains the 
   * sequence number 
   */
  seqNumLength = startPart2 - (prod+PART1_SIZE);
  
  /*
   * Sanity check: if the length of the sequence number string is 
   * too big, return 
   */
  if (seqNumLength > MAX_SEQ_NUM_LEN)
    return 0;

  /*
   * If we got here, we've classified it as a wmo product.  
   * Return a pointer to the beginning of the product.
   */
  return startPart2 + PART2_SIZE - 1;  /* exclude trailing '\0'  */
}

/*
 * Arguments:
 *      arrival Data-product creation-time.  IGNORED.  The creation-time
 *              will be set by this function according to the system clock
 *              just prior to inserting the data-product into the product-
 *              queue.
 *      seqno   Sequence number.
 *      ident   Product-identifier.
 *      len     Size of data-portion of data-product in bytes.
 *      buf     Pointer to data-portion of data-product.
 */
void
toClients(timestampt arrival,
        unsigned seqno,
        const char *ident,
        unsigned len,
        const char *buf)
{
        static struct product prod;
        int status;
        char *result;

        MD5Init(md5ctxp);
        /*
         * If user has not disabled skipLeadingCtlString with -5 option,
         * and the product contains the correct leading control string for
         * a wmo product, then skip that control string in calculating the 
         * checksum.
         */
        if (skipLeadingCtlString && ((result = wmo_prod(buf)) != 0))
          {
          MD5Update(md5ctxp, (const unsigned char *)result, len-(result-buf));
#if DEBUG
          uinfo("WMO prod: Skipping %d chars\n", result-buf);
#endif
          }
        else  /* calculate checksum on entire product */         
        {
          MD5Update(md5ctxp, (const unsigned char *)buf, len);
#if DEBUG
          uinfo("not a WMO Prod\n");
#endif
        }
        MD5Final((unsigned char*)prod.info.signature, md5ctxp);

        prod.info.origin = myname;
        prod.info.feedtype = feedtype;
        prod.info.seqno = seqno;
        prod.info.ident = (char *)ident; /* cast away const */
        prod.info.sz = len;
        prod.data = (void *)buf; /* cast away const */

        if(((strncmp(prod.info.ident,"SDUS2",5) == 0) ||
            (strncmp(prod.info.ident,"SDUS3",5) == 0) ||
            (strncmp(prod.info.ident,"SDUS5",5) == 0) ||
            (strncmp(prod.info.ident,"SDUS7",5) == 0)) && (useNex == 1))
           {
           prod.info.feedtype = NEXRAD;
           }

        if(ulogIsVerbose())
                uinfo("%s", s_prod_info(NULL, 0, &prod.info, ulogIsDebug()));

        if(pq == NULL)          /* if we are "feedtest", do nothing else */
                return;

        set_timestamp(&prod.info.arrival);

        status = pq_insert(pq, &prod);
        if(status == ENOERR)
                return; /* Normal return */

        /* else */
        if(status == PQUEUE_DUP)
        {
                ndups++;
                uinfo("Product already in queue");
                return;
        }

        /* else, error */
        uerror("pq_insert: %s\n",
                status > 0 ? strerror(status) : "Internal error");
        exit(1); /* ??? */
}


static void
setFeedDefaults(feedtypet type)
{
        /* set up defaults for feed according to type */
        switch (type) {
        case DDPLUS :
                baud = "19200";
                parity = "even";
                break;
        case PPS :
        case DDS :
        case IDS :
                baud = "9600";
                parity = "even";
                break;
        case HDS :
                baud = "19200";
                parity = "none";
                break;
        case AFOS :
                baud = "4800"; /* ??? */
                parity = "none";
                break;
        case FAA604 :
                baud = "1200";
                parity = "even";
                break;
        }
}


static feedtypet 
whatami(const char *av0)
{
        feedtypet type;
#define SEP     '/' /* separates components of path */
        /* strip off leading path */
        if ((progname = strrchr(av0, SEP)) == NULL)
                progname = av0;
        else
            progname++;
        
        type = atofeedtypet(progname);
        if(type == NONE)
                type = WMO; /* default for wmo ingestd */
        setFeedDefaults(type);
        return type;    
}


int
main(int ac, char *av[])
{

        int logfd;
        int width;
        int ready;
        unsigned long idle;
        fd_set readfds;
        fd_set exceptfds;
        struct timeval timeo;

        feedtype = whatami(av[0]);

        /*
         * Check the environment for some options.
         * May be overridden by command line switches below.
         */
        {
                const char *ldmpqfname = getenv("LDMPQFNAME");
                if(ldmpqfname != NULL)
                        pqpath = ldmpqfname;
        }

        {
        extern int optind;
        extern int opterr;
        extern char *optarg;
        int ch;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE));

        opterr = 1;
        usePil = 1;
        useNex = 1;

        while ((ch = getopt(ac, av, "vxcni5Nl:b:p:P:T:q:r:f:")) != EOF)
                switch (ch) {
                case 'v':
                        logmask |= LOG_MASK(LOG_INFO);
                        break;
                case 'x':
                        logmask |= LOG_MASK(LOG_DEBUG);
                        break;
                case 'c':
                        chkflag = CHK_CHECK;
                        break;
                case 'n':
                        chkflag = CHK_DONT;
                        break;
                case 'i':
                        usePil = 0;
                        break;
                case 'N':
                        useNex = 0;
                        break;
                case '5':
                        skipLeadingCtlString = 0;
                        break;
                case 'l':
                        if(optarg[0] == '-' && optarg[1] != 0)
                        {
                                fprintf(stderr, "logfile \"%s\" ??\n",
                                        optarg);
                                usage(av[0]);
                        }
                        /* else */
                        logpath = optarg;
                        break;
                case 'b':
                        baud = optarg;
                        break;
                case 'p':
                        parity = optarg;
                        break;
#if NET
                case 'P':
                        *((int *)&server_port) = atoi(optarg); /* cast away const */
                        if(server_port <= 0 || server_port > 65536)
                        {
                                fprintf(stderr, "invalid server_port %s\n",
                                        optarg);
                                usage(av[0]);
                        }
                        break;
                case 'T':
                        reset_secs = atoi(optarg);
                        if(reset_secs < 0)
                        {
                                fprintf(stderr, "invalid timeout %s\n",
                                        optarg);
                                usage(av[0]);
                        }
                        break;
#endif /* NET */
                case 'q':
                        pqpath = optarg;
                        break;
                case 'r':
                        rawfname = optarg;
                        break;
                case 'f':
                        {
                                feedtypet type;
                                type = atofeedtypet(optarg);
                                if(type != NONE)
                                {
                                        feedtype = type;
                                        if(!parity && !baud)
                                                setFeedDefaults(type);
                                }
                        }
                        break;
                case '?':
                        usage(av[0]);
                        break;
                }

        /* last arg, feedfname, is required */
        if(ac - optind <= 0)
                usage(av[0]);
        strncat(feedfname,av[optind], sizeof(feedfname) - 6 );

        (void) setulogmask(logmask);
        }


        /*
         * initialize logger
         */
        if(logpath == NULL || !(*logpath == '-' && logpath[1] == 0))
                (void) fclose(stderr);
        logfd = openulog(ubasename(av[0]),
                (LOG_CONS|LOG_PID), LOG_LDM, logpath);
        unotice("Starting Up");
        udebug(version);

        if(logpath == NULL || !(*logpath == '-' && logpath[1] == 0))
        {
                setbuf(fdopen(logfd, "a"), NULL);
        }       

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

        /*
         * open the product queue, unless we were invoked as "feedtest"
         */
        if(strcmp(progname, "feedtest") != 0)
        {
                if(ready = pq_open(pqpath, PQ_DEFAULT, &pq))
                {
                        if (PQ_CORRUPT == ready) {
                            uerror("The product-queue \"%s\" is inconsistent\n",
                                    pqpath);
                        }
                        else {
                            uerror("pq_open: \"%s\" failed: %s",
                                    pqpath, strerror(ready));
                        }
                        return 1;
                }
        }

        /*
         * who am i, anyway
         */
        (void) strcpy(myname, ghostname());

        /*
         * open the feed
         */
        if(!(*feedfname == '-' && feedfname[1] == 0) && logfd != 0)
                (void) close(0);

        if(open_feed(feedfname, &ifd) != ENOERR)
                return 1;

        if(usePil == 1)
           {
           if ((feedtype & DDS)||(feedtype & PPS)||(feedtype & IDS)||
                (feedtype & HRS))
              {
              usePil = 1;
              uinfo("Creating AFOS-like pil tags\0");
              }
           else
              {
              usePil = 0;
              }
           }

        if (feedtype & HDS)
        {
                if(chkflag == CHK_CHECK
                                || (isatty(ifd) && chkflag != CHK_DONT))
                        setTheScanner(scan_wmo_binary_crc);
                else
                        setTheScanner(scan_wmo_binary);
        }
        else if (feedtype == ( DDPLUS | IDS ) ) 
        { 
                /* this is the combined NOAAPORT fos-alike. We know these have the
                   4 byte start and end sequences. Using the binary scanner
                   ensures that we don't stop on an arbitray embedded CTRL-C */
                unotice("Note: Using the wmo_binary scanner for SDI ingest\0");
                setTheScanner (scan_wmo_binary); 
        }
        else if (feedtype & (NMC2 | NMC3))
        {
                setTheScanner(scan_wmo_binary);
        }
        else if (feedtype == AFOS)
        {
                prod_stats = afos_stats;
                setTheScanner(scan_afos);
        }
        else if (feedtype == FAA604)
        {
                prod_stats = faa604_stats;
                if(chkflag == CHK_CHECK
                        || (isatty(ifd)
                                 && chkflag != CHK_DONT
                                 && parity != NULL
                                 && *parity != 'n')
                        )
                {
                        setTheScanner(scan_faa604_parity);
                }
                else
                {
                        setTheScanner(scan_faa604);
                }
        }
        else
        {
                if(chkflag == CHK_CHECK
                        || (isatty(ifd)
                                 && chkflag != CHK_DONT
                                 && parity != NULL
                                 && *parity != 'n')
                        )
                {
                        setTheScanner(scan_wmo_parity);
                }
                else
                {
                        setTheScanner(scan_wmo);
                }
        }

        /*
         * Allocate an MD5 context
         */
        md5ctxp = new_MD5_CTX();
        if(md5ctxp == NULL)
        {
                serror("new_md5_CTX failed");
                return 1;
        }


        /*
         * Main Loop
         */
        idle = 0;
        while(exitIfDone(0))
        {
#if NET
if (INPUT_IS_SOCKET)
{
                if (port_error)
                {
                        /*
                         * lost connection => close
                         */
                        if (ifd >= 0)
                        {
                                if(feed_close)
                                        (*feed_close)(ifd);
                                ifd = -1;
                        }
                        port_error = 0;
                        sleep (2);      /* allow things to settle down */
                        continue;
                }
}
#endif
                if(stats_req)
                {
                        unotice("Statistics Request"); 
                        if(pq != NULL)
                        {
                                off_t highwater = 0;
                                size_t maxregions = 0;
                                (void) pq_highwater(pq, &highwater,
                                         &maxregions);
                                unotice("  Queue usage (bytes):%8ld",
                                                        (long)highwater);
                                unotice("           (nregions):%8ld",
                                                        (long)maxregions);
                        }
                        unotice("       Idle: %8lu seconds", idle);
#if NET
if (INPUT_IS_SOCKET)
{
                        unotice("    Timeout: %8d", reset_secs);
}
#endif
                        unotice("%21s: %s", "Status",
                                (ifd < 0) ?
                                "Not connected or input not open." :
                                "Connected.");
                        (*prod_stats)();
                        (*feed_stats)();
                        stats_req = 0;
                }
#if NET
if (INPUT_IS_SOCKET)
{
                if (ifd < 0)
                {
                        /* Attempt reconnect */
                        static int retries = 0;
                        if (retries > MAX_RETRIES)
                        {
                                uerror ("maximum retry attempts %d, aborting",
                                        MAX_RETRIES);
                                done = !0;
                                continue;
                        }
                        /* Try to reopen on tcp read errors */
                        unotice("Trying to re-open connection on port %d", 
                                server_port);
                        ++retries;
                        if(open_feed(feedfname, &ifd) != ENOERR)
                        {
                                unotice ("sleeping %d seconds before retry %d",
                                         retries * RETRY_DELAY, retries+1);
                                sleep (retries * RETRY_DELAY);
                                continue;
                        }
                        retries = 0;
                }
}
#endif /* NET */
                timeo.tv_sec = 3;
                timeo.tv_usec = 0;
                FD_ZERO(&readfds);
                FD_ZERO(&exceptfds);
                FD_SET(ifd, &readfds);
                FD_SET(ifd, &exceptfds);
                width =  ifd + 1;
                ready = select(width, &readfds, 0, &exceptfds, &timeo);
                if(ready < 0 )
                {
                        /* handle EINTR as a special case */
                        if(errno == EINTR)
                        {
                                errno = 0;
                                continue;
                        }
                        serror("select");
                        return 1;
                }
                /* else */
#if 0
                if (FD_ISSET(ifd, &exceptfds))
                {
                        uerror("Exception on input fd %d, select returned %d",
                               ifd, ready);
                }
#endif
                if(ready > 0)
                {
                        /* do some work */
                        if(FD_ISSET(ifd, &readfds) || 
                           FD_ISSET(ifd, &exceptfds))
                        {
                                idle = 0;
                                if(feedTheXbuf(ifd) != ENOERR)
                                {
#if NET
if (INPUT_IS_SOCKET)
{
                                        port_error = !0;
                                        continue;
}                                       /* else */
#endif /* NET */
                                        done = !0;
                                }
                                FD_CLR(ifd, &readfds);
                                FD_CLR(ifd, &exceptfds);
                        }
                        else
                        {
                                uerror("select returned %d but ifd not set",
                                        ready);
                                idle += timeo.tv_sec;
                        }
                }
                else    /* ready == 0 */
                {
                        idle += timeo.tv_sec;
#if NET
if (INPUT_IS_SOCKET)
{
                        /* VOODOO
                         * This is necessary to stimulate
                         * 'Connection reset by peer'
                         * when the Portmaster goes down and comes
                         * back up.
                         */
                        static char zed[1] = {0};
                        if(write(ifd, zed, sizeof(zed)) < 0)
                        {
                                port_error = !0;
                                continue;
                        }

}
#endif
                }
#if NET
if (INPUT_IS_SOCKET)
{
                if ((reset_secs > 0) && (idle >= reset_secs))
                {
                        unotice("Idle for %ld seconds, reconnecting",
                                idle);
                        /* force reconnect */
                        port_error = !0;
                        idle = 0;
                        continue;
                }
}
#endif /* NET */
                (void) scanTheXbuf();
        }

        return 0;
}

