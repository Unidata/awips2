/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqinsert.c,v 1.27.10.2.2.6 2007/02/09 21:35:02 steve Exp $ */

/* 
 * Convert files to ldm "products" and insert in local que
 */
#include <ldmconfig.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <rpc/rpc.h>
#include <signal.h>
#ifndef NO_MMAP
#include <sys/mman.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include "paths.h"
#include "ldm.h"
#include "pq.h"
#include "globals.h"
#include "atofeedt.h"
#include "ldmprint.h"
#include "inetutil.h"
#include "ulog.h"
#include "md5.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

        /* N.B.: assumes hostname doesn't change during program execution :-) */
static char             myname[HOSTNAMESIZE];
static feedtypet        feedtype = EXP;
#ifdef NO_MMAP
static struct pqe_index index = PQE_NONE;
#endif


static void
usage(
        char *av0 /*  id string */
)
{
        (void)fprintf(stderr,
                "Usage: %s [options] filename ...\n\tOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-v         Verbose, tell me about each product\n");
        (void)fprintf(stderr,
                "\t-l logfile    log to a file rather than stderr\n");
        (void)fprintf(stderr,
                "\t-q queue      default \"%s\"\n", DEFAULT_QUEUE);
        (void)fprintf(stderr,
                "\t-s seqno      set initial product sequence number to \"seqno\", defaults to 0\n");
        (void)fprintf(stderr,
                "\t-f feedtype   assert your feed type as \"feedtype\", defaults to \"EXP\"\n");
        (void)fprintf(stderr,
                "\t-p productID  assert product ID as \"productID\", defaults to filename\n\t\t      With multiple files, product ID becomes productID.<sequenceNumber>\n");
        exit(1);
}


void
cleanup(void)
{
    if (pq) {
#ifdef NO_MMAP
        if (!pqeIsNone(index))
            (void)pqe_discard(pq, index);
#endif

        (void) pq_close(pq);
        pq = NULL;
    }

    (void)closeulog();
}


static void
signal_handler(
        int sig
)
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
    }
}


static void
set_sigactions(void)
{
#ifndef NO_POSIXSIGNALS
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
        /* Don't restart after interrupt */
        sigact.sa_flags = 0;
#ifdef SA_INTERRUPT     /* SunOS 4.x */
        sigact.sa_flags |= SA_INTERRUPT;
#endif
        (void) sigaction(SIGINT, &sigact, NULL);
#else
        
        (void) signal(SIGHUP, SIG_IGN);
        (void) signal(SIGALRM, SIG_IGN);
        (void) signal(SIGCHLD, SIG_IGN);

        (void) signal(SIGTERM, signal_handler);
        (void) signal(SIGINT, signal_handler);
#endif
}


#ifdef NO_MMAP
static int
fd_md5(MD5_CTX *md5ctxp, int fd, off_t st_size, signaturet signature)
{
        int nread;
        char buf[8192];
        MD5Init(md5ctxp);

        for(; st_size > 0; st_size -= nread )
        {
                nread = read(fd, buf, sizeof(buf));
                if(nread <= 0)
                {
                        serror("fd_md5: read");
                        return -1;
                } /* else */
                MD5Update(md5ctxp, buf, nread);

                (void)exitIfDone(1);
        }

        MD5Final(signature, md5ctxp);
        return 0;
}
#else
static int
mm_md5(MD5_CTX *md5ctxp, void *vp, size_t sz, signaturet signature)
{
        MD5Init(md5ctxp);

        MD5Update(md5ctxp, vp, sz);

        MD5Final((unsigned char*)signature, md5ctxp);
        return 0;
}
#endif


int main(
        int ac,
        char *av[]
)
{
        char *progname = av[0];
        char *logfname;
        int useProductID = FALSE;
        int signatureFromId = FALSE;
        char *productID = NULL;
        int multipleFiles = FALSE;
        char identifier[KEYSIZE];
        int status;
        int seq_start = 0;
        enum ExitCode {
            exit_success = 0,
            exit_system = 1,    /* operating-system failure */
            exit_pq_open = 2,   /* couldn't open product-queue */
            exit_infile = 3,    /* couldn't process input file */
            exit_md5 = 6        /* couldn't initialize MD5 processing */
        } exitCode = exit_success;

        logfname = "-";

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

        while ((ch = getopt(ac, av, "ivxl:q:f:s:p:")) != EOF)
                switch (ch) {
                case 'i':
                        signatureFromId = 1;
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
                case 's':
                        seq_start = atoi(optarg);
                        break;
                case 'f':
                        feedtype = atofeedtypet(optarg);
                        if(feedtype == NONE)
                        {
                            fprintf(stderr, "Unknown feedtype \"%s\"\n", optarg);
                                usage(progname);        
                        }
                        break;
                case 'p':
                        useProductID = TRUE;
                        productID = optarg;
                        break;
                case '?':
                        usage(progname);
                        break;
                }

        ac -= optind; av += optind ;

        if(ac < 1) usage(progname);
        (void) setulogmask(logmask);
        }

        /*
         * Set up error logging
         */
        (void) openulog(ubasename(progname), LOG_NOTIME, LOG_LDM, logfname);

        /*
         * register exit handler
         */
        if(atexit(cleanup) != 0)
        {
                serror("atexit");
                exit(exit_system);
        }

        /*
         * set up signal handlers
         */
        set_sigactions();

        /*
         * who am i, anyway
         */
        (void) strcpy(myname, ghostname());

        /*
         * open the product queue
         */
        if(status = pq_open(pqfname, PQ_DEFAULT, &pq))
        {
                if (PQ_CORRUPT == status) {
                    uerror("The product-queue \"%s\" is inconsistent\n",
                            pqfname);
                }
                else {
                    uerror("pq_open: \"%s\" failed: %s",
                            pqfname, status > 0 ? strerror(status) :
                                            "Internal error");
                }
                exit(exit_pq_open);
        }


        {
        char *filename;
        int fd;
        struct stat statb;
        product prod;
        MD5_CTX *md5ctxp = NULL;

        /*
         * Allocate an MD5 context
         */
        md5ctxp = new_MD5_CTX();
        if(md5ctxp == NULL)
        {
                serror("new_md5_CTX failed");
                exit(exit_md5);
        }


        /* These members are constant over the loop. */
        prod.info.origin = myname;
        prod.info.feedtype = feedtype;

        if (ac > 1) {
          multipleFiles = TRUE;
        }

        for(prod.info.seqno = seq_start ; ac > 0 ;
                         av++, ac--, prod.info.seqno++)
        {
                filename = *av;

                fd = open(filename, O_RDONLY, 0);
                if(fd == -1)
                {
                        serror("open: %s", filename);
                        exitCode = exit_infile;
                        continue;
                }

                if( fstat(fd, &statb) == -1) 
                {
                        serror("fstat: %s", filename);
                        (void) close(fd);
                        exitCode = exit_infile;
                        continue;
                }

                /* Determine what to use for product identifier */
                if (useProductID) 
                  {
                    if (multipleFiles) 
                      {
                        sprintf(identifier,"%s.%d", productID, prod.info.seqno);
                        prod.info.ident = identifier;
                      }
                    else
                      prod.info.ident = productID;
                   }
                else
                    prod.info.ident = filename;
                
                prod.info.sz = statb.st_size;
                prod.data = NULL;

#ifndef NO_MMAP
                prod.data = mmap(0, prod.info.sz,
                        PROT_READ, MAP_PRIVATE, fd, 0);
                if(prod.data == NULL)
                {
                        serror("mmap: %s", filename);
                        (void) close(fd);
                        exitCode = exit_infile;
                        continue;
                }

                status = 
                    signatureFromId
                        ? mm_md5(md5ctxp, prod.info.ident,
                            strlen(prod.info.ident), prod.info.signature)
                        : mm_md5(md5ctxp, prod.data, prod.info.sz,
                            prod.info.signature);

                (void)exitIfDone(1);

                if (status != 0) {
                    serror("mm_md5: %s", filename);
                    (void) munmap(prod.data, prod.info.sz);
                    (void) close(fd);
                    exitCode = exit_infile;
                    continue;
                }

                /* These members, and seqno, vary over the loop. */
                status = set_timestamp(&prod.info.arrival);
                if(status != ENOERR) {
                        serror("set_timestamp: %s, filename");
                        exitCode = exit_infile;
                        continue;
                }

                /*
                 * Do the deed
                 */
                status = pq_insert(pq, &prod);

                switch (status) {
                case ENOERR:
                    /* no error */
                    if(ulogIsVerbose())
                        uinfo("%s", s_prod_info(NULL, 0, &prod.info,
                            ulogIsDebug())) ;
                    break;
                case PQUEUE_DUP:
                    uerror("Product already in queue: %s",
                        s_prod_info(NULL, 0, &prod.info, 1));
                    break;
                case ENOMEM:
                    uerror("queue full?");
                    break;  
                case EINTR:
#if defined(EDEADLOCK) && EDEADLOCK != EDEADLK
                case EDEADLOCK:
                    /*FALLTHROUGH*/
#endif
                case EDEADLK:
                    /* TODO: retry ? */
                    /*FALLTHROUGH*/
                default:
                    uerror("pq_insert: %s", status > 0
                        ? strerror(status) : "Internal error");
                    break;
                }

                (void) munmap(prod.data, prod.info.sz);
#else /*!NO_MMAP*/
                status = 
                    signatureFromId
                        ? mm_md5(md5ctxp, prod.info.ident,
                            strlen(prod.info.ident), prod.info.signature)
                        : fd_md5(md5ctxp, fd, statb.st_size,
                            prod.info.signature);

                (void)exitIfDone(1);

                if (status != 0) {
                        serror("fd_md5: %s", filename);
                        (void) close(fd);
                        exitCode = exit_infile;
                        continue;
                }

                if(lseek(fd, 0, SEEK_SET) == (off_t)-1)
                {
                        serror("rewind: %s", filename);
                        (void) close(fd);
                        exitCode = exit_infile;
                        continue;
                }

                index = PQE_NONE;
                status = pqe_new(pq, &prod.info, &prod.data, &index);

                if(status != ENOERR) {
                    serror("pqe_new: %s", filename);
                    exitCode = exit_infile;
                }
                else {
                    ssize_t     nread = read(fd, prod.data, prod.info.sz);

                    (void)exitIfDone(1);

                    if (nread != prod.info.sz) {
                        serror("read %s %u", filename, prod.info.sz);
                        status = EIO;
                    }
                    else {
                        status = pqe_insert(pq, index);
                        index = PQE_NONE;

                        switch (status) {
                        case ENOERR:
                            /* no error */
                            if(ulogIsVerbose())
                                uinfo("%s", s_prod_info(NULL, 0, &prod.info,
                                    ulogIsDebug())) ;
                            break;
                        case PQUEUE_DUP:
                            uerror("Product already in queue: %s",
                                s_prod_info(NULL, 0, &prod.info, 1));
                            break;
                        case ENOMEM:
                            uerror("queue full?");
                            break;  
                        case EINTR:
#if defined(EDEADLOCK) && EDEADLOCK != EDEADLK
                        case EDEADLOCK:
                            /*FALLTHROUGH*/
#endif
                        case EDEADLK:
                            /* TODO: retry ? */
                            /*FALLTHROUGH*/
                        default:
                            uerror("pq_insert: %s", status > 0
                                ? strerror(status) : "Internal error");
                        }
                    }                   /* data read into "index" region */

                    if (status != ENOERR) {
                        (void)pqe_discard(pq, index);
                        index = PQE_NONE;
                    }
                }                       /* "index" region allocated */

#endif /*!NO_MMAP*/
                (void) close(fd);
        }                               /* input-file loop */

        free_MD5_CTX(md5ctxp);  
        }                               /* code block */

        exit(exitCode);
}
