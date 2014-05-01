/*
 *   Copyright 1994, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pqcreate.c,v 1.13.16.1.2.2 2007/02/12 20:38:54 steve Exp $ */

/*
 * Create an empty ldm product queue of a given size
 */

#include <ldmconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "ldm.h"
#include "paths.h"
#include "ulog.h"
#include "pq.h"


static void
usage(const char *av0)
{
#define USAGE_FMT "\
Usage: %s [options] <initialsz>[k|m|g] <pqfname>\n\
       %s [options] -s <initialsz>[k|m|g] [-q <pqfname>]\n\
Options:\n\
        -v\n\
        -c\n\
        -f\n\
        -l logfname\n\
        -S nproducts\n\
       (default pqfname is \"%s\")\n\
"

        (void)fprintf(stderr, USAGE_FMT,
                        av0,
                        av0,
                        DEFAULT_QUEUE);
        exit(1);
}


int main(int ac, char *av[])
{
        int verbose = 0;
        const char *pqfname = DEFAULT_QUEUE;
        int pflags = PQ_NOCLOBBER;
        off_t initialsz = 0;
        size_t nproducts = 0;
        pqueue *pq = NULL;
        int errnum = 0;
        char *logfname = "-" ;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE)) ;

        int ch;
        char *qopt = NULL;
        char *sopt = NULL;
        char *Sopt = NULL;
        extern char     *optarg;
        extern int       optind;

        /*
         * Check the environment for some options.
         * May be overridden by command line switches below.
         */
        {
                const char *ldmpqfname = getenv("LDMPQFNAME");
                if(ldmpqfname != NULL)
                        pqfname = ldmpqfname;
        }

        while ((ch = getopt(ac, av, "xvcfq:s:S:l:")) != EOF)
                switch (ch) {
                case 'v':
                        verbose = !0;
                        logmask |= LOG_MASK(LOG_INFO) ;
                        break;
                case 'c':
                        pflags &= ~PQ_NOCLOBBER;
                        break;
                case 'f':
                        pflags |= PQ_SPARSE;
                        break;
                case 's':
                        sopt = optarg;
                        break;
                case 'S':
                        Sopt = optarg;
                        break;
                case 'q':
                        qopt = optarg;
                        break;
                case 'x':
                        logmask |= LOG_MASK(LOG_DEBUG) ;
                        break;
                case 'l':
                        logfname = optarg ;
                        break;
                case '?':
                        usage(av[0]);
                        break;
                }
        
        if(ac - optind > 1)
        {
                if(sopt)
                        usage(av[0]);
                sopt = av[ac - 2];
        }
        if(ac - optind > 0)
        {
                if(qopt)        
                        usage(av[0]);
                qopt =  av[ac - 1];
        }

        if(qopt)
                pqfname = qopt ;

        if(sopt)
        {
                char *cp = sopt + strlen(sopt) -1;
                initialsz = atol(sopt);
                if(isalpha(*cp))
                {
                        switch(*cp) {
                        case 'k':
                        case 'K':
                                initialsz *= 1000;
                                break;
                        case 'm':
                        case 'M':
                                initialsz *= 1000*1000;
                                break;
                        case 'g':
                        case 'G':
                                initialsz *= 1000*1000*1000;
                                break;
                        default:
                                initialsz = 0; /* trigger error below */
                                break;
                        }
                }
        }

        if(initialsz <= 0)
        {
                if(sopt)
                        fprintf(stderr, "Illegal size \"%s\"\n", sopt);
                else
                        fprintf(stderr, "No size specified\n");
                usage(av[0]);
        }

        if(Sopt != NULL)
        {
                nproducts = (size_t)atol(Sopt);
                if(nproducts == 0)
                {
                        fprintf(stderr, "Illegal nproducts \"%s\"\n", Sopt);
                }
        }
        else
        {
#define PQ_AVG_PRODUCT_SIZE 4096
                /* For default number of product slots, use average product size estimate */
                nproducts = initialsz/PQ_AVG_PRODUCT_SIZE;
        }


        /*
         * initialize logger
         */
        (void) setulogmask(logmask) ;
        (void)openulog(ubasename(av[0]),
                LOG_PID, LOG_LDM, logfname) ;

        if(verbose)
                fprintf(stderr, "Creating %s, %ld bytes, %ld products.\n",
                        pqfname, (long)initialsz, (long)nproducts);

        errnum = pq_create(pqfname, 0666, pflags,
                0, initialsz, nproducts, &pq);
        if(errnum)
        {
                fprintf(stderr, "%s: create \"%s\" failed: %s\n",
                        av[0], pqfname, strerror(errnum));
                exit(1);
        }

        (void)pq_close(pq);

        return(0);
}
