/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldmsend.c,v 1.71.2.2.6.1.4.9 2008/04/15 16:34:12 steve Exp $ */

/* 
 * ldm client to ship files
 */

#ifdef PORTMAP
/* SVR4 tirpc, but this prog doesn't use the old interfaces */
#undef PORTMAP
#define TIRPC
#endif
#include <ldmconfig.h>

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "ldm.h"        /* needed by following */
#include "atofeedt.h"
#include "error.h"
#include "globals.h"
#include "inetutil.h"
#include "ldm_clnt.h"
#include "ldmprint.h"
#include "md5.h"
#include "prod_class.h"
#include "rpcutil.h"
#include "ulog.h"

#ifdef NO_ATEXIT
#include "atexit.h"
#endif

#ifndef DEFAULT_FEEDTYPE
        /* default to using the "experimental" feedtype */
#define DEFAULT_FEEDTYPE EXP
#endif

static const char *remote = NULL; /* hostname of data remote */
static CLIENT *clnt = NULL;
static int   (*hiya)   (CLIENT* clnt, prod_class_t** clsspp);
static void  (*send_product)(CLIENT *clnt, int fd, prod_info *infop);
static void* (*nullproc)(void* arg, CLIENT *clnt);
static max_hereis_t max_hereis;


/* Begin Convenience functions */
static struct timeval timeo = {25, 0}; /* usual RPC default */

static enum clnt_stat
my_comingsoon_5(
        CLIENT *clnt,
        prod_info *infop,
        u_int pktsz,
        ldm_replyt *replyp)
{
        comingsoon_args arg;
        arg.infop = infop;
        arg.pktsz = pktsz;

        memset(replyp, 0, sizeof(ldm_replyt));

        return clnt_call(clnt, COMINGSOON,
                xdr_comingsoon_args, (caddr_t)&arg,
                xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}

static enum clnt_stat
my_blkdata_5(CLIENT *clnt, datapkt *dpkp, ldm_replyt *replyp)
{
        memset(replyp, 0, sizeof(ldm_replyt));

        return clnt_call(clnt, BLKDATA,
                xdr_datapkt, (caddr_t)dpkp,
                xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}

static int
my_hiya_5(CLIENT *clnt, prod_class_t **clsspp)
{
        static ldm_replyt reply;
        enum clnt_stat rpc_stat;

        memset(&reply, 0, sizeof(ldm_replyt));

        rpc_stat = clnt_call(clnt, HIYA,
                xdr_prod_class, (caddr_t)*clsspp,
                xdr_ldm_replyt, (caddr_t)&reply,
                timeo);

        if(rpc_stat != RPC_SUCCESS)
        {
                uerror("hiya %s:  %s", remote, clnt_sperrno(rpc_stat));
                return ECONNABORTED; /* Perhaps could be more descriptive */
        }
        switch (reply.code) {
                case OK:
                        break;
                case SHUTTING_DOWN:
                        uerror("%s is shutting down", remote);
                        return ECONNABORTED;
                case DONT_SEND:
                case RESTART:
                case REDIRECT: /* TODO */
                default:
                        uerror("%s: unexpected reply type %s",
                                remote, s_ldm_errt(reply.code));
                        return ECONNABORTED;
                case RECLASS:
                        *clsspp = reply.ldm_replyt_u.newclssp;
                        clss_regcomp(*clsspp);
                        /* N.B. we use the downstream patterns */
                        unotice("%s: reclass: %s",
                                remote, s_prod_class(NULL, 0, *clsspp));
                        break;
        }
        return 0;
}

static int
my_hiya_6(CLIENT *clnt, prod_class_t **clsspp)
{
    static hiya_reply_t* reply;
    int                  error;
    
    reply = hiya_6(*clsspp, clnt);

    if (NULL == reply) {
        uerror("%s: HIYA_6 failure: %s", remote, clnt_errmsg(clnt));

        error = ECONNABORTED;
    }
    else {
        switch (reply->code) {
            case OK:
                max_hereis = reply->hiya_reply_t_u.max_hereis;
                error = 0;
                break;

            case SHUTTING_DOWN:
                uerror("%s: LDM shutting down", remote);
                error = ECONNABORTED;
                break;

            case BADPATTERN:
                uerror("%s: Bad product-class pattern", remote);
                error = ECONNABORTED;
                break;

            case DONT_SEND:
                uerror("%s: LDM says don't send", remote);
                error = ECONNABORTED;
                break;

            case RESEND:
                uerror("%s: LDM says resend (ain't gonna happen)", remote);
                error = ECONNABORTED;
                break;

            case RESTART:
                uerror("%s: LDM says restart (ain't gonna happen)", remote);
                error = ECONNABORTED;
                break;

            case REDIRECT:
                uerror("%s: LDM says redirect (ain't gonna happen)", remote);
                error = ECONNABORTED;
                break;

            case RECLASS:
                *clsspp = reply->hiya_reply_t_u.feedPar.prod_class;
                max_hereis = reply->hiya_reply_t_u.feedPar.max_hereis;
                clss_regcomp(*clsspp);
                /* N.B. we use the downstream patterns */
                unotice("%s: reclass: %s",
                        remote, s_prod_class(NULL, 0, *clsspp));
                error = 0;
                break;
        }

        if (!error)
            udebug("max_hereis = %u", max_hereis);
    }

    return error;
}


/* End Convenience functions */


static void
usage(
        char *av0 /*  id string */
)
{
        (void)fprintf(stderr,
                "Usage: %s [options] filename ...\n\tOptions:\n", av0);
        (void)fprintf(stderr,
                "\t-v           Verbose, tell me about each product\n");
        (void)fprintf(stderr,
                "\t-l logfile   log to a file rather than stderr\n");
        (void)fprintf(stderr,
                "\t-h remote    remote service host, defaults to \"localhost\"\n");
        (void)fprintf(stderr,
                "\t-s seqno     set initial product sequence number to \"seqno\", defaults to 0\n");
        (void)fprintf(stderr,
                "\t-f feedtype  assert your feed type as \"feedtype\", defaults to \"%s\"\n", s_feedtypet(DEFAULT_FEEDTYPE));
        exit(1);
}


void
cleanup(void)
{
        if(clnt != NULL)
        {
                clnt_destroy(clnt);
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
         exit(1);
         return;
      case SIGTERM :
         done = 1;
         return;
      case SIGPIPE :
         exit(1);
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
        (void) sigaction(SIGINT, &sigact, NULL);

        sigact.sa_flags |= SA_RESTART;
        (void) sigaction(SIGTERM, &sigact, NULL);
        (void) sigaction(SIGPIPE, &sigact, NULL);

        sigact.sa_handler = SIG_IGN;
        (void) sigaction(SIGALRM, &sigact, NULL);
#else
        (void) signal(SIGINT, signal_handler);
        (void) signal(SIGTERM, signal_handler);
        (void) signal(SIGPIPE, signal_handler);
        (void) signal(SIGALRM, SIG_IGN);
#endif
}


/*
 * Send a product from file-descriptor to clnt using LDM-5 protocol.
 */
static void
send_product_5(CLIENT *clnt, int fd, prod_info *infop)
{
        static ldm_replyt reply;
        enum clnt_stat rpc_stat;
        datapkt pkt;
        ssize_t unsent;
        ssize_t nread;
        char buf[DBUFMAX];

        rpc_stat = my_comingsoon_5(clnt, infop, DBUFMAX, &reply);
        if(rpc_stat != RPC_SUCCESS)
        {
                uerror("send_product_5: %s %s",
                        infop->ident,
                        clnt_sperrno(rpc_stat));
                return;
        }
        /* else */

        if(reply.code != OK)
        {
                if(reply.code == DONT_SEND)
                   uinfo("send_product_5: %s: %s",
                        infop->ident,
                        s_ldm_errt(reply.code));
                else
                   uerror("send_product_5: %s: %s",
                        infop->ident,
                        s_ldm_errt(reply.code));
                return;
        }

        pkt.signaturep = &infop->signature;
        pkt.pktnum = 0;

        for(unsent = (ssize_t)infop->sz; unsent > 0;
                        unsent -= nread )
        {
                nread = read(fd, buf, DBUFMAX);
                if(nread <= 0)
                {
                        serror("read: %s (seqno %d)",
                                infop->ident, infop->seqno);
                        break;
                } /* else */
                pkt.data.dbuf_len = (u_int)nread;
                pkt.data.dbuf_val = buf;
                rpc_stat = my_blkdata_5(clnt, &pkt, &reply);
                if(rpc_stat != RPC_SUCCESS)
                        break;
                if(reply.code != OK)
                        break;
                pkt.pktnum++;
        }
}


/*
 * Send a product from file-descriptor to clnt using LDM-6 protocol.
 */
static void
send_product_6(CLIENT *clnt, int fd, prod_info *infop)
{
    unsigned size = infop->sz;

    if (size <= max_hereis) {
        /*
         * The file is small enough to be sent in a single HEREIS message.
         */
        void*   buf = (char*)malloc(size);

        udebug("Sending file via HEREIS");

        if (NULL == buf) {
            serror("Couldn't allocate %u bytes for product data", size);
        }
        else {
            ssize_t nread = read(fd, buf, size);

            if(nread != size) {
                serror("Couldn't read %u bytes of data", size);
            }
            else {
                product product;

                product.info = *infop;
                product.data = buf;

                if (NULL == hereis_6(&product, clnt)) {
                    uerror("%s: HEREIS_6 failure: %s",
                        remote, clnt_errmsg(clnt));
                }
            }

            free(buf);
        }
    }
    else {
        /*
         * The file is so large that it must be sent via COMINGSOON/BLKDATA 
         * messages.
         */
        comingsoon_reply_t* reply;
        comingsoon_args     soonArg;

        udebug("Sending file via COMINGSOON/BLKDATA");

        soonArg.infop = infop;
        soonArg.pktsz = size;
        
        reply = comingsoon_6(&soonArg, clnt);

        if (NULL == reply) {
            uerror("%s: COMINGSOON_6 failure: %s", remote, clnt_errmsg(clnt));
        }
        else {
            if (DONT_SEND == *reply) {
                if (ulogIsVerbose() || ulogIsDebug())
                    uinfo("Downstream LDM says don't send: %s",
                        s_prod_info(NULL, 0, infop, ulogIsDebug()));
            }
            else if (0 != *reply) {
                uwarn("Unexpected reply (%s) from downstream LDM: %s",
                    s_prod_info(NULL, 0, infop, ulogIsDebug()));
            }
            else {
                void*   buf = (char*)malloc(size);

                if (NULL == buf) {
                    serror("Couldn't allocate %u bytes for product data", 
                        size);
                }
                else {
                    ssize_t nread = read(fd, buf, size);

                    if(nread != size) {
                        serror("Couldn't read %u bytes of data", size);
                    }
                    else {
                        datapkt packet;

                        packet.signaturep = (signaturet*)&infop->signature;
                        packet.pktnum = 0;
                        packet.data.dbuf_len = size;
                        packet.data.dbuf_val = buf;

                        if (NULL == blkdata_6(&packet, clnt)) {
                            uerror("%s: BLKDATA_6 failure: %s",
                                remote, clnt_errmsg(clnt));
                        }
                    }

                    free(buf);
                }
            }
        }
    }
}


static int
fd_md5(MD5_CTX *md5ctxp, int fd, off_t st_size, signaturet signature)
{
        ssize_t nread;
        char buf[8192];
        MD5Init(md5ctxp);

        for(; exitIfDone(1) && st_size > 0; st_size -= (off_t)nread )
        {
                nread = read(fd, buf, sizeof(buf));
                if(nread <= 0)
                {
                        serror("fd_md5: read");
                        return -1;
                } /* else */
                MD5Update(md5ctxp, (unsigned char *)buf, (unsigned int)nread);
        }

        MD5Final((unsigned char*)signature, md5ctxp);
        return 0;
}


static int
ldmsend(CLIENT *clnt,
        prod_class_t* clssp,
        char*       origin,
        int         seq_start,
        int         nfiles,
        char*       filenames[])
{
        int status = 0;
        char *filename;
        int fd;
        struct stat statb;
        prod_info info;
        MD5_CTX *md5ctxp = NULL;

        /*
         * Allocate an MD5 context
         */
        md5ctxp = new_MD5_CTX();
        if(md5ctxp == NULL)
        {
                status = errno;
                serror("new_md5_CTX failed");
                return status;
        }

        status = (*hiya)(clnt, &clssp);

        if(status != 0)
                return status;

        /* These members are constant over the loop. */
        info.origin = origin;
        info.feedtype = clssp->psa.psa_val->feedtype;

        for( info.seqno = seq_start; exitIfDone(1) && nfiles > 0;
                 filenames++, nfiles--, info.seqno++)
        {
                filename = *filenames;
                info.ident = filename;
                /*
                 * ?? This could be the creation time of the file.
                 */
                (void) set_timestamp(&info.arrival);

                /*
                 * Checks 'arrival', 'feedtype', and 'ident'
                 * against what the other guy has said he wants.
                 */
                if(!prodInClass(clssp, &info))
                {
                        uinfo("%s doesn't want %s", remote, filename);
                        continue;       
                }

                fd = open(filename, O_RDONLY, 0);
                if(fd == -1)
                {
                        serror("open: %s", filename);
                        continue;
                }

                if( fstat(fd, &statb) == -1) 
                {
                        serror("fstat: %s", filename);
                        (void) close(fd);
                        continue;
                }

                uinfo("Sending %s, %d bytes", filename, statb.st_size);
                
                /* These members, and seqno, vary over the loop. */
                if(fd_md5(md5ctxp, fd, statb.st_size, info.signature) != 0)
                {
                        (void) close(fd);
                        continue;
                }
                if(lseek(fd, 0, SEEK_SET) == (off_t)-1)
                {
                        serror("rewind: %s", filename);
                        (void) close(fd);
                        continue;
                }

                (void)exitIfDone(1);

                info.sz = (u_int)statb.st_size;

                (*send_product)(clnt, fd, &info);

                (void) close(fd);
        }

        if (exitIfDone(1) && NULL != nullproc &&
                NULL == (*nullproc)(NULL, clnt)) {
            uerror("%s: NULLPROC failure: %s", remote, clnt_errmsg(clnt));

            status = 1;
        }

        free_MD5_CTX(md5ctxp);  
        return status;
}


int main(int ac, char *av[])
{
        char myname[HOSTNAMESIZE];
        char *progname = av[0];
        char *logfname;
        unsigned version;
        prod_class_t clss;
        prod_spec spec;
        int seq_start = 0;
        int status;
        ErrorObj* error;
        unsigned remotePort = LDM_PORT;
        
        logfname = "-";
        remote = "localhost";

        (void)set_timestamp(&clss.from);
        clss.to = TS_ENDT;
        clss.psa.psa_len = 1;
        clss.psa.psa_val = &spec;
        spec.feedtype = DEFAULT_FEEDTYPE;
        spec.pattern = ".*";

        {
        extern int optind;
        extern char *optarg;
        int ch;
        int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE));

        while ((ch = getopt(ac, av, "vxl:h:f:P:s:")) != EOF)
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
                case 'f':
                        spec.feedtype = atofeedtypet(optarg);
                        if(spec.feedtype == NONE)
                        {
                            fprintf(stderr, "Unknown feedtype \"%s\"\n", optarg);
                                usage(progname);        
                        }
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
                case 's':
                        seq_start = atoi(optarg);
                        break;
                case '?':
                        usage(progname);
                        break;
                }

        ac -= optind; av += optind;

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
                exit(1);
        }

        /*
         * set up signal handlers
         */
        set_sigactions();

        (void) strcpy(myname, ghostname());

        /*
         * Contact the server.
         */
        error = ldm_clnttcp_create_vers(remote, remotePort, SIX, &clnt,
                NULL, NULL);

        (void)exitIfDone(1);

        if (!error) {
            version = SIX;
            hiya = my_hiya_6;
            send_product = send_product_6;
            nullproc = nullproc_6;
        }
        else if (LDM_CLNT_BAD_VERSION == err_code(error)) {
            err_free(error);

            error = ldm_clnttcp_create_vers(remote, remotePort, FIVE, &clnt,
                    NULL, NULL);

            (void)exitIfDone(1);

            if (!error) {
                version = FIVE;
                hiya = my_hiya_5;
                send_product = send_product_5;
                nullproc = NULL;
            }
        }

        if (error) {
            err_log(error, ERR_FAILURE);
            err_free(error);
            status = 1;
        }
        else {
            udebug("version %u", version);

            status = ldmsend(clnt, &clss, myname, seq_start, ac, av);
        }

        return status != 0; 
}
