/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldmsend.c,v 1.15.2.4.2.2.4.8 2008/04/15 16:34:11 steve Exp $ */

/* 
 * ldm client to ship files
 */

#ifdef PORTMAP
/* SVR4 tirpc, but this prog doesn't use the old interfaces */
#undef PORTMAP
#define TIRPC
#endif
#include <ldmconfig.h>

#include "ldm.h"
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

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
        /* default to using the "experimental" feedtype for reported statistics*/
#define DEFAULT_FEEDTYPE EXP
#endif

const char *remote = NULL; /* hostname of data remote */
extern unsigned remotePort;
static int signed_on_hiya=0;
static CLIENT *clnt = NULL;
static int     sock = -1;
static int   (*hiya)   (CLIENT* clnt, prod_class_t** clsspp);
static void  (*send_product)(
    CLIENT*          clnt,
    const char*      statsdata,
    const prod_info* infop);
static void* (*nullproc)(void* arg, CLIENT *clnt);
static max_hereis_t max_hereis;
static unsigned     version;


/* Begin Convenience functions */
static struct timeval timeo = {25, 0}; /* usual RPC default */
static ldm_replyt reply;

static enum clnt_stat
my_comingsoon_5(
        CLIENT*     clnt,
        const prod_info* const  infop,
        u_int       pktsz,
        ldm_replyt* replyp)
{
        comingsoon_args arg;
        arg.infop = (prod_info*)infop;
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
                        if (ulogIsVerbose())
                                uinfo("%s: reclass: %s",
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
                if (ulogIsVerbose())
                    uinfo("%s: reclass: %s",
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



void ldmsend_clnt_destroy()
{
    if (clnt != NULL) {
        if (NULL != nullproc && NULL == (*nullproc)(NULL, clnt)) {
            uerror("%s: NULLPROC failure: %s", remote,
                clnt_errmsg(clnt));
        }

        auth_destroy(clnt->cl_auth);
        clnt_destroy(clnt);
        (void)close(sock);

        clnt = NULL;
        sock = -1;
    }
}


/*
 * Send a product
 * from descriptor fd to clnt using LDM-5 protocols.
 */
static void
send_product_5(
    CLIENT* const          clnt,
    const char* const      statsdata,
    const prod_info* const infop)
{
        enum clnt_stat rpc_stat;
        datapkt        pkt;
        ssize_t unsent;
        size_t nread;
        const char* buf;

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

        pkt.signaturep = (signaturet*)infop->signature;
        pkt.pktnum = 0;

        buf = statsdata;
        for(unsent = (ssize_t)infop->sz; unsent > 0;
                        unsent -= nread )
        {

                if(strlen(statsdata) > DBUFMAX)
                   nread = DBUFMAX;
                else
                   nread = strlen(statsdata);

                pkt.data.dbuf_len = (u_int)nread;
                pkt.data.dbuf_val = (char*)buf;         /* remove "const" */
                rpc_stat = my_blkdata_5(clnt, &pkt, &reply);
                if(rpc_stat != RPC_SUCCESS)
                        break;
                if(reply.code != OK)
                        break;
                pkt.pktnum++;
                buf += nread;
        }
}


/*
 * Send a product to clnt using LDM-6 protocols.
 */
static void
send_product_6(
    CLIENT* const          clnt,
    const char* const      statsdata,
    const prod_info* const infop)
{
    unsigned size = infop->sz;

    if (size <= max_hereis) {
        /*
         * The file is small enough to be sent in a single HEREIS message.
         */
        product product;

        udebug("Sending file via HEREIS");

        product.info = *infop;
        product.data = (void*)statsdata;

        if (NULL == hereis_6(&product, clnt)) {
            uerror("%s: HEREIS_6 failure: %s",
                remote, clnt_errmsg(clnt));
        }
    }
    else {
        /*
         * The product is so large that it must be sent via COMINGSOON/BLKDATA 
         * messages.
         */
        comingsoon_reply_t* reply;
        comingsoon_args     soonArg;

        udebug("Sending file via COMINGSOON/BLKDATA");

        soonArg.infop = (prod_info*)infop;              /* remove "const" */
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
                datapkt packet;

                /* Remove "const" from following. */
                packet.signaturep = (signaturet*)&infop->signature;
                packet.pktnum = 0;
                packet.data.dbuf_len = size;
                packet.data.dbuf_val = (void*)statsdata;

                if (NULL == blkdata_6(&packet, clnt)) {
                    uerror("%s: BLKDATA_6 failure: %s",
                        remote, clnt_errmsg(clnt));
                }
            }
        }
    }
}


static int
ldmsend(
    CLIENT*     clnt,
    prod_class_t* clssp,
    char*       origin,
    int*        seq_start,
    char*       statsdata)
{
    int         status = 0;
    int         idlen;
    int         icnt;
    char        filename[255];
    char        feedid[80]="\0";
    char        prodo[HOSTNAMESIZE]="\0";
    char*       cpos;
    prod_info   info;
    MD5_CTX*    md5ctxp = NULL;
    prod_class_t* test_clssp = clssp;

    /* ldmproduct "filename" length = 255
     * assert that sprintf(filename,"rtstats-%s/%s/%s/%s\0",ldm_version,
     * origin,feedid,prodo); will fit into allocated space.
     */
    assert ( ( strlen(ldm_version) + 2 * HOSTNAMESIZE + 80 + 9 ) < 255 );

    /*
     * time_insert time_arrive myname feedid product_origin
     */
    icnt = 0;   
    cpos = statsdata;

    while ( ( (cpos = (char *)strchr(cpos,' ')) != NULL ) && ( icnt < 4 ) ) {
       icnt++;
       while ( cpos[0] == ' ' ) cpos++;
       if ( icnt == 3 ) {
          idlen = strcspn(cpos," ");
          if(idlen > 79) idlen = 79;
          strncat(feedid,cpos,idlen);
       }
       if ( icnt == 4 ) {
          idlen = strcspn(cpos," ");
          if(idlen > HOSTNAMESIZE) idlen = HOSTNAMESIZE;
          strncat(prodo,cpos,idlen);
       }
    }

    /*
     * Allocate an MD5 context
     */
    md5ctxp = new_MD5_CTX();
    if(md5ctxp == NULL)
    {
        status = errno;
        serror("new_md5_CTX failed");
    }
    else {
        if(signed_on_hiya) {
           udebug("already signed on");
        }
        else {
            status = (*hiya)(clnt, &clssp);

            if(status == 0)
                signed_on_hiya = 1;
        }

        if(status == 0) {
            /* These members are constant over the loop. */
            info.origin = origin;
            info.feedtype = clssp->psa.psa_val->feedtype;

            info.seqno = *seq_start;

            sprintf(filename,"rtstats-%s/%s/%s/%s\0",ldm_version,origin,
                feedid,prodo);
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
                if(test_clssp != clssp) free_prod_class(clssp);
            }
            else {
                uinfo("Sending %s, %d bytes", filename, strlen(statsdata));
                
                MD5Init(md5ctxp);
                MD5Update(md5ctxp, (unsigned char *)statsdata,
                    (unsigned int)strlen(statsdata));
                MD5Final((unsigned char*)info.signature, md5ctxp);

                info.sz = (u_int)strlen(statsdata);

                (*send_product)(clnt, statsdata, &info);
            }
        }

        free_MD5_CTX(md5ctxp);  
    }                                   /* MD5 object allocated */
            
    if (test_clssp != clssp)
        free_prod_class(clssp);

    (*seq_start)++;

    return status;
}

int ldmsend_main(char *statsdata)
{
    char         myname[HOSTNAMESIZE];
    prod_class_t   clss;
    prod_spec    spec;
    static int   seq_start = 0;
    int          status = 0;            /* success */
    ErrorObj*     error;
    
    clss.from = TS_ZERO;
    clss.to = TS_ENDT;
    clss.psa.psa_len = 1;
    clss.psa.psa_val = &spec;
    spec.feedtype = DEFAULT_FEEDTYPE;
    spec.pattern = ".*";

    (void) strcpy(myname, ghostname());

    if (NULL == clnt) {

        /*
         * Connect to the LDM server.
         */
        error = ldm_clnttcp_create_vers(remote, remotePort, SIX, &clnt, NULL,
            NULL);

        if (!error) {
            version = SIX;
        }
        else if (LDM_CLNT_BAD_VERSION == err_code(error)) {
            err_free(error);

            error = ldm_clnttcp_create_vers(remote, LDM_PORT, FIVE, &clnt,
                NULL, NULL);

            if (!error)
                version = FIVE;
        }

        if (!error) {
            signed_on_hiya = 0;

            udebug("version = %u", version);
        }
        else {
            err_log_and_free(error, ERR_WARNING);

            status = -1;
        }
    }

    if (NULL != clnt) {
        if (5 == version) {
            hiya = my_hiya_5;
            send_product = send_product_5;
            nullproc = NULL;
        }
        else if (6 == version) {
            hiya = my_hiya_6;
            send_product = send_product_6;
            nullproc = nullproc_6;
        }
        else {
            abort();
        }

        status = ldmsend(clnt, &clss, myname, &seq_start, statsdata);

        if (seq_start > 999)
            seq_start = 0;
    }

    return status;
}
