/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: forn5_svc.c,v 1.41.2.3.4.2.2.5 2008/04/15 16:34:13 steve Exp $ */

/* 
 *
 */

#include <ldmconfig.h>
#include <rpc/rpc.h>
#include <errno.h> /* EIO */
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "ldm.h"
#include "ulog.h"
#include "ldmprint.h"
#include "abbr.h"
#include "pq.h"
#include "ldm5_clnt.h"
#include "prod_class.h"
#include "peer_info.h"
#include "globals.h"

static timestampt maxlatency;

/*
 * "pktsz" used by comingsoon5 and blkdata5.
 * The default set here is reset from remote.sendsz below.
 */
static u_int feed_pktsz = DBUFMAX;

static int nohereis = 0;


static int
s_csbd(h_clnt *hcp, const prod_info *infop, const void *datap)
{
        ldm_replyt reply;
        enum clnt_stat rpc_stat;
        datapkt pkt;
        unsigned unsent;
        char *cp;

restart:
        /* TODO: mtu */
        rpc_stat = comingsoon5(hcp, infop, feed_pktsz, rpctimeo, &reply);
        if(rpc_stat != RPC_SUCCESS)
        {
                uerror("%s: %s",
                        infop->ident,
                        clnt_sperrno(rpc_stat));
                return EIO;
        }
        /* else */

        switch(reply.code) {
        case OK:
        case RESTART:   /* unexpected but okay ??? */
                break;
        case DONT_SEND:
                return ENOERR;
        case RECLASS:
        {
                prod_class_t *want = reply.ldm_replyt_u.newclssp;
                int status = update_remote_clss(want);
                unotice("RECLASS: %s", s_prod_class(NULL, 0, want));

                if(status != ENOERR)
                        return EIO;
                if(remote.clssp == NULL
                                || remote.clssp->psa.psa_len == 0)
                {
                        unotice("No match for request");
                        return EIO;
                }
                if(!clss_eq(remote.clssp, want))
                {
                        ldm_replyt reply2;

                        /*
                         * For now, the RECLASS should
                         * only be changing timestamps.
                         */
                        uerror("SHOULDN'T HAPPEN remote: %s",
                                 s_prod_class(NULL, 0, remote.clssp));

                        rpc_stat = hiya5(hcp, remote.clssp, rpctimeo, &reply2);
                        if(rpc_stat != RPC_SUCCESS)
                        {
                                uerror("RECLASS: hiya5 failed: %s",
                                        clnt_sperrno(rpc_stat));
                                return EIO;
                        }
                        if(reply2.code != OK)
                        {
                                uerror("reclass hiya5 returns: %s",
                                        s_ldm_errt(reply2.code));
                                return EIO;
                        }
                }
                if(!prodInClass(remote.clssp, infop))
                        return ENOERR;  /* He doesn't want this one */
                /* else, here we go */
                break;
        }
        default:
                uerror("%s: %s",
                        infop->ident,
                        s_ldm_errt(reply.code));
                return EIO;
        }

        pkt.signaturep = (signaturet *)&infop->signature; /* cast away const */
        pkt.pktnum = 0;

        for(cp = (char *)datap, unsent = infop->sz; unsent > 0;
                        unsent -= pkt.data.dbuf_len)
        {
                pkt.data.dbuf_len = unsent < feed_pktsz ? unsent : feed_pktsz;
                pkt.data.dbuf_val = cp;
                rpc_stat = blkdata5(hcp, &pkt, rpctimeo, &reply);
                if(rpc_stat != RPC_SUCCESS) {
                        uerror("%s:%d: blkdata5(%d): %s",
                                __FILE__, __LINE__, pkt.pktnum,
                                clnt_sperrno(rpc_stat));
                        return EIO;
                }
                switch(reply.code) {
                case OK:
                        break;
                case DONT_SEND:
                        return ENOERR;
                case RESTART:
                        goto restart;
                default:
                        uerror("%s: %s",
                                infop->ident,
                                s_ldm_errt(reply.code));
                        return EIO;
                }
                cp += pkt.data.dbuf_len;
                pkt.pktnum++;
        }

        /* else */
        if(ulogIsVerbose())
                uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));
        return ENOERR;
}


static int
s_xhereis(h_clnt *hcp, const prod_info *infop, const void *datap,
        void *xprod, size_t size)
{
        ldm_replyt reply;
        enum clnt_stat rpc_stat;
        
        rpc_stat = xhereis5(hcp, xprod, size, rpctimeo, &reply);        
        if(rpc_stat == RPC_PROCUNAVAIL)
        {
                udebug("RPC_PROCUNAVAIL");
                nohereis = 1;
                return s_csbd(hcp, infop, datap);
        }

        if(rpc_stat != RPC_SUCCESS)
        {
                uerror("%s: %s (%d)",
                        infop->ident,
                        clnt_sperrno(rpc_stat), (int)rpc_stat);
                return EIO;
        }
        /* else */

        switch(reply.code) {
        case OK:
        case RESTART:   /* unexpected but okay ??? */
        case DONT_SEND:
                break;
        case RECLASS:
        {
                prod_class_t *want = reply.ldm_replyt_u.newclssp;
                int status = update_remote_clss(want);
                unotice("RECLASS: %s", s_prod_class(NULL, 0, want));

                if(status != ENOERR)
                        return EIO;
                if(remote.clssp == NULL
                                || remote.clssp->psa.psa_len == 0)
                {
                        unotice("No match for request");
                        return EIO;
                }
                if(!clss_eq(remote.clssp, want))
                {
                        ldm_replyt reply2;

                        /*
                         * For now, the RECLASS should
                         * only be changing timestamps.
                         */
                        uerror("SHOULDN'T HAPPEN remote: %s",
                                 s_prod_class(NULL, 0, remote.clssp));

                        rpc_stat = hiya5(hcp, remote.clssp, rpctimeo, &reply2);
                        if(rpc_stat != RPC_SUCCESS)
                        {
                                uerror("RECLASS: hiya5 failed: %s",
                                        clnt_sperrno(rpc_stat));
                                return EIO;
                        }
                        if(reply2.code != OK)
                        {
                                uerror("reclass hiya5 returns: %s",
                                        s_ldm_errt(reply2.code));
                                return EIO;
                        }
                }
                return ENOERR;  /* but he got it anyway */
        }
        default:
                uerror("%s: %s",
                        infop->ident,
                        s_ldm_errt(reply.code));
                return EIO;
        }
        return ENOERR;
}


/*ARGSUSED*/
int
feed5_sqf(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *vp)
{
        h_clnt *hcp = (h_clnt *)vp;

        if(infop->sz > feed_pktsz || nohereis)
                return s_csbd(hcp, infop, datap);
        /* else */
        return s_xhereis(hcp, infop, datap, xprod, size);
}


/*
 */
/*ARGSUSED*/
int
noti5_sqf(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *vp)
{
        ldm_replyt reply;
        h_clnt *hcp = (h_clnt *)vp;
        enum clnt_stat rpc_stat;

        rpc_stat = notification5(hcp, infop, rpctimeo, &reply);
        if(rpc_stat != RPC_SUCCESS)
        {
                uerror("%s: %s",
                        infop->ident,
                        clnt_sperrno(rpc_stat));
                return EIO;
        }
        /* else */

        if(reply.code == RECLASS)
        {
                prod_class_t *want = reply.ldm_replyt_u.newclssp;
                int status = update_remote_clss(want);
                unotice("RECLASS: %s", s_prod_class(NULL, 0, want));
                if(status != ENOERR)
                        return EIO;
                if(remote.clssp == NULL
                                || remote.clssp->psa.psa_len == 0)
                {
                        unotice("No match for request %s");
                        return EIO;
                }
                if(!clss_eq(remote.clssp, want))
                {
                        ldm_replyt reply2;

                        /*
                         * For now, the RECLASS should
                         * only be changing timestamps.
                         */
                        uerror("SHOULDN'T HAPPEN remote: %s",
                                 s_prod_class(NULL, 0, remote.clssp));

                        rpc_stat = hiya5(hcp, remote.clssp, rpctimeo, &reply2);
                        if(rpc_stat != RPC_SUCCESS)
                        {
                                uerror("RECLASS: hiya5 failed: %s",
                                        clnt_sperrno(rpc_stat));
                                return EIO;
                        }
                        if(reply.code != OK)
                        {
                                uerror("reclass hiya5 returns: %s",
                                        s_ldm_errt(reply2.code));
                                return EIO;
                        }
                }
                /* else, here we go */
        }
        else if(reply.code != OK)
        {
                uerror("%s: %s",
                        infop->ident,
                        s_ldm_errt(reply.code));
                return EIO;
        }

        /* else */
        if(ulogIsVerbose())
                uinfo("%s", s_prod_info(NULL, 0, infop, ulogIsDebug()));
        return 0;
}


ldm_replyt *
forn_5_svc(prod_class_t *want, struct svc_req *rqstp, const char *ident,
        pq_seqfunc *doit)
{
        static ldm_replyt theReply;
        h_clnt hc;
        enum clnt_stat rpc_stat;
        int status;
        pq_match mt = TV_GT;
        /* Used for keepalive. Time when the last rpc completed */
        timestampt lastsent;
        /* used in keepalive calculation */
        timestampt now;
        const int keepalive_interval = (int)(inactive_timeo/2 - 2 * interval);

        /* assert(keepalive_interval > 0); */
        
        (void)memset(&theReply, 0, sizeof(theReply));

        if(done)
        {
                theReply.code = SHUTTING_DOWN;
                return(&theReply);
        }

        switch(update_remote_clss(want)) {
        case ENOERR:
                break;
        case EINVAL:
                theReply.code = BADPATTERN;
                return(&theReply);
        default:
                svcerr_systemerr(rqstp->rq_xprt);
                return NULL;
        }

        if(remote.clssp == NULL
                        || remote.clssp->psa.psa_len == 0)
        {
                unotice("No match for request %s",
                        s_prod_class(NULL, 0, want));
                /* ??? */
                svcerr_weakauth(rqstp->rq_xprt);
                return NULL;
        }

        if(!clss_eq(remote.clssp, want))
        {
                theReply.code = RECLASS;
                if (ulogIsVerbose())
                    uinfo("reclss: %s: %s",
                            remote_name(),
                            s_prod_class(NULL, 0, remote.clssp));
                theReply.ldm_replyt_u.newclssp = remote.clssp;
                return(&theReply);
        }

        /*
         * Ensure that the product queue is open for reading only.  It will be 
         * closed during process termination by cleanup().
         */
        if (NULL != pq) {
            (void) pq_close(pq);
            pq = NULL;
        }
        status = pq_open(pqfname, PQ_READONLY, &pq);
        if(status)
        {
                if (PQ_CORRUPT == status) {
                    uerror("The product-queue \"%s\" is inconsistent\n",
                            pqfname);
                }
                else {
                    uerror("pq_open failed: %s: %s\n",
                            pqfname, strerror(status)) ;
                }
                svcerr_systemerr(rqstp->rq_xprt);
                return NULL;
        }

        /* Set the ulog identifer, optional. */
        if(ident != NULL && *ident != 0)
                set_abbr_ident(remote_name(), ident);
        unotice("Starting Up(%s/5): %s", 
            ldm_version, s_prod_class(NULL, 0, remote.clssp));

        unotice("topo:  %s %s", remote_name(),
                        s_feedtypet(clss_feedtypeU(remote.clssp)));

        /* else, theReply.code == OK */
        if(!svc_sendreply(rqstp->rq_xprt, (xdrproc_t)xdr_ldm_replyt, 
            (caddr_t)&theReply))
        {
                svcerr_systemerr(rqstp->rq_xprt);
        }
        if(!svc_freeargs(rqstp->rq_xprt, xdr_prod_class, (caddr_t)want))
        {
                uerror("unable to free arguments");
                exit(1);
        }

        /*
         * Wait a second before sending anything to the downstream LDM.
         */
        (void)sleep(1);

        /*
         * Change the SVCXPRT into an h_clnt.
         */
        if(h_xprt_turn(&hc, remote_name(), rqstp->rq_xprt,
                        remote.sendsz, remote.recvsz) < H_CLNTED)
        {
                uerror("%s", s_hclnt_sperrno(&hc));
                exit(1);
        }

        /* Set the "pktsz" used by comingsoon5/blkdata5 */
        feed_pktsz = remote.sendsz - DATAPKT_RPC_OVERHEAD;
        if(feed_pktsz > DBUFMAX)
                feed_pktsz = DBUFMAX;
        udebug("feed_pktsz %u", feed_pktsz);

        status =  pq_cClassSet(pq,  &mt, remote.clssp);
        if(status)
        {
                uerror("pq_cClassSet failed: %s: %s\n",
                        pqfname, strerror(status)) ;
                exit(1);
        }
        
        lastsent = TS_ZERO;
        hc.begin = TS_ZERO;
        hc.elapsed = TS_ZERO;
        while(exitIfDone(0))
        {
                status = pq_sequence(pq, mt, remote.clssp, doit, &hc);

                switch(status) {
                case 0: /* no error */
                        lastsent = timestamp_add(&hc.begin, &hc.elapsed);
                        continue; /* N.B., other cases sleep */
                case PQUEUE_END:
                        udebug("End of Queue");
                        if(!pq_ctimeck(pq, mt, remote.clssp, &maxlatency))
                        {
                                unotice("Request Satisfied");
                                done = 1;
                                continue;
                        }
                        (void)set_timestamp(&now);
                        if(d_diff_timestamp(&now, &lastsent)
                                         < keepalive_interval)
                                break; /* don't bother, yet */
                        /* else, send the keepalive */
                        rpc_stat = nullproc5(&hc, rpctimeo);
                        if(rpc_stat != RPC_SUCCESS)
                        {
                                uerror("nullproc5(%s): %s",
                                        remote_name(), clnt_sperrno(rpc_stat));
                                done = 1;
                                continue;
                        }
                        lastsent = timestamp_add(&hc.begin, &hc.elapsed);
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

                pq_suspend(interval);
                        
        }
        
        exit(0);

        /*NOTREACHED*/
        return NULL;
}
