/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldm5_clnt.c,v 5.69.2.3.2.1.4.10 2008/04/15 16:34:11 steve Exp $ */

/*
 * ldm client side functions
 */

#include "ldmconfig.h"

#include <errno.h>
#include <rpc/rpc.h>
#include <string.h>
#include <unistd.h>

#include "ldm5.h"
#include "globals.h"
#include "h_clnt.h"
#include "ldmprint.h"
#include "prod_class.h"
#include "pq.h"
#include "savedInfo.h"
#include "ulog.h"

#include "ldm5_clnt.h"

/*
 * clnt side NULLPROC
 */
enum clnt_stat
nullproc5(h_clnt *hcp, unsigned int timeo)
{
        return h_clnt_call(hcp, NULLPROC,
                (xdrproc_t)xdr_void, NULL,
                (xdrproc_t)xdr_void, NULL,
                timeo);
}


/*
 * Ship a smallish (< DBUFMAX) data product.
 */
enum clnt_stat
hereis5( h_clnt *hcp, product *prod, unsigned int timeo, 
        ldm_replyt *replyp)
{
        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, HEREIS,
                (xdrproc_t)xdr_product, (caddr_t)prod,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}


/* Begin XHEREIS */
/*
 * The next three declarations are tightly coupled.
 * They allow us to send an already xdr'ed product
 * directly from the arena.
 */
static u_int xlen_prod = 0;

static bool_t
xdr_xprod(XDR *xdrs, caddr_t xprod)
{
        if(xdrs->x_op != XDR_ENCODE)
                return FALSE; /* assert(xdrs->x_op == XDR_ENCODE) */
        return XDR_PUTBYTES(xdrs, xprod, xlen_prod);
}

/*
 * Ship a smallish data product using HEREIS rpc
 */
enum clnt_stat
xhereis5(h_clnt *hcp, void *xprod, size_t size, unsigned int timeo,
        ldm_replyt *replyp)
{
        xlen_prod = (u_int)size;

        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, HEREIS,
                (xdrproc_t)xdr_xprod, (caddr_t)xprod,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}
/* End XHEREIS */


/*
 * 
 */ 
enum clnt_stat
feedme5(h_clnt *hcp, const prod_class *clssp, unsigned int timeo,
        ldm_replyt *replyp) 
{
        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, FEEDME,
                (xdrproc_t)xdr_prod_class, (caddr_t)clssp,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}


/*
 * clnt side HIYA
 */ 
enum clnt_stat
hiya5(h_clnt *hcp, const prod_class *clssp, unsigned int timeo,
        ldm_replyt *replyp) 
{
        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, HIYA,
                (xdrproc_t)xdr_prod_class, (caddr_t)clssp,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}


/*
 * 
 */
enum clnt_stat
notification5(h_clnt *hcp, const prod_info *infop,
        unsigned int timeo, ldm_replyt *replyp)
{
        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, NOTIFICATION,
                (xdrproc_t)xdr_prod_info, (caddr_t)infop,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}


/*
 *
 */ 
enum clnt_stat
notifyme5(h_clnt *hcp, const prod_class *clssp, unsigned int timeo,
        ldm_replyt *replyp) 
{
        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, NOTIFYME,
                (xdrproc_t)xdr_prod_class, (caddr_t)clssp,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}


/*
 * To send a data product is a minimum of two transactions.
 * First do this routine.
 * Then do a sequence of 'blkdata' calls.
 */
enum clnt_stat
comingsoon5(h_clnt *hcp, const prod_info *infop, u_int pktsz,
        unsigned int timeo, ldm_replyt *replyp)
{
        comingsoon_args arg;
        arg.infop = (prod_info *)infop; /* cast away const */
        arg.pktsz = pktsz;

        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, COMINGSOON,
                (xdrproc_t)xdr_comingsoon_args, (caddr_t)&arg,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}


/*
 * Ship some data.
 */
enum clnt_stat
blkdata5(h_clnt *hcp, const datapkt *pktp,
        unsigned int timeo, ldm_replyt *replyp)
{
        (void) memset(replyp, 0, sizeof(ldm_replyt));

        return h_clnt_call(hcp, BLKDATA,
                (xdrproc_t)xdr_datapkt, (caddr_t)pktp,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)replyp,
                timeo);
}

/* forn */

/*
 * Make a TCP FEEDME or NOTIFYME me call,
 * retrying for certain types of failures.
 * TotalTimeo says how long to keep trying,
 * (You may spend up to TotalTimeo plus rpctimeo.)
 * Calls exitIfDone() after potentially lengthy operations.
 */
static int
forn_signon(
        const u_long proc, /* FEEDME or NOTIFYME only */
        const char *remote, 
        prod_class **reqpp, /* may be modified on return */
        const unsigned rpctimeo,
        int *sockp      /* modified on return */
        )
{
        int status = 0;
        prod_class *clssp;
        prod_class *reclssp = NULL;
        h_clnt hc;
        enum clnt_stat rpc_stat;
        ldm_replyt reply;
        int cfd;
        int sock;

        (void) init_h_clnt(&hc, remote, LDMPROG, 5, IPPROTO_TCP);
        reply.code = OK;

        /*FALLTHROUGH*/
retry:
        if(reply.code != RECLASS)
                clssp = *reqpp;
        
        (void) memset(&reply, 0, sizeof(ldm_replyt));
        rpc_stat = h_clnt_call(&hc, proc,
                (xdrproc_t)xdr_prod_class, (caddr_t)clssp,
                (xdrproc_t)xdr_ldm_replyt, (caddr_t)&reply,
                rpctimeo);

        (void)exitIfDone(0);

        switch (rpc_stat) {

        case RPC_SUCCESS:
                /* call succeeded, can use the reply */
                if (ulogIsDebug())
                    udebug("%s(%s) returns %s",
                             s_ldmproc(proc), remote, s_ldm_errt(reply.code));
                break;

        case RPC_TIMEDOUT:
                status = ETIMEDOUT;
                goto out;

        case RPC_PROGVERSMISMATCH:
        case RPC_PROGUNAVAIL:
        case RPC_PMAPFAILURE:
        case RPC_PROGNOTREGISTERED:
                status = ECONNABORTED;
                goto out;

        case RPC_AUTHERROR:
                uerror("%s(%s): %d: %s",
                        s_ldmproc(proc), remote, rpc_stat,
                         "Authentication error; No match for request");
                status = ECONNABORTED;
                goto out;

        case RPC_CANTRECV:
                if(hc.rpcerr.re_errno == ECONNRESET)
                {
                        /*
                         * The server won't even talk to us
                         */
                        rpc_stat = RPC_AUTHERROR;
                        uerror("%s(%s): %d: %s",
                                s_ldmproc(proc), remote, rpc_stat,
                                 "Access denied by remote server");
                        status = ECONNREFUSED;
                        goto out;
                }
                /* else */
                /*FALLTHROUGH*/
        case RPC_UNKNOWNHOST:
        default:
                uerror("%s(%s): %d: %s",
                        s_ldmproc(proc), remote, rpc_stat,
                                 s_hclnt_sperrno(&hc));
                status = ECONNABORTED;
                goto out;

        }

        /* assert(rpc_stat == RPC_SUCCESS); */
        switch (reply.code) {

        case OK:
                unotice("%s(%s): OK",
                        s_ldmproc(proc), remote);
                break;

        case RECLASS:
                unotice("%s(%s): reclass: %s",
                        s_ldmproc(proc), remote,
                        s_prod_class(NULL, 0,
                                reply.ldm_replyt_u.newclssp));
                if(reply.ldm_replyt_u.newclssp->psa.psa_len == 0)
                {
                        /* shouldn't happen ... */
                        (void) free_prod_class(reply.ldm_replyt_u.newclssp);
                        status = ECONNREFUSED;
                        goto out;
                }
                /* else */
                if(reclssp != NULL)
                        (void) free_prod_class(reclssp);
                reclssp = reply.ldm_replyt_u.newclssp;
                clssp = reclssp;
                        
                goto retry;

        case SHUTTING_DOWN:
                uerror("%s is shutting down", remote);
                status = ECONNABORTED;
                goto out;
        case DONT_SEND:
        case RESTART:
        case REDIRECT: /* TODO */
        default:
                uerror("%s(%s): unexpected reply type %s",
                         s_ldmproc(proc), remote,
                         s_ldm_errt(reply.code));
                status = ECONNABORTED;
                goto out;

        }
        
        /* assert(reply.code == OK); */
        cfd = h_clntfileno(&hc);
        sock = dup(cfd);
        if(sock == -1)
        {
                status = errno;
                serror("dup %d", cfd);
                goto out;
        }
        *sockp = sock;
        *reqpp = clssp;

        /*FALLTHROUGH*/
out:
        close_h_clnt(&hc);
        return status;
}


/*
 * Returns a newly-allocated data-product selection-criteria corresponding to a
 * prototype one and adjusted according to the metadata of the last received
 * data-product.  If there is no metadata, then the prototype selection-criteria
 * is unmodified.
 *
 * Arguements:
 *      prodClass       Pointer to pointer to prototype data-product
 *                      selection-criteria.  On success and if the relevant
 *                      metadata exists, then free_prod_class(*prodClass) was
 *                      called and *prodClass points to a new product-class.
 *                      In any case, it's the caller's responsibility to call
 *                      free_prod_class(*prodClass).
 * Returns:
 *      0               Success.
 *      EINVAL          Metadata exists and its "arrival" time is TS_NONE.
 *      EINVAL          Regular-expression pattern is invalid.
 *      ENOMEM          Out of memory.
 */
static int
adjustByLastInfo(
    prod_class** const          prodClass)
{
    int                         errCode = 0;    /* success */
    const prod_info* const      info = savedInfo_get();

    if (NULL != info) {
        if (tvIsNone(info->arrival)) {
            uerror("Creation-time of last data-product is TS_NONE");

            errCode = EINVAL;
        }
        else {
            prod_class* const   newClass = dup_prod_class(*prodClass);

            if (newClass == NULL) {
                uerror("Couldn't duplicate product-class: %s", strerror(errno));

                errCode = errno;
            }
            else {
                /*
                 * Ensure that the "from" time in the product-class
                 * specification is not earlier than the data-product
                 * creation-time of the product-information structure minus
                 * some slop.
                 */
                {
                    timestampt  newFrom = info->arrival;

                    newFrom.tv_sec -= 2 * interval;

                    if (tvCmp(newClass->from, newFrom, <))
                        newClass->from = newFrom;
                }

                free_prod_class((prod_class*)*prodClass);
                *prodClass = newClass;
            }                           /* newClass allocated */
        }                               /* info->arrival != TS_NONE */
    }                                   /* have "info" */

    return errCode;
}


/*
 * Send a FEEDME or NOTIFYME request to an LDM on a remote host for a class of
 * data-products.  The connection is "turned around" and an RPC dispatch routine
 * is executed until 1) the connection is closed; or 2) there's no activity on
 * the connection for a given amount of time.  Calls exitIfDone() after
 * potentially lengthy operations.
 *
 * Arguments:
 *      proc            The type of request: FEEDME or NOTIFYME.
 *      remote          Pointer to the identifier of the remote host.  May be 
 *                      hostname or encoded IP address.
 *      request         Pointer to pointer to initial, desired product-class.
 *      rpctimeo        The RPC timeout to use.
 *      inactive_timeo  How long to wait with no activity before disconnecting.
 *      dispatch        The RPC dispatch routine to use.
 * Returns:
 *      0               Success.
 *      ECONNABORTED    The connection was aborted.
 *      ECONNRESET      The connection was closed by the remote LDM.
 *      ETIMEDOUT       Activity on the connection timed-out.
 *      ECONNREFUSED    The connection was refused by the remote host.
 *      EINVAL          Creation-time of last received data-product is TS_NONE.
 *      EINVAL          Regular-expression pattern in "request" is invalid.
 *      ENOMEM          Out of memory.
 */
int
forn5(
    const u_long                        proc,
    const char* const                   remote,
    prod_class_t** const                  request,
    const unsigned                      rpctimeo, 
    const int                           inactive_timeo,
    void (* const                       dispatch)(struct svc_req*, SVCXPRT*))
{
    int status = adjustByLastInfo(request);

    if (status == 0) {
        int             xp_sock;

        unotice("LDM-5 desired product-class: %s",
            s_prod_class(NULL, 0, *request));

        status = forn_signon(proc, remote, request, rpctimeo, &xp_sock);

        if (exitIfDone(0) && status == 0) {
            /*
             * "xp_sock" is set.  The other end will now turn the connection
             * around -- reversing the roles of the client and server.
             *
             * Should we setsockopt(sock, SOL_SOCKET, SO_RCVBUF, ...)?
             */
            SVCXPRT* const      xprt =
                svcfd_create(xp_sock, 0, MAX_RPC_BUF_NEEDED);

            if(xprt == NULL) {
                uerror("svcfd_create() failure.");
            }
            else {
                if(!svc_register(xprt, LDMPROG, 5, dispatch, 0)) {
                    uerror("svc_register() failure.");
                    svc_destroy(xprt);
                }
                else {
                    status = one_svc_run(xp_sock, inactive_timeo);
                    /*
                     * svc_destroy(xprt) was invoked by svc_getreqset() in
                     * one_svc_run().
                     */
                }                       /* dispatch routine registered */
            }                           /* svcfd_create() success */

            (void)close(xp_sock);       /* might already be closed */
        }                               /* forn_signon() success */
    }                                   /* product-class adjusted */

    return status;
}
