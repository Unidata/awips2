/*
 * This module contains the server-side functions that are invoked by the
 * "ldm6_svc" module.  Because RPC assumes a client/server structure, this
 * module contains code for both upstream and downstream LDM-s.
 *
 * From a design-pattern perspective, this module is a combination façade and
 * adapter for the "up6" and "down6" modules.
 */
/* $Id: ldm6_server.c,v 1.13.8.2.2.3.2.18 2009/06/18 23:37:22 steve Exp $ */

#include "ldmconfig.h"

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>       /* ENOMEM */
#include <netinet/in.h>
#include <rpc/rpc.h>     /* SVCXPRT, xdrproc_t */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>      /* getenv, exit */
#include <string.h>      /* strncpy(), strerror() */
#include <strings.h>     /* strncasecmp() */
#include <unistd.h>      /* getpid() */

#include "acl.h"         /* acl_product_intersection(), acl_check_hiya() */
#include "autoshift.h"
#include "child_process_set.h"       /* cps_contains() */
#include "down6.h"       /* the pure "downstream" LDM module */
#include "error.h"
#include "globals.h"
#include "inetutil.h"    /* hostbyaddr() */
#include "ldm.h"         /* *_6_svc() functions */
#include "ldmprint.h"    /* s_prod_class() */
#include "pq.h"
#include "prod_class.h"  /* free_prod_class() */
#include "ulog.h"
#include "UpFilter.h"

#include "up6.h"         /* the pure "upstream" LDM module */


static fornme_reply_t           theReply;


/*
 * Decodes a data-product signature from the last product-specification of a
 * product-class if it exists.
 *
 * Arguments:
 *      class           Pointer to the product-class.  Caller may free upon
 *                      return.
 * Returns:
 *      NULL            The last product-specification didn't contain a valid,
 *                      encoded signature.
 *      else            Pointer to a static signature buffer into which the
 *                      signature specification was successfully decoded.
 */
static const signaturet*
decodeSignature(
    const prod_class_t* const     prodClass)
{
    const signaturet* sig = NULL;             /* no valid, encoded signature */

    if (0 < prodClass->psa.psa_len) {
        const prod_spec* const  lastProdSpec =
            &prodClass->psa.psa_val[prodClass->psa.psa_len-1];

        if (NONE == lastProdSpec->feedtype) {
            char*       pat = lastProdSpec->pattern;

            if (strncasecmp("SIG=", pat, 4) == 0) {
                char*                   encodedSig = pat + 4;
                int                     i;
                unsigned                value;
                static signaturet       sigBuf;

                errno = 0;

                for (i = 0; i < sizeof(signaturet); i++) {
                    if (sscanf(encodedSig + 2*i, "%2x", &value) != 1)
                        break;

                    sigBuf[i] = (unsigned char)value;
                }

                if (i == sizeof(signaturet)) {
                    sig = (const signaturet*)&sigBuf[0];
                }
                else {
                    if (0 == errno) {
                        err_log_and_free(
                            ERR_NEW1(1, NULL, "Invalid signature (%s)",
                                encodedSig),
                            ERR_NOTICE);
                    }
                    else {
                        err_log_and_free(
                            ERR_NEW2(1, NULL, "Invalid signature (%s): %s", 
                                encodedSig, strerror(errno)),
                            ERR_NOTICE);
                    }
                }                       /* signature not decoded */
            }                           /* "SIG=" found */
        }                               /* last feedtype is NONE */
    }                                   /* at least one product-specification */

    return sig;
}


/*
 * Separates a product-class into a signature component and a non-signature
 * component.
 *
 * Arguments:
 *      prodClass       Pointer to product-class to be separated.  Caller may
 *                      free upon return.
 *      noSigProdClass  Pointer to pointer to be set to an allocated product-
 *                      class that will not have a signature encoded within
 *                      it.  Set on and only on success.  On success, caller
 *                      should invoke free_prod_class(*noSigProdClass).
 *      signature       Pointer to pointer to signature.  Set on and only on
 *                      success.  Will be set to NULL if and only if "prodClass"
 *                      didn't contain an encoded signature; otherwise, will
 *                      be set to point to a static buffer that contains the
 *                      signature.  Caller must not free.
 * Returns:
 *      NULL            Success.
 *      else            Error object.
 */
static ErrorObj*
separateProductClass(
    const prod_class_t* const     prodClass,
    prod_class_t** const          noSigProdClass,
    const signaturet** const    signature)
{
    ErrorObj*            errObj;
    prod_class_t*         noSigClass = dup_prod_class(prodClass);

    if (NULL == noSigClass) {
        errObj = ERR_NEW1(0, NULL,
            "Couldn't duplicate product-class: %s", strerror(errno));
    }
    else {
        const signaturet*       sig = decodeSignature(prodClass);

        if (NULL != sig)
            clss_scrunch(noSigClass);   /* removes encoded signature */

        *noSigProdClass = noSigClass;
        *signature = sig;
        errObj = NULL;                  /* success */
    }

    return errObj;
}


/*
 * Code common to both FEEDME and NOTIFYME requests.  If a fatal error occurs,
 * then an error-response is sent to the client and the process is terminated.
 * 
 * Arguments:
 *      xprt            Pointer to server-side transport handle.
 *      want            Pointer to product-class desired by downstream LDM.
 *                      May contain a "signature" product-specification.
 *      prodClass       Pointer to pointer to intersection of desired and
 *                      allowed product-classes.  On success, caller should
 *                      call free_prod_class(*prodClass).  Will not contain a
 *                      "signature" product-specification.
 *      signature       Pointer to pointer to the signature of the last,
 *                      successfully-received data-product.  Set on and only
 *                      on success.  Set to NULL if and only if *class did not
 *                      contain such a specification; otherwise, points to a
 *                      static buffer.
 *      downName        Pointer to buffer into which this function will copy
 *                      the NUL-terminated name of the downstream host.
 *      len             Size of "downName" buffer in bytes -- including
 *                      terminating NUL.
 *      downAddr        Pointer to address of downstream LDM.  Set by this 
 *                      function.
 *      socket          Pointer to socket on which to send to downstream LDM.
 *                      Set by this function. Duplicate of xprt->xp_sock.
 *      upFilter        Pointer to pointer to upstream filter of data-products.
 *                      Set on and only on success.  Caller should invoke
 *                      upFilter_free(*upFilter).
 * Returns:
 *      NULL            Success.  A reply has been sent to the client and the
 *                      connection can now be "turned around".  *class, 
 *                      *downname, *downAddr, *socket, and *prodClass are set.
 *                      The caller should invoke free_prod_class(*prodClass).
 *      else            The return-value should be sent to the client as a
 *                      reply.  The return-value may reference *prodClass.
 *                      The connection should not be "turned around".
 *                      *prodClass is set and the caller should invoke
 *                      free_prod_class(*prodClass).
 */
static fornme_reply_t*
feed_or_notify(
    SVCXPRT* const              xprt,
    const prod_class_t* const     want,
    prod_class_t** const          prodClass,
    const signaturet** const    signature,
    char* const                 downName,
    const size_t                len,
    struct sockaddr_in* const   downAddr,
    int* const                  socket,
    UpFilter** const            upFilter)
{
    fornme_reply_t*             reply = NULL;
    int                         errCode;
    static prod_class_t*          allow = NULL;
    int                         terminate = 1;    /* terminate process */
    int                         sock;
    const char*                 name;
    struct sockaddr_in*         addr;
    UpFilter*                   filter;
    prod_class_t*                 noSigProdClass;
    const signaturet*           sig;
    ErrorObj*                   errObj;

    addr = svc_getcaller(xprt);
    name = hostbyaddr(addr);

    if (errObj = separateProductClass(want, &noSigProdClass, &sig)) {
        err_log_and_free(errObj, ERR_FAILURE);
        svcerr_systemerr(xprt);
    }
    else {
        if (NULL != allow) {
            free_prod_class(allow);
            allow = NULL;
        }

        errCode = acl_product_intersection(name, &addr->sin_addr, 
            noSigProdClass, &allow);

        if (errCode == ENOMEM) {
            serror("Couldn't compute wanted/allowed product intersection");

            svcerr_systemerr(xprt);
        }
        else {
            sock = dup(xprt->xp_sock);

            if (sock == -1) {
                serror("Couldn't duplicate socket %d", xprt->xp_sock);
                svcerr_systemerr(xprt);
            }
            else {
                reply = (fornme_reply_t*)memset(&theReply, 0, sizeof(theReply));

                if (errCode == EINVAL) {
                    uerror("%s:%d: Invalid product pattern: %s",
                        __FILE__, __LINE__,
                        s_prod_class(NULL, 0, noSigProdClass));

                    reply->code = BADPATTERN;
                }
                else {
                    assert(errCode == 0);

                    /* TODO: adjust time? */

                    if (allow->psa.psa_len == 0 ||
                        !clss_eq(noSigProdClass, allow)) {

                        /*
                         * The downstream LDM is not allowed to receive what it
                         * requested.
                         */
                        if (allow->psa.psa_len == 0) {
                            uwarn("Empty wanted/allowed product-class "
                                "intersection for %s: %s -> <nil>",
                                name,
                                s_prod_class(NULL, 0, noSigProdClass));
                            svcerr_weakauth(xprt);
                        }
                        else {
                            char wantStr[1984];

                            (void)s_prod_class(wantStr, sizeof(wantStr),
                                noSigProdClass);
                            unotice("Restricting request: %s -> %s",
                                wantStr, s_prod_class(NULL, 0, allow));

                            reply->code = RECLASS;
                            reply->fornme_reply_t_u.prod_class = allow;
                            terminate = 0;          /* return to caller */
                        }
                    }                               /* request not allowed */
                    else {
                        errObj = acl_getUpstreamFilter(name, &addr->sin_addr,
                            noSigProdClass, &filter);

                        if (errObj) {
                            err_log_and_free(
                                ERR_NEW(0, errObj,
                                    "Couldn't get \"upstream\" filter"),
                                ERR_FAILURE);
                            svcerr_systemerr(xprt);
                        }
                        else if (NULL == filter) {
                            err_log_and_free(
                                ERR_NEW(0, NULL,
                                    "Upstream filter prevents data-transfer"),
                                ERR_FAILURE);
                            svcerr_weakauth(xprt);
                        }
                        else {
                            /*
                             * The downstream LDM is allowed to receive 
                             * data-products.
                             */
                            reply->code = OK;
                            reply->fornme_reply_t_u.id = (unsigned)getpid();

                            if (svc_sendreply(xprt,
                                    (xdrproc_t)xdr_fornme_reply_t, 
                                    (caddr_t)reply)) {
                                terminate = 0;          /* return to caller */
                                reply = NULL;           /* success */
                            }
                            else {
                                uerror("svc_sendreply(...) failure");
                                svcerr_systemerr(xprt);
                                upFilter_free(filter);
                            }
                        }               /* "filter" allocated */
                    }                   /* request feedtype allowed */
                }                       /* valid patterns */

                if (terminate)
                    (void)close(sock);
            }                           /* socket duplicated */

            if (terminate) {
                free_prod_class(allow);
                allow = NULL;
            }
        }                               /* "allow" allocated */

        free_prod_class(noSigProdClass);
    }                                   /* "noSigProdClass" allocated */

    if (terminate) {
        svc_destroy(xprt);
        exit(2);
        /*NOTREACHED*/
    }
    /* else */ {
        (void)strncpy(downName, name, len-1);
        downName[len-1] = 0;
        *downAddr = *addr;
        *socket = sock;
        *prodClass = allow;
        *signature = sig;
        *upFilter = filter;
    }

    return reply;
}


/******************************************************************************
 * Begin Public API:
 ******************************************************************************/


/*
 * This function will not normally return unless the request necessitates a
 * reply (e.g., RECLASS).
 */
fornme_reply_t *feedme_6_svc(
    feedpar_t      *feedPar,
    struct svc_req *rqstp)
{
    SVCXPRT* const      xprt = rqstp->rq_xprt;
    prod_class_t*         want = feedPar->prod_class;
    prod_class_t*         prodClass;
    const signaturet*   signature;
    char                downName[MAXHOSTNAMELEN+1];
    struct sockaddr_in  downAddr;
    int                 socket;
    UpFilter*           upFilter;
    fornme_reply_t*     reply = 
        feed_or_notify(xprt, want, &prodClass, &signature, downName,
            sizeof(downName), &downAddr, &socket, &upFilter);

    if (!reply) {
        int     errCode;

        if (!svc_freeargs(xprt, xdr_feedpar_t, (caddr_t)feedPar)) {
            uerror("Couldn't free arguments");
            svc_destroy(xprt);
            exit(1);
            /*NOTREACHED*/
        }

        svc_destroy(xprt);

        /*
         * Wait a second before sending anything to the downstream LDM.
         */
        (void)sleep(1);

        errCode = up6_new_feeder(socket, downName, &downAddr, prodClass,
            signature, feedPar->max_hereis > UINT_MAX/2, pqfname, interval,
            upFilter);

        free_prod_class(prodClass);

        exit(errCode);

        /*NOTREACHED*/
    }

    return reply;
}


/*ARGSUSED1*/
fornme_reply_t *notifyme_6_svc(
    prod_class_t*         want,
    struct svc_req*     rqstp)
{
    SVCXPRT* const      xprt = rqstp->rq_xprt;
    prod_class_t*         prodClass;
    const signaturet*   signature;
    char                downName[MAXHOSTNAMELEN+1];
    struct sockaddr_in  downAddr;
    int                 socket;
    UpFilter*           upFilter;
    fornme_reply_t*     reply = 
        feed_or_notify(xprt, want, &prodClass, &signature, downName,
            sizeof(downName), &downAddr, &socket, &upFilter);

    if (!reply) {
        int     errCode;

        if (!svc_freeargs(xprt, xdr_prod_class, (caddr_t)want)) {
            uerror("Couldn't free arguments");
            svc_destroy(xprt);
            exit(1);
            /*NOTREACHED*/
        }

        svc_destroy(xprt);

        /*
         * Wait a second before sending anything to the downstream LDM.
         */
        (void)sleep(1);

        errCode = up6_new_notifier(socket, downName, &downAddr, prodClass,
            signature, pqfname, interval, upFilter);

        free_prod_class(prodClass);

        exit(errCode);

        /*NOTREACHED*/
    }

    return reply;
}


int *is_alive_6_svc(
    unsigned       *id,
    struct svc_req *rqstp)
{
    static int  alive;
    SVCXPRT    *const xprt = rqstp->rq_xprt;
    int         error = 0;

    alive = cps_contains((pid_t)*id);

    if (ulogIsDebug()) {
        udebug("LDM %u is %s", *id, alive ? "alive" : "dead");
    }

    if (!svc_sendreply(xprt, (xdrproc_t)xdr_bool, (caddr_t)&alive)) {
        svcerr_systemerr(xprt);

        error = 1;
    }

    if (!svc_freeargs(xprt, xdr_u_int, (caddr_t)id)) {
        uerror("Couldn't free arguments");

        error = 1;
    }

    svc_destroy(xprt);
    exit(error);

    /*NOTREACHED*/

    return NULL;
}


hiya_reply_t*
hiya_6_svc(
    prod_class_t     *offered,
    struct svc_req *rqstp)
{
    static hiya_reply_t reply;
    SVCXPRT            *const xprt = rqstp->rq_xprt;
    struct sockaddr_in *upAddr = (struct sockaddr_in*)svc_getcaller(xprt);
    const char         *upName = hostbyaddr(upAddr);
    int                 error;
    int                 isPrimary;
    unsigned int        maxHereis;
    static prod_class_t  *accept;

    /*
     * Open the product-queue for writing.  It will be closed by cleanup()
     * during process termination.
     */
    if (pq) {
        (void)pq_close(pq);
        pq = NULL;
    }
    error = pq_open(pqfname, PQ_DEFAULT, &pq);
    if (error) {
        err_log_and_free(
            ERR_NEW2(error, NULL,
                "Couldn't open product-queue \"%s\" for writing: %s",
                pqfname,
                PQ_CORRUPT == error 
                    ? "The product-queue is inconsistent" 
                    : strerror(error)),
            ERR_FAILURE);
        svcerr_systemerr(xprt);
        svc_destroy(xprt);
        exit(error);
    }

    /* else */

    error = down6_init(upName, upAddr, pqfname, pq);
    if (error) {
        uerror("Couldn't initialize downstream LDM");
        svcerr_systemerr(xprt);
        svc_destroy(xprt);
        exit(error);
    }
    else {
        uinfo("Downstream LDM initialized");
    }

    /*
     * The previous "accept" is freed here -- rather than freeing the
     * soon-to-be-allocated "accept" at the end of its block -- because it can
     * be used in the reply.
     */
    if (accept) {
        free_prod_class(accept);  /* NULL safe */
        accept = NULL;
    }

    error = acl_check_hiya(upName, inet_ntoa(upAddr->sin_addr), offered,
        &accept, &isPrimary);

    maxHereis = isPrimary ? UINT_MAX : 0;

    if (error) {
        serror("Couldn't validate HIYA");
        svcerr_systemerr(xprt);
        svc_destroy(xprt);
        exit(error);
    }
    else {
        if (ulogIsDebug())
            udebug("intersection: %s", s_prod_class(NULL, 0, accept));

        if (accept->psa.psa_len == 0) {
            uwarn("Empty intersection of HIYA offer from %s (%s) and ACCEPT "
                "entries", upName, s_prod_class(NULL, 0, offered));
            svcerr_weakauth(xprt);
            svc_destroy(xprt);
            exit(0);
        }
        else {
            error = down6_set_prod_class(accept);

            if (error) {
                if (DOWN6_SYSTEM_ERROR == error) {
                    serror("Couldn't set product class: %s",
                        s_prod_class(NULL, 0, accept));
                }
                else {
                    uerror("Couldn't set product class: %s",
                        s_prod_class(NULL, 0, accept));
                }

                svcerr_systemerr(xprt);
                svc_destroy(xprt);
                exit(EXIT_FAILURE);
            }

            /* else */

            /*
             * At this point, a reply will be sent to the upstream LDM;
             * consequently, the autoshift module must be unconditionally
             * initialized in case the upstream LDM starts sending data-products
             * without first sending another HIYA.  The autoshift module is
             * initialized as an upstream LDM to effectively disable it (which
             * is appropriate for a HIYA message).
             */
            (void)as_init(1, isPrimary, xprt->xp_sock);

            if (clss_eq(offered, accept)) {
                unotice("hiya6: %s", s_prod_class(NULL, 0, offered));

                reply.code = OK;
                reply.hiya_reply_t_u.max_hereis = maxHereis;
            }
            else {
                if (ulogIsVerbose()) {
                    char off[512];
                    char acc[512];

                    (void)s_prod_class(off, sizeof(off), offered),
                    (void)s_prod_class(acc, sizeof(acc), accept);

                    uinfo("hiya6: RECLASS: %s -> %s", off, acc);
                }

                reply.code = RECLASS;
                reply.hiya_reply_t_u.feedPar.prod_class = accept;
                reply.hiya_reply_t_u.feedPar.max_hereis = maxHereis;
            }
        }  /* product-intersection != empty set */
    }  /* successful acl_check_hiya() */

    return &reply;
}


/*ARGSUSED1*/
void *hereis_6_svc(
    product        *prod,
    struct svc_req *rqstp)
{
    int         error = down6_hereis(prod);

    if (error && DOWN6_UNWANTED != error && DOWN6_PQ_BIG != error) {
        (void)svcerr_systemerr(rqstp->rq_xprt);
        svc_destroy(rqstp->rq_xprt);
        exit(error);
    }

    return NULL;  /* don't reply */
}


/*ARGSUSED1*/
void *notification_6_svc(
    prod_info      *info,
    struct svc_req *rqstp)
{
    (void)down6_notification(info);

    return NULL;  /* don't reply */
}


/*ARGSUSED1*/
comingsoon_reply_t *comingsoon_6_svc(
    comingsoon_args    *comingPar,
    struct svc_req     *rqstp)
{
    static comingsoon_reply_t reply;
    int                       error = down6_comingsoon(comingPar);

    if (error == 0) {
        reply = OK;
    }
    else if (DOWN6_UNWANTED == error || DOWN6_PQ_BIG == error) {
        reply = DONT_SEND;
    }
    else {
        (void)svcerr_systemerr(rqstp->rq_xprt);
        svc_destroy(rqstp->rq_xprt);
        exit(error);
    }

    return &reply;
}


/*ARGSUSED1*/
void *blkdata_6_svc(
    datapkt        *argp,
    struct svc_req *rqstp)
{
    int error = down6_blkdata(argp);

    if (error && DOWN6_UNWANTED != error && DOWN6_PQ_BIG != error) {
        (void)svcerr_systemerr(rqstp->rq_xprt);
        svc_destroy(rqstp->rq_xprt);
        exit(error);
    }

    return NULL;  /* don't reply */
}


/*
 * Frees resources allocated by the creation of the return result.
 *
 * @param *transp     Server-side handle.
 * @param  xdr_result XDR function for result.
 * @param  result     Result.
 * @return 0          if failure.
 * @return !0         if success.
 */
/*ARGSUSED0*/
int ldmprog_6_freeresult(
    SVCXPRT   *transp,
    xdrproc_t  xdr_result, 
    caddr_t    result)
{
    (void)xdr_free(xdr_result, result);

    return 1;
}
