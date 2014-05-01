/*
 * This module contains the requester, which implements the REQUEST action for
 * version 6 of the LDM.
 */
/* $Id: requester6.c,v 1.34.2.5.2.3.2.2.2.19 2008/04/15 16:34:13 steve Exp $ */

#include "ldmconfig.h"

#include <assert.h>      /* assert() */
#include <errno.h>       /* system error codes */
#include <limits.h>      /* *INT_MAX */
#include <arpa/inet.h>   /* for <netinet/in.h> under FreeBSD 4.5-RELEASE */
#include <netinet/in.h>  /* sockaddr_in */
#include <rpc/rpc.h>
#include <signal.h>      /* sig_atomic_t */
#include <stdlib.h>      /* NULL, malloc() */
#include <string.h>      /* strerror() */
#include <strings.h>     /* strncasecmp() */
#include <sys/time.h>    /* struct timeval */
#include <unistd.h>      /* close() */

#include "autoshift.h"
#include "down6.h"       /* down6_init(), down6_destroy() */
#include "error.h"
#include "globals.h"
#include "inetutil.h"    /* hostbyaddr() */
#include "ldm.h"         /* client-side LDM functions */
#include "ldm_clnt.h"    /* client-side LDM functions */
#include "ldmprint.h"    /* s_prod_info() */
#include "prod_class.h"  /* clss_eq() */
#include "prod_info.h"
#include "rpcutil.h"     /* clnt_errmsg() */
#include "savedInfo.h"
#include "timestamp.h"
#include "ulog.h"

#include "requester6.h"

static char*    _upName = NULL;
static int      requestSocket = -1;
static int      dataSocket = -1;
static int      isAliveSocket = -1;


/*
 * Indicates if the upstream, sending LDM should be assumed to be alive based
 * on the failure mode of an IS_ALIVE inquiry.
 */
static int
assume_alive(
    const enum clnt_stat stat,
    const int            err)
{
    return
        RPC_TIMEDOUT == stat ||
        RPC_CANTSEND == stat ||
        RPC_CANTRECV == stat ||
        (RPC_SYSTEMERROR == stat && (
            ECONNREFUSED == err ||
            ECONNRESET == err ||
            ETIMEDOUT == err ||
            ECONNABORTED == err));
}


/*
 * @return !0 if upstream LDM is alive.
 * @return  0 if upstream LDM is dead.
 */
static int is_upstream_alive(
    const char* const         upName,
    struct sockaddr_in* const upAddr,
    unsigned int              upId)
{
    int                 alive = 0;      /* dead */
    ErrorObj*           error;
    int                 hasAddress;

    /*
     * Verify that the upstream host still has the original IP address.  This
     * detects failures due to a reconnection by the upstream host to its
     * Internet Service Provider resulting in a different IP address (e.g., the
     * UCAR HAIPER plane).
     */
    error = hostHasIpAddress(upName, upAddr->sin_addr.s_addr, &hasAddress);

    if (error) {
        err_log_and_free(
            ERR_NEW1(0, error, "Couldn't get IP address for upstream host %s",
                upName),
            ERR_ERROR);
    }
    else if (!hasAddress) {
        err_log_and_free(
            ERR_NEW2(0, NULL, "Upstream host %s no longer has IP address %s",
                upName, inet_ntoa(upAddr->sin_addr)),
            ERR_NOTICE);
    }
    else {
        CLIENT *clnt;
        /*
         * In the following, an upstream LDM that is unavailable due to
         * certain failure modes is, nevertheless, considered alive.  This
         * prevents unnecessary reconnections due to network congestion.
         */
        isAliveSocket = RPC_ANYSOCK;
        clnt = clnttcp_create(upAddr, LDMPROG, SIX, &isAliveSocket, 0, 0);

        if(clnt == NULL) {
            alive = assume_alive(rpc_createerr.cf_stat, 
                rpc_createerr.cf_error.re_errno);

            err_log_and_free(
                ERR_NEW2(0, 
                    ERR_NEW(0, NULL, clnt_spcreateerror("")), 
                    "Couldn't connect to upstream LDM on %s; "
                        "assuming sending LDM is %s",
                    upName, alive ? "alive" : "dead"),
                ERR_INFO);
        }
        else {
            bool_t* isAlive = is_alive_6(&upId, clnt);

            if (isAlive != NULL) {
                alive = *isAlive;
            }
            else {
                struct rpc_err rpcErr;

                clnt_geterr(clnt, &rpcErr);

                alive = assume_alive(rpcErr.re_status, rpcErr.re_errno);

                err_log_and_free(
                    ERR_NEW2(0, 
                        ERR_NEW(0, NULL, clnt_errmsg(clnt)), 
                        "No IS_ALIVE reply from upstream LDM on %s; "
                            "assuming sending LDM is %s",
                        upName, alive ? "alive" : "dead"),
                    ERR_INFO);
            }

            auth_destroy(clnt->cl_auth);
            clnt_destroy(clnt);

            /*
             * The socket should have been closed by clnt_destroy() because 
             * RPC_ANYSOCK was specified.  But this can't hurt.
             */
            (void)close(isAliveSocket);
            isAliveSocket = -1;
        }                               /* valid client and socket */
    }                                   /* upstream host has same IP address */

    return alive;
}


/*
 * On return, the socket might or might not be closed.
 *
 * Returns:
 *      NULL    Success.  as_shouldSwitch() is true.
 *      else    Error.  err_code() values:
 *                  REQ6_SYSTEM_ERROR
 *                      err_cause() will be system error.
 *                  REQ6_DISCONNECT (died or closed 
 *                      connection). err_cause() will be NULL.
 */
static ErrorObj*
run_service(
    int                    socket,
    unsigned               inactiveTimeout,
    const char            *const upName,
    struct sockaddr_in    *const upAddr,
    const unsigned int     upId,
    const char            *pqPathname,
    prod_class_t            *const expect,
    pqueue                *const pq)
{
    ErrorObj*    error = NULL;           /* success */
    SVCXPRT*    xprt;

    assert(socket >= 0);
    assert(inactiveTimeout != 0);
    assert(upName != NULL);
    assert(upAddr != NULL);
    assert(pqPathname != NULL);
    assert(pq != NULL);

    xprt = svcfd_create(socket, 0, MAX_RPC_BUF_NEEDED);

    if (xprt == NULL) {
        error = ERR_NEW1(REQ6_SYSTEM_ERROR, NULL, 
            "Couldn't create RPC service for %s", upName);
    }
    else {
        int destroyTransport = 1;

        if (!svc_register(xprt, LDMPROG, SIX, ldmprog_6, 0)) {
            error = ERR_NEW(REQ6_SYSTEM_ERROR, NULL, 
                "Couldn't register LDM service");
        }
        else {
            if (down6_init(upName, upAddr, pqPathname, pq)) {
                error = ERR_NEW(REQ6_SYSTEM_ERROR, NULL, 
                    "Couldn't initialize downstream LDM");
            }
            else {
                if (down6_set_prod_class(expect)) {
                    error = ERR_NEW1(REQ6_SYSTEM_ERROR, NULL, 
                        "Couldn't set expected product class: %s",
                        s_prod_class(NULL, 0, expect));
                }
                else {
                    udebug("%s:%d: Downstream LDM initialized",
                        __FILE__, __LINE__);

                    for (;;) {
                        /*
                         * The only possible return values are:
                         *     0          if as_shouldSwitch()
                         *     ETIMEDOUT  if timeout exceeded.
                         *     ECONNRESET if connection closed.
                         *     EBADF      if socket not open.
                         *     EINVAL     if invalid timeout.
                         */
                        int err = one_svc_run(socket, inactiveTimeout);

                        (void)exitIfDone(0);

                        if (err == ETIMEDOUT) {
                            if (ulogIsVerbose())
                                uinfo("Connection from upstream LDM silent "
                                    "for %d seconds", inactiveTimeout);

                            if (is_upstream_alive(upName, upAddr, upId)) {
                                if (ulogIsVerbose())
                                    uinfo("Upstream LDM is alive.  Waiting...");

                                continue;
                            }

                            error = ERR_NEW(REQ6_DISCONNECT, NULL, 
                                "Upstream LDM died");
                        }
                        else if (err != 0) {
                            error = ERR_NEW(REQ6_DISCONNECT, NULL,
                                "Connection to upstream LDM closed");
                            /*
                             * The service-tranport was destroyed by 
                             * one_svc_run().
                             */
                            destroyTransport = 0;
                        }

                        break;
                    }                   /* service timeout loop */
                }                       /* expected product-class set */

                down6_destroy();
            }                           /* down6 module initialized */
        }                               /* RPC service registered */

        if (destroyTransport)
            svc_destroy(xprt);
    }                                   /* xprt != NULL */

    return error;
}


static ErrorObj*
make_request(
    const char* const                   upName,
    const prod_class_t* const             prodClass,
    const int                           isPrimary,
    CLIENT* const                       clnt,
    unsigned* const                     id)
{
    ErrorObj*    errObj = NULL;          /* no error */
    int         finished = 0;
    feedpar_t   feedpar;

    assert(prodClass != NULL);
    assert(clnt != NULL);
    assert(id != NULL);

    feedpar.max_hereis = isPrimary ? UINT_MAX : 0;
    feedpar.prod_class = dup_prod_class(prodClass);

    if (NULL == feedpar.prod_class) {
        errObj = ERR_NEW1(errno, NULL, "Couldn't duplicate product-class: %s",
            strerror(errno));
    }
    else {
        while (!errObj && !finished && exitIfDone(0)) {
            fornme_reply_t*     feedmeReply;

            udebug("%s:%d: Calling feedme_6(...)", __FILE__, __LINE__);

            feedmeReply = feedme_6(&feedpar, clnt);

            if (!feedmeReply) {
                errObj = ERR_NEW(
                    REQ6_DISCONNECT,
                    ERR_NEW(clnt_stat(clnt), NULL, clnt_errmsg(clnt)),
                    "Upstream LDM didn't reply to FEEDME request");
            }
            else {
                if (feedmeReply->code == 0) {
                    unotice("Upstream LDM-6 on %s is willing to be %s feeder",
                        upName,
                        feedpar.max_hereis == UINT_MAX
                            ? "a primary"
                            : "an alternate");

                    *id = feedmeReply->fornme_reply_t_u.id;
                    errObj = NULL;
                    finished = 1;
                }
                else {
                    if (feedmeReply->code == BADPATTERN) {
                        errObj = ERR_NEW1(
                            REQ6_BAD_PATTERN,
                            NULL,
                            "Upstream LDM can't compile pattern: %s",
                            s_prod_class(NULL, 0, feedpar.prod_class));
                    }
                    else if (feedmeReply->code == RECLASS) {
                        prod_class_t *allow =
                            feedmeReply->fornme_reply_t_u.prod_class;

                        if (allow->psa.psa_len == 0) {
                            errObj = ERR_NEW1( REQ6_NOT_ALLOWED, NULL,
                                "Upstream LDM says we're not allowed to "
                                    "receive requested products: %s",
                                s_prod_class(NULL, 0, feedpar.prod_class));
                        }
                        else {
                            char wantStr[1984];

                            (void)s_prod_class(wantStr, sizeof(wantStr), 
                                feedpar.prod_class),
                            unotice("Product reclassification by upstream LDM: "
                                "%s -> %s",
                                wantStr, s_prod_class(NULL, 0, allow));

                            if (tvEqual(TS_NONE, allow->from) ||
                                    tvEqual(TS_NONE, allow->to)) {
                                errObj = ERR_NEW1(
                                    REQ6_BAD_RECLASS,
                                    NULL,
                                    "Invalid RECLASS from upstream LDM: %s",
                                    s_prod_class(NULL, 0, allow));
                            }
                            else {
                                /*
                                 * The product-class in the reply from the
                                 * upstream LDM will be xdr_free()ed, so it's
                                 * duplicated for use in the next iteration.
                                 */
                                prod_class_t* clone = dup_prod_class(allow);

                                if (NULL == clone) {
                                    errObj = ERR_NEW1(
                                        REQ6_SYSTEM_ERROR,
                                        ERR_NEW(errno, NULL, strerror(errno)),
                                        "Couldn't clone product-class \"%s\"",
                                        s_prod_class(NULL, 0, allow));
                                }
                                else {
                                    free_prod_class(feedpar.prod_class);

                                    feedpar.prod_class = clone;
                                    errObj = NULL;
                                }
                            }           /* valid RECLASS prod-class */
                        }               /* non-empty RECLASS */
                    }                   /* RECLASS reply */
                }                       /* feedmeReply->code != 0 */

                (void)xdr_free((xdrproc_t)xdr_fornme_reply_t,
                    (char*)feedmeReply);
            }                           /* non-NULL reply */
        }                               /* try loop */

        free_prod_class(feedpar.prod_class);
    }                                   /* "feedpar.prod_class" allocated */

    return errObj;
}


/*
 * Creates a "signature" product-class based on a prototype product-class and 
 * data-product metadata.
 *
 * Arguements:
 *      protoClass      Pointer to the prototype product-class.  Must be a
 *                      non-signature product-class.  Caller may free on
 *                      return.
 *      info            The data-product metadata to use.  Caller may free on
 *                      return.
 *      newClass        Pointer to the pointer to the returned product-class.
 *                      *newClass is set on and only on success.  Upon
 *                      successful return, it is the caller's responsibility to
 *                      invoke free_prod_class() on *newClass.
 * Returns:
 *      NULL            Success.
 *      else            Failure.  Pointer to error structure.  err_code():
 *                              ENOMEM  
 *                              EINVAL  Pattern couldn't be compiled.
 */
static ErrorObj*
newSigClass(
    const prod_class_t* const     protoClass,
    const prod_info* const      info,
    prod_class_t** const          newClass)
{
    ErrorObj*    error = NULL;           /* success */
    prod_class_t* prodClass = new_prod_class(protoClass->psa.psa_len + 1);

    if (NULL == prodClass) {
        error = ERR_NEW1(errno, NULL, "Couldn't allocate new product-class: %s",
            strerror(errno));
    }
    else {
        int     err = cp_prod_class(prodClass, protoClass, 0);

        if (err) {
            error = ERR_NEW1(errno, NULL,
                "Couldn't copy product-class: %s", strerror(err));
        }
        else {
            size_t      bufLen = 4 + sizeof(signaturet)*2 + 1;  /* SIG=...\0 */
            char*       buf = (char*)malloc(bufLen);

            if (NULL == buf) {
                error = ERR_NEW1(errno, NULL,
                    "Couldn't allocate pattern-buffer: %s", strerror(errno));
            }
            else {
                prod_spec*      prodSpec = prodClass->psa.psa_val
                    + prodClass->psa.psa_len;

                (void)sprintf(buf, "SIG=%s",
                    s_signaturet(NULL, 0, info->signature));

                prodSpec->feedtype = NONE;
                prodSpec->pattern = buf;
                prodClass->psa.psa_len++;
            }                           /* pattern-buffer allocated */
        }                               /* product-class copied */

        if (error) {
            free_prod_class(prodClass);
        }
        else {
            *newClass = prodClass;
        }
    }                                   /* "class" allocated */

    return error;
}


/*
 * Returns a newly-allocated data-product selection-criteria corresponding to a
 * prototype one and adjusted according to the metadata of the last received
 * data-product.
 *
 * Arguements:
 *      protoClass      Pointer to prototype data-product 
 *                      selection-criteria.  It must not be a "signature"
 *                      product-class.  Caller may free on return.
 *      newClass        Pointer to the pointer to the adjusted data-product
 *                      selection-criteria.  *newClass is set on and only on 
 *                      success.  Upon successful return, it is the caller's 
 *                      responsibility to invoke free_prod_class() on *newClass.
 * Returns:
 *      NULL            Success. *newClass is set.
 *      else            Failure.  Pointer to error structure.  err_code():
 *                              EINVAL  info->arrival is TS_NONE
 */
static ErrorObj*
adjustByLastInfo(
    const prod_class_t* const     protoClass,
    prod_class_t** const          newClass)
{
    ErrorObj*                    errObj = NULL;  /* success */
    const prod_info* const      info = savedInfo_get();

    if (NULL == info) {
        prod_class_t* const       prodClass = dup_prod_class(protoClass);

        if (NULL == prodClass) {
            errObj = ERR_NEW1(errno, NULL,
                "Couldn't duplicate product-class: %s", strerror(errno));
        }
        else {
            *newClass = prodClass;
        }
    }
    else {
        if (tvIsNone(info->arrival)) {
            errObj = ERR_NEW(EINVAL, NULL,
                "Creation-time of last data-product is TS_NONE");
        }
        else {
            errObj = newSigClass(protoClass, info, newClass);
        }                               /* info->arrival != TS_NONE */
    }                                   /* NULL != info */

    return errObj;
}


/*******************************************************************************
 * Begin public API.
 ******************************************************************************/


/*
 * Initializes this requester.  The class of products actually received will be
 * the intersection of the requested class and the class allowed by the upstream
 * LDM.  This function logs messages via ulog(3).
 *
 * Arguments:
 *      upName                  Name of upstream host.
 *      port                    The port on which to connect.
 *      request                 Pointer to class of products to request.
 *      inactiveTimeout         if this much time, in seconds, passes without
 *                              hearing from upstream LDM, then check that it's
 *                              not dead.
 *      pqPathname              The pathname of the product-queue.
 *      *pq                     The product-queue.  Must be open for writing.
 *      isPrimary               Whether or not the initial data-product
 *                              exchange-mode should use HEREIS or
 *                              COMINGSOON/BLKDATA messages.
 * Returns:
 *      NULL                    Success.  as_shouldSwitch() is true.
 *      else                    Error.  err_code() values:
 *                                 REQ6_TIMED_OUT
 *                                      The connection timed-out.
 *                                 REQ6_UNKNOWN_HOST
 *                                      The upstream host is unknown.
 *                                 REQ6_BAD_VERSION
 *                                      The upstream LDM isn't version 6.
 *                                 REQ6_NO_CONNECT
 *                                      Couldn't connect to upstream LDM.
 *                                 REQ6_DISCONNECT
 *                                      Connection established, but then closed.
 *                                 REQ6_BAD_PATTERN
 *                                      The upstream LDM couldn't compile the
 *                                      regular expression of the request.
 *                                 REQ6_NOT_ALLOWED,
 *                                      The upstream LDM refuses to send the
 *                                      requested products.
 *                                 REQ6_BAD_RECLASS,
 *                                      The upstream LDM replied with an invalid
 *                                      RECLASS message.
 *                                 REQ6_SYSTEM_ERROR
 *                                      A fatal system-error occurred.
 */
ErrorObj*
req6_new(
    const char* const                   upName,
    const unsigned                      port,
    const prod_class_t* const             request,
    const unsigned                      inactiveTimeout,
    const char* const                   pqPathname,
    pqueue* const                       pq,
    const int                           isPrimary)
{
    prod_class_t* prodClass;
    ErrorObj*    errObj = adjustByLastInfo(request, &prodClass);

    assert(upName != NULL);
    assert(request != NULL);
    assert(inactiveTimeout > 0);
    assert(pqPathname != NULL);
    assert(pq != NULL);

    if (NULL != errObj) {
        errObj = ERR_NEW(REQ6_SYSTEM_ERROR, errObj,
            "Couldn't adjust product-class");
    }
    else {
        CLIENT*                 clnt;
        struct sockaddr_in      upAddr;

        unotice("LDM-6 desired product-class: %s",
            s_prod_class(NULL, 0, prodClass));

        errObj =
            ldm_clnttcp_create_vers(upName, port, SIX, &clnt, &requestSocket,
                &upAddr);

        (void)exitIfDone(0);

        if (errObj) {
            switch (err_code(errObj)) {
                case LDM_CLNT_UNKNOWN_HOST:
                    errObj = ERR_NEW(REQ6_UNKNOWN_HOST, errObj, NULL);
                    break;

                case LDM_CLNT_TIMED_OUT:
                    errObj = ERR_NEW(REQ6_TIMED_OUT, errObj, NULL);
                    break;

                case LDM_CLNT_BAD_VERSION:
                    errObj = ERR_NEW(REQ6_BAD_VERSION, errObj, NULL);
                    break;

                case LDM_CLNT_NO_CONNECT:
                    errObj = ERR_NEW(REQ6_NO_CONNECT, errObj, NULL);
                    break;

                case LDM_CLNT_SYSTEM_ERROR:
                    errObj = ERR_NEW(REQ6_SYSTEM_ERROR, errObj, NULL);
                    break;

                case LDM_CLNT_DONE:
                    err_free(errObj);
                    errObj = NULL;
                    break;
            }
        }                               /* no connection */
        else {
            /*
             * "clnt" and "requestSocket" have resources.
             */
            unsigned    id;

            uinfo("Connected to upstream LDM-6 on host %s using port %u",
                upName, (unsigned)ntohs(upAddr.sin_port));

            errObj = make_request(upName, prodClass, isPrimary, clnt, &id);

            if (!errObj) {
                errObj = as_init(0, isPrimary, requestSocket);

                if (NULL != errObj) {
                    errObj = ERR_NEW(REQ6_SYSTEM_ERROR, errObj, 
                        "Couldn't initialize autoshift module");
                }
                else {
                    /*
                     * Duplicate the socket to allow destruction of the client 
                     * handle.
                     */
                    dataSocket = dup(requestSocket);

                    if (-1 == dataSocket) {
                        errObj = ERR_NEW1(REQ6_SYSTEM_ERROR,
                            ERR_NEW(errno, NULL, strerror(errno)),
                            "Couldn't duplicate socket %d", requestSocket);
                    }
                    else {
                        auth_destroy(clnt->cl_auth);
                        clnt_destroy(clnt);
                        clnt = NULL;

                        (void)close(requestSocket);
                        requestSocket = -1;

                        udebug("%s:%d: Calling run_service()",
                            __FILE__, __LINE__);

                        errObj = run_service(dataSocket, inactiveTimeout,
                            upName, &upAddr, id, pqPathname, prodClass, pq);
      
                        (void)close(dataSocket);/* might be closed already */
                        dataSocket = -1;
                    }                   /* socket duplicated */
                }                       /* autoshift module initialized */
            }                           /* make_request() success */

            /*
             * Ensure release of client resources.
             */
            if (NULL != clnt) {
                auth_destroy(clnt->cl_auth);
                clnt_destroy(clnt);
            }

            /*
             * Ensure release of socket resources.
             */
            if (-1 != requestSocket) {
                (void)close(requestSocket);
                requestSocket = -1;
            }
        }                               /* ldm_clnttcp_create_vers() success */

        free_prod_class(prodClass);     /* NULL safe */
    }                                   /* "prodClass" allocated */

    return errObj;
}


/*
 * Closes the connection.  This is a safe function suitable for calling from a
 * signal handler.
 */
void
req6_close()
{
    if (requestSocket >= 0) {
        (void)close(requestSocket);
        requestSocket = -1;
    }

    if (dataSocket >= 0) {
        (void)close(dataSocket);
        dataSocket = -1;
    }

    if (isAliveSocket >= 0) {
        (void)close(isAliveSocket);
        isAliveSocket = -1;
    }
}
