/*
 * This module contains the "upstream" code for version 6 of the LDM.
 */
/* $Id: up6.c,v 1.14.2.3.2.2.2.2.2.19 2008/04/15 16:34:14 steve Exp $ */

#include "ldmconfig.h"

#include <assert.h>      /* assert() */
#include <errno.h>       /* error numbers */
#include <arpa/inet.h>   /* for <netinet/in.h> under FreeBSD 4.5-RELEASE */
#include <netinet/in.h>  /* sockaddr_in */
#include <rpc/rpc.h>     /* CLIENT, clnt_stat */
#include <signal.h>      /* sig_atomic_t */
#include <stdlib.h>      /* NULL, malloc() */
#include <string.h>      /* strerror() */
#include <strings.h>     /* strncasecmp() */
#if defined(_AIX) || 1
#   include <unistd.h>
#   include <fcntl.h>
#endif
#include <time.h>

#include "abbr.h"        /* set_abbr_ident() */
#include "acl.h"
#include "autoshift.h"
#include "error.h"
#include "ldm.h"         /* LDM version 6 client-side functions */
#include "ldmprint.h"    /* s_prod_class(), s_prod_info() */
#include "peer_info.h"   /* peer_info */
#include "pq.h"          /* pq_close(), pq_open() */
#include "prod_class.h"  /* clss_eq() */
#include "rpcutil.h"     /* clnt_errmsg() */
#include "UpFilter.h"
#include "ulog.h"
#include "globals.h"

#include "up6.h"


typedef enum {
    FEED,
    NOTIFY
} up6_mode_t;


static struct pqueue*           _pq;            /* the product-queue */
static const prod_class_t*        _class;         /* selected product-class */
static const signaturet*        _signature;     /* signature of last product */
static pq_match                 _mt = TV_GT;    /* time-matching condition */
static CLIENT*                  _clnt;          /* client-side transport */
static struct sockaddr_in       _downAddr;      /* downstream host addr. */
static UpFilter*                _upFilter;      /* filters data-products */
static up6_mode_t               _mode;          /* FEED, NOTIFY */
static int                      _socket = -1;   /* socket # */
static int                      _isPrimary;     /* use HEREIS or CSBD */
static unsigned                 _interval;      /* pq_suspend() interval */
static const char*              _downName;      /* downstream host name */
static time_t                   _lastSendTime;  /* time of last activity */
static int                      _flushNeeded;   /* connection needs a flush? */


typedef enum clnt_stat clnt_stat_t;


static up6_error_t
up6_error(
    clnt_stat_t stat)
{
    up6_error_t error;

    switch (stat) {
        case RPC_PROGVERSMISMATCH:
            error = UP6_VERSION_MISMATCH;
            break;
        case RPC_TIMEDOUT:
            error = UP6_TIME_OUT;
            break;
        case RPC_UNKNOWNHOST:
        case RPC_PMAPFAILURE:
        case RPC_PROGNOTREGISTERED:
        case RPC_PROGUNAVAIL:
            error = UP6_UNAVAILABLE;
            break;
        case RPC_CANTSEND:
            error = UP6_CLOSED;
            break;
        default:
            error = UP6_SYSTEM_ERROR;
    }

    return error;
}


/*
 * Arguments:
 *      info    Pointer to the data-product's metadata.
 *      data    Pointer to the data-product's data.
 *      xprod   Pointer to an XDR-encoded version of the data-product (data and
 *              metadata).
 *      size    Size, in bytes, of the XDR-encoded version.
 *      arg     Pointer to pointer to error-object:
 *                  NULL        Success.
 *                  else        Failure.  err_code() values:
 *                      UP6_CLIENT_FAILURE      Client-side RPC transport
 *                                              couldn't be created from
 *                                              Internet address and LDM program
 *                                              number.
 *                      UP6_VERSION_MISMATCH    Downstream LDM isn't version 6.
 *                      UP6_TIME_OUT            Communication timed-out.
 *                      UP6_INTERRUPT           This function was interrupted.  
 *                      UP6_UNKNOWN_HOST        Downstream host is unknown.
 *                      UP6_UNAVAILABLE         Downstream LDM can't be reached
 *                                              for some reason (see log).
 *                      UP6_SYSTEM_ERROR        System-error occurred (check
 *                                              errno or see log).
 *                      UP6_CLOSED              Connection closed.
 * Returns:
 *      0       Always.
 */
/*ARGSUSED*/
static int
notify(
    const prod_info* const      info,
    const void* const           data,
    void* const                 xprod,
    const size_t                size,
    void* const                 arg)
{
    ErrorObj** const     errObj = (ErrorObj**)arg;

    if (upFilter_isMatch(_upFilter, info)) {
        int     isDebug = ulogIsDebug();

        if (ulogIsVerbose() || isDebug)
            err_log_and_free(
                ERR_NEW1(0, NULL, "notifying: %s",
                    s_prod_info(NULL, 0, info, isDebug)),
                isDebug ? ERR_DEBUG : ERR_INFO);

        if (NULL == notification_6((prod_info*)info, _clnt)) {
            *errObj = ERR_NEW1(up6_error(clnt_stat(_clnt)), NULL,
                "NOTIFICATION failure: %s", clnt_errmsg(_clnt));
        }
        else {
            _lastSendTime = time(NULL);
            _flushNeeded = 1;
        }
    }

    return 0;
}


/*
 * Sets "_lastSendTime".
 *
 * Arguments:
 *      infop           Pointer to the metadata of the data.
 *      datap           Pointer to beginning of data.
 * Returns:
 *      NULL            Success.
 *      else            Error object. err_code() values:
 *          UP6_CLIENT_FAILURE      Client-side RPC transport couldn't be 
 *                                  created from Internet address and LDM 
 *                                  program number.
 *          UP6_VERSION_MISMATCH    Upstream LDM isn't version 6.
 *          UP6_TIME_OUT            Communication timed-out.
 *          UP6_INTERRUPT           This function was interrupted.    
 *          UP6_UNKNOWN_HOST        Downstream host is unknown.
 *          UP6_UNAVAILABLE         Downstream LDM can't be reached for some
 *                                  reason (see log).
 *          UP6_SYSTEM_ERROR        System-error occurred (check errno or see
 *                                  log).
 *          UP6_CLOSED              Connection closed.
 */
static ErrorObj*
hereis(
    const prod_info*    infop,
    const void*         datap)
{
    ErrorObj*   errObj = NULL;          /* success */
    product     prod;

    prod.info = *infop;
    prod.data = (void*)datap;

    if (NULL == hereis_6(&prod, _clnt)) {
        errObj = ERR_NEW1(up6_error(clnt_stat(_clnt)), NULL,
            "HEREIS: %s", clnt_errmsg(_clnt));
    }
    else {
        int     error;

        _lastSendTime = time(NULL);
        _flushNeeded = 1;
        error = as_hereis(1, infop->sz);        /* accepted */

        if (error) {
            errObj = ERR_NEW1(UP6_SYSTEM_ERROR, NULL,
                "Couldn't save acceptance of data-product: %s",
                strerror(error));
        }
        else {
            if (ulogIsDebug())
                udebug("%s", s_prod_info(NULL, 0, infop, 1));
        }
    }

    return errObj;
}


/*
 * Sets "_lastSendTime".
 *
 * Arguments:
 *      infop                   Pointer to the metadata of the data.
 *      datap                   Pointer to beginning of data.
 * Returns:
 *      NULL            Success.
 *      else            Error object. err_code() values:
 *          UP6_CLIENT_FAILURE      Client-side RPC transport couldn't be 
 *                                  created from Internet address and LDM 
 *                                  program number.
 *          UP6_VERSION_MISMATCH    Upstream LDM isn't version 6.
 *          UP6_TIME_OUT            Communication timed-out.
 *          UP6_INTERRUPT           This function was interrupted.    
 *          UP6_UNKNOWN_HOST        Upstream host is unknown.
 *          UP6_UNAVAILABLE         Upstream LDM can't be reached for some
 *                                  reason (see log).
 *          UP6_SYSTEM_ERROR        System-error occurred (check errno or see
 *                                  log).
 *          UP6_CLOSED              Connection closed.
 */
static ErrorObj*
csbd(
    const prod_info*    infop,
    const void*         datap)
{
    ErrorObj*           errObj = NULL;  /* success */
    comingsoon_args     comingSoon;
    comingsoon_reply_t* reply;

    comingSoon.infop = (prod_info*)infop;
    comingSoon.pktsz = infop->sz;
    reply = comingsoon_6(&comingSoon, _clnt);

    if (NULL == reply) {
        errObj = ERR_NEW1(up6_error(clnt_stat(_clnt)), NULL,
            "COMINGSOON: %s", clnt_errmsg(_clnt));
    }
    else {
        _lastSendTime = time(NULL);
        _flushNeeded = 0;               /* because synchronous RPC call */

        if (*reply == DONT_SEND) {
            /*
             * Notify the autoshift module of the rejection.  Use the 
             * approximate size of the COMINGSOON argument packet as the
             * amount of data.
             */
            int         error = as_comingsoon(0, (size_t)(
                sizeof(timestampt) +
                sizeof(signaturet) +
                strlen(infop->origin) +
                sizeof(feedtypet) +
                sizeof(u_int) +
                strlen(infop->ident) +
                sizeof(u_int) +
                sizeof(u_int)));

            if (error) {
                errObj = ERR_NEW1(UP6_SYSTEM_ERROR, NULL,
                    "Couldn't save rejection of data-product: %s",
                    strerror(error));
            }
        }
        else {
            datapkt         pkt;

            pkt.signaturep = (signaturet *)&infop->signature; /* not const */
            pkt.pktnum = 0;
            pkt.data.dbuf_len = infop->sz;
            pkt.data.dbuf_val = (void*)datap;

            if (NULL == blkdata_6(&pkt, _clnt)) {
                errObj = ERR_NEW1(up6_error(clnt_stat(_clnt)), NULL,
                    "Error sending BLKDATA: %s", clnt_errmsg(_clnt));
            }
            else {
                int     error;

                _lastSendTime = time(NULL);
                _flushNeeded = 1;       /* because asynchronous RPC call */
                error = as_comingsoon(1, infop->sz);    /* accepted */

                if (error) {
                    errObj = ERR_NEW1(UP6_SYSTEM_ERROR, NULL,
                            "Couldn't save acceptance of data-product: %s",
                            strerror(error));
                }
                else {
                    if (ulogIsDebug())
                        udebug("%s", s_prod_info(NULL, 0, infop, 1));
                }
            }
        }

        xdr_free((xdrproc_t)xdr_comingsoon_reply_t, (char*)reply);
    }                                   /* successful comingsoon_6() */

    return errObj;
}


/*
 * Transmits a data-product to a downstream LDM.  Called by pq_sequence().
 *
 * Arguments:
 *      info    Pointer to the data-product's metadata.
 *      data    Pointer to the data-product's data.
 *      xprod   Pointer to an XDR-encoded version of the data-product (data and
 *              metadata).
 *      size    Size, in bytes, of the XDR-encoded version.
 *      arg     Pointer to pointer to error-object:
 *                  NULL        Success.
 *                  else        Failure.  err_code() values:
 *                      UP6_CLIENT_FAILURE      Client-side RPC transport
 *                                              couldn't be created from
 *                                              Internet address and LDM program
 *                                              number.
 *                      UP6_VERSION_MISMATCH    Downstream LDM isn't version 6.
 *                      UP6_TIME_OUT            Communication timed-out.
 *                      UP6_INTERRUPT           This function was interrupted.  
 *                      UP6_UNKNOWN_HOST        Downstream host is unknown.
 *                      UP6_UNAVAILABLE         Downstream LDM can't be reached
 *                                              for some reason (see log).
 *                      UP6_SYSTEM_ERROR        System-error occurred (check
 *                                              errno or see log).
 *                      UP6_CLOSED              Connection closed.
 * Returns:
 *      0       Always.
 */
/*ARGSUSED*/
static int
feed(
    const prod_info* const      info,
    const void* const           data,
    void* const                 xprod,
    const size_t                size,
    void* const                 arg)
{
    ErrorObj** const     errObj = (ErrorObj**)arg;

    if (upFilter_isMatch(_upFilter, info)) {
        int     isDebug = ulogIsDebug();

        if (ulogIsVerbose() || isDebug)
            err_log_and_free(
                ERR_NEW1(0, NULL, "sending: %s",
                    s_prod_info(NULL, 0, info, isDebug)),
                isDebug ? ERR_DEBUG : ERR_INFO);

        *errObj = _isPrimary ? hereis(info, data) : csbd(info, data);

        if (NULL == *errObj) {
            if (as_shouldSwitch()) {
                err_log_and_free(
                    ERR_NEW1(0, NULL,
                        "Switching data-product send-mode from %s",
                            _isPrimary
                                ? "primary to alternate"
                                : "alternate to primary"),
                    ERR_NOTICE);

                _isPrimary = !_isPrimary;
            }
        }                               /* product successfully sent */
    }                                   /* product passes up-filter */

    return 0;
}


/*
 * Flushes the connection by sending a NULLPROC message and receiving the
 * (nil) acknowledgement.  Sets "_lastSendTime".
 *
 * Returns:
 *      NULL    Success.
 *      else    Error object.
 */
static ErrorObj*
flushConnection(void)
{
    ErrorObj*    errObj;

    if (nullproc_6(NULL, _clnt)) {
        _lastSendTime = time(NULL);
        _flushNeeded = 0;
        errObj = NULL;                  /* success */
        udebug("flushConnection(): nullproc_6 roundtrip");
    }
    else {
        errObj = ERR_NEW2(up6_error(clnt_stat(_clnt)), NULL,
            "nullproc_6() failure to %s: %s", 
            _downName, clnt_errmsg(_clnt));
    }

    return errObj;
}


/*
 * This function doesn't return until an error occurs.  It calls exitIfDone()
 * after potentially lengthy operations.
 *
 * Returns:
 *      0                    Success.
 *      UP6_CLIENT_FAILURE   Client-side RPC transport couldn't be 
 *                           created from Internet address and LDM program
 *                           number.
 *      UP6_SYSTEM_ERROR     System-error occurred (check errno or see log).
 *      UP6_PQ               Problem with product-queue.
 */
static up6_error_t
up6_run(void)
{
    int         errCode = 0;            /* success */
    int         flags;
    char        buf[64];
    char*       sig = _signature == NULL ? "NONE" :
        s_signaturet(buf, sizeof(buf), *_signature);

    assert(_mode == FEED || _mode == NOTIFY);
    assert(_class != NULL);

    if (NOTIFY == _mode) {
        set_abbr_ident(_downName, "(noti)");
        unotice("Starting Up(%s/6): %s, SIG=%s",
            ldm_version, s_prod_class(NULL, 0, _class), sig);
    }
    else {
        set_abbr_ident(_downName, "(feed)");
        unotice("Starting Up(%s/6): %s, SIG=%s, %s",
            ldm_version, s_prod_class(NULL, 0, _class),
            sig, _isPrimary ? "Primary" : "Alternate");
    }

    unotice("topo:  %s %s", _downName, upFilter_toString(_upFilter));
        /* s_feedtypet(clss_feedtypeU(_class))); */

    /*
     * Beginning with maintenance-level 11 of AIX 4.3.3 and
     * maintenance-level 5 of AIX 5.1, the TCP socket that will be
     * "turned around" is set to non-blocking -- contrary to the RPC,
     * socket, and TCP standards.  Because an upstream LDM assumes a
     * blocking socket to the downstream LDM, the following code is
     * necessary -- even though it shouldn't be.
     */
    flags = fcntl(_socket, F_GETFL);

    if (-1 == flags) {
        serror("fcntl(F_GETFL) failure");
        errCode = UP6_SYSTEM_ERROR;
    }
    else if ((flags & O_NONBLOCK) &&
        -1 == fcntl(_socket, F_SETFL, flags & ~O_NONBLOCK)) {

        serror("fcntl(F_SETFL) failure");
        errCode = UP6_SYSTEM_ERROR;
    }
    else {
        /*
         * Create a client-side RPC transport on the connection.
         */
        do {
            _clnt = clnttcp_create(&_downAddr, LDMPROG, SIX, &_socket, 
                MAX_RPC_BUF_NEEDED, 0);

            /* TODO: adjust sending buffer size in above */
        }
        while (_clnt == NULL && rpc_createerr.cf_stat == RPC_TIMEDOUT);

        if(_clnt == NULL) {
            uerror("Couldn't connect to downstream LDM on %s%s",
                _downName, clnt_spcreateerror(""));

            errCode = UP6_CLIENT_FAILURE;
        }
        else {
            ErrorObj*        error = as_init(1, _isPrimary, _socket);

            if (error != NULL) {
                err_log_and_free(
                    ERR_NEW(0, error,
                        "Couldn't initialize autoshift module"),
                    ERR_FAILURE);

                errCode = UP6_SYSTEM_ERROR;
            }
            else {
                while (0 == errCode && exitIfDone(0)) {
                    ErrorObj*   errObj = NULL;
                    int         err = pq_sequence(_pq, _mt, _class,
                        _mode == FEED ? feed : notify, &errObj);

                    (void)exitIfDone(0);

                    if (NULL != errObj) {
                        /*
                         * The feed() or notify() function reports a
                         * problem.
                         */
                        errCode = (up6_error_t)err_code(errObj);

                        err_log_and_free(
                            ERR_NEW(0, errObj, "feed or notify failure"),
                            ERR_NOTICE);
                    }
                    else if (err) {
                        /*
                         * The product-queue module reports a problem.
                         */
                        if (err == PQUEUE_END || 
                                err == EAGAIN || err == EACCES) {

                            if (_flushNeeded) {
                                errObj = flushConnection();

                                if (NULL != errObj) {
                                    errCode = (up6_error_t)err_code(errObj);

                                    err_log_and_free(
                                        ERR_NEW(0, errObj,
                                            "Couldn't flush connection"),
                                        ERR_FAILURE);
                                }

                                (void)exitIfDone(0);
                            }

                            if (errCode == 0) {
                                time_t      timeSinceLastSend =
                                    time(NULL) - _lastSendTime;

                                udebug(err == PQUEUE_END
                                    ? "End of product-queue"
                                    : "Hit a lock");

                                if (_interval <= timeSinceLastSend) {
                                    _flushNeeded = 1;
                                }
                                else {
                                    (void)pq_suspend(
                                        _interval - timeSinceLastSend);
                                }
                            }
                        }           /* end-of-queue reached or lock hit */
                        else {
                            uerror("Product send failure: %s", strerror(err));

                            errCode = UP6_PQ;
                        }
                    }               /* problem in product-queue module */
                }                   /* pq_sequence() loop */
            }                       /* autoshift module init-ed */

            auth_destroy(_clnt->cl_auth);
            clnt_destroy(_clnt);

            _clnt = NULL;
        }                               /* _clnt != NULL */
    }                                   /* socket set to blocking */

    return errCode;
}


/*
 * Destroys the upstream LDM module -- freeing resources.
 * This function prints diagnostic messages via the ulog(3) module.
 */
static void
up6_destroy(void)
{
    if (_clnt) {
        auth_destroy(_clnt->cl_auth);
        clnt_destroy(_clnt);
        _clnt = NULL;
    }

    if (_pq) {
        (void)pq_close(_pq);
        _pq = NULL;
    }
}


/*
 * Initializes the upstream LDM module.  This function prints diagnostic
 * messages via the ulog(3) module.
 *
 * Arguments:
 *      socket          Connected socket to be used by up6_t module.
 *      downName        Pointer to name of host of downstream LDM.  Caller must
 *                      not free or modify on successful return.
 *      downAddr        Pointer to address of host of downstream LDM.  Caller
 *                      must not free or modify on successful return.
 *      prodClass       Pointer to class of products to send.  Caller must not
 *                      free or modify on successful return.
 *      signature       Pointer to the signature of the last, successfully-
 *                      received data-product.  May be NULL.
 *      pqPath          Pointer to pathname of product-queue.
 *      interval        pq_suspend() interval in seconds.
 *      upFilter        Pointer to product-class for filtering data-products.
 *                      May not be NULL.
 *      mode            Transfer mode: FEED or NOTIFY.
 *      isPrimary       If "mode == FEED", then data-product exchange-mode.
 * Returns:
 *      0                       Success.
 *      UP6_PQ                  Problem with the product-queue.
 *      UP6_SYSTEM_ERROR        System-error occurred.  Message should be
 *                              logged and errno is set.
 */
static up6_error_t
up6_init(
    const int                           socket,
    const char* const                   downName,
    const struct sockaddr_in* const     downAddr,
    const prod_class_t* const             prodClass,
    const signaturet* const             signature,
    const char*                         pqPath,
    const unsigned                      interval,
    UpFilter* const                     upFilter,
    const up6_mode_t                    mode,
    int                                 isPrimary)
{
    int errCode;

    assert(socket >= 0);
    assert(downName != NULL);
    assert(prodClass != NULL);
    assert(pqPath != NULL);
    assert(upFilter != NULL);

    /*
     * Open the product-queue read-only.
     */
    if (errCode = pq_open(pqPath, PQ_READONLY, &_pq)) {
        if (PQ_CORRUPT == errCode) {
            uerror("The product-queue \"%s\" is inconsistent", pqPath);
        }
        else {
            uerror("Couldn't open product-queue \"%s\": %s",
                pqPath, strerror(errno)) ;
        }

        errCode = UP6_PQ;
    }
    else {
        int     cursorSet = 0;

        if (signature != NULL) {
            int         err = pq_setCursorFromSignature(_pq, *signature);

            if (err == 0) {
                _mt = TV_GT;
                cursorSet = 1;
            }
            else if (PQ_NOTFOUND == err) {
                err_log_and_free(
                    ERR_NEW1(0, NULL, "Data-product with signature "
                            "%s wasn't found in product-queue",
                        s_signaturet(NULL, 0, *signature)),
                    ERR_NOTICE);
            }
            else {
                err_log_and_free(
                    ERR_NEW2(0,
                        ERR_NEW(UP6_PQ, NULL, pq_strerror(_pq, err)),
                        "Couldn't set product-queue (%s) cursor from signature "
                            "(%s)",
                        pqPath, s_signaturet(NULL, 0, *signature)),
                    ERR_FAILURE);

                errCode = UP6_PQ;
            }
        }                               /* "signature != NULL" */

        if (errCode == 0 && !cursorSet) {
            int         err = pq_cClassSet(_pq,  &_mt, prodClass);

            if (err) {
                err_log_and_free(
                    ERR_NEW2(0, 
                        ERR_NEW(UP6_PQ, NULL, pq_strerror(_pq, err)),
                        "Couldn't set product-queue (%s) cursor from "
                            "product-class (%s)",
                        pqPath, s_prod_class(NULL, 0, prodClass)),
                    ERR_FAILURE);

                errCode = UP6_PQ;
            }
        }

        if (errCode == 0) {
            _class = prodClass;
            _signature = signature;
            _downName = downName;
            _clnt = NULL;
            _socket = socket;
            _downAddr = *downAddr;
            _interval = interval;
            _upFilter = upFilter;
            _lastSendTime = time(NULL);
            _flushNeeded = 0;
            _mode = mode;
            _isPrimary = isPrimary;

            errCode = UP6_SUCCESS;
        }                               /* product-queue cursor set */
    }                                   /* product-queue opened */

    return (up6_error_t)errCode;
}


/*******************************************************************************
 * Begin public API.
 ******************************************************************************/


/*
 * Constructs a new, upstream LDM object that feeds a downtstream LDM. function
 * prints diagnostic messages via the ulog(3) module.  It calls exitIfDone()
 * after potentially lengthy operations.
 *
 * Arguments:
 *      socket          Connected socket to be used by up6_t module.
 *      downName        Pointer to name of host of downstream LDM.  Caller may
 *                      free or modify on return.
 *      downAddr        Pointer to address of host of downstream LDM. Caller may
 *                      free or modify on return.
 *      prodClass       Pointer to class of products to send.  Caller may free
 *                      or modify on return.
 *      signature       Pointer to the signature of the last, successfully-
 *                      received data-product.  May be NULL.
 *      isPrimary       Whether data-product exchage-mode should be
 *                      primary (i.e., use HEREIS) or alternae (i.e.,
 *                      use COMINGSOON/BLKDATA).
 *      pqPath          Pointer to pathname of product-queue.  Caller may
 *                      free or modify on return.
 *      interval        pq_suspend() interval in seconds.
 *      upFilter        Pointer to product-class for filtering data-products.
 *                      May not be NULL.
 * Returns:
 *      0                       Success.
 *      UP6_PQ                  Problem with the product-queue.
 *      UP6_SYSTEM_ERROR        Failure.  errno is set and message should be
 *                              logged.
 */
int
up6_new_feeder(
    const int                           socket, 
    const char* const                   downName,
    const struct sockaddr_in* const     downAddr,
    const prod_class_t* const             prodClass,
    const signaturet* const             signature,
    const int                           isPrimary,
    const char*                         pqPath, 
    const unsigned                      interval,
    UpFilter* const                     upFilter)
{
    int         errCode = up6_init(socket, downName, downAddr, prodClass,
        signature, pqPath, interval, upFilter, FEED, isPrimary);

    if (!errCode) {
        errCode = up6_run();

        up6_destroy();
    }

    return errCode;
}


/*
 * Constructs a new, upstream LDM object that sends product notifications to a
 * downtstream LDM.  This function prints diagnostic messages via the ulog(3)
 * module.  It calls exitIfDone() after potentially lengthy operations.
 *
 * Arguments:
 *      socket          Connected socket to be used by up6_t module.
 *      downName        Pointer to name of host of downstream LDM.  Caller may
 *                      free or modify on return.
 *      downAddr        Pointer to address of host of downstream LDM. Caller may
 *                      free or modify on return.
 *      prodClass       Pointer to class of products to send.  Caller may free
 *                      or modify on return.
 *      signature       Pointer to the signature of the last, successfully-
 *                      received data-product.  May be NULL.
 *      pqPath          Pointer to pathname of product-queue.  Caller may
 *                      free or modify on return.
 *      interval        pq_suspend() interval in seconds.
 *      upFilter        Pointer to product-class for filtering data-products.
 *                      May not be NULL.
 * Returns:
 *      0                       Success.
 *      UP6_PQ                  Problem with the product-queue.
 *      UP6_SYSTEM_ERROR        Failure.  errno is set and message should be
 *                              logged.
 */
int
up6_new_notifier(
    const int                           socket, 
    const char* const                   downName,
    const struct sockaddr_in* const     downAddr,
    const prod_class_t* const             prodClass,
    const signaturet* const             signature,
    const char*                         pqPath, 
    const unsigned                      interval,
    UpFilter* const                     upFilter)
{
    int         errCode = up6_init(socket, downName, downAddr, prodClass,
        signature, pqPath, interval, upFilter, NOTIFY, 0);

    if (!errCode) {
        errCode = up6_run();

        up6_destroy();
    }

    return errCode;
}


/*
 * Closes all connections to the downstream LDM.  This safe function is
 * suitable for being called from a signal handler.
 */
void
up6_close()
{
    if (_socket >= 0) {
        (void)close(_socket);
        _socket = -1;
    }
}
