/*
 * This module contains the "downstream" code for version 6 of the LDM.
 */
/* $Id: down6.c,v 1.24.2.1.2.1.4.17 2009/07/23 20:02:13 steve Exp $ */

#include "ldmconfig.h"

#include <arpa/inet.h>   /* inet_ntoa() */
#include <assert.h>      /* assert() */
#include <errno.h>       /* error numbers */
#include <limits.h>      /* UINT_MAX */
#include <netinet/in.h>  /* sockaddr_in */
#include <rpc/rpc.h>
#include <stdlib.h>      /* NULL, malloc() */
#include <string.h>      /* strerror(), ... */

#include "acl.h"
#include "autoshift.h"
#include "DownHelp.h"
#include "error.h"
#include "globals.h"     /* inactive_timeo */
#include "ldm.h"         /* client-side LDM functions */
#include "ldmprint.h"    /* s_prod_info() */
#include "peer_info.h"   /* peer_info */
#include "pq.h"          /* pq_*(), pqe_*() */
#include "prod_class.h"  /* clss_eq(), prodInClass() */
#include "prod_info.h"
#include "savedInfo.h"
#include "timestamp.h"   /* clss_eq(), prodInClass() */
#include "ulog.h"
#include "xdr_data.h"

#include "down6.h"


typedef enum {
    DONT_EXPECT_BLKDATA,
    EXPECT_BLKDATA,
    IGNORE_BLKDATA
} comingSoonMode;


static struct pqueue* _pq;              /* product-queue */
static prod_class_t*    _class;           /* product-class to accept */
static char*          _datap;           /* BLKDATA area */
static prod_info*     _info;            /* product-info */
static unsigned       _remaining;       /* remaining BLKDATA bytes */
static int            _expectBlkdata;   /* expect BLKDATA message? */
static int            _initialized;     /* module initialized? */
static char           _upName[MAXHOSTNAMELEN+1];        /* upstream host name */
static char           _dotAddr[DOTTEDQUADLEN];  /* dotted-quad IP address */


/*******************************************************************************
 * Begin public API.
 ******************************************************************************/


/*
 * Initializes new this module from an upstream hostname, the pathname of the
 * product-queue, and a flag to watch.  The "arrival" time of the most-recently
 * received product is initialized to TS_NONE.  This function prints diagnostic
 * messages via the ulog(3) module.  This function may be called multiple times
 * without calling down6_destroy().
 *
 * @param upName                 Name of upstream host.
 * @param *upAddr                Address of upstream host.
 * @param pqPath                 Pathname of product-queue.
 * @param *pq                    Product-queue.
 * @return 0                     if success.
 * @return DOWN6_SYSTEM_ERROR    if system-error occurred (check errno or see
 *                               log).
 */
int down6_init(
    const char               *upName, 
    const struct sockaddr_in *upAddr,
    const char               *pqPath,
    pqueue                   *const pq)
{
    int         errCode = 0;            /* success */

    assert(upName != NULL);
    assert(upAddr != NULL);
    assert(pqPath != NULL);
    assert(pq != NULL);

    _initialized = 0;

    (void)strcpy(_dotAddr, inet_ntoa(upAddr->sin_addr));
    (void)strncpy(_upName, upName, sizeof(_upName)-1);

    _upName[sizeof(_upName)-1] = 0;

    free_prod_class(_class);            /* NULL safe */
    _class = NULL;

    _datap = NULL;
    _pq = pq;
    _expectBlkdata = 0;

    if (NULL == _info) {
        _info = pi_new();

        if (NULL == _info) {
            err_log_and_free(
                ERR_NEW1(0, NULL,
                    "Couldn't allocate new product-information structure: %s",
                    strerror(errno)),
                ERR_FAILURE);

            errCode = DOWN6_SYSTEM_ERROR;
        }
    }

    if (!errCode)
        _initialized = 1;

    return errCode;
}


/*
 * Sets the class of products that the downstream LDM module will accept.
 * The caller may free the "offered" argument on return.
 *
 * Arguments:
 *      offered         Pointer to class of offered products.
 * Returns:
 *      0                       Success.
 *      DOWN6_SYSTEM_ERROR      System error.  errno is set.
 *      DOWN6_UNINITIALIZED     Module not initialized.
 */
int down6_set_prod_class(prod_class_t *offered)
{
    int         errCode = 0;            /* success */

    if (!_initialized) {
        uerror("down6_set_prod_class(): Module not initialized");
        errCode = DOWN6_UNINITIALIZED;
    }
    else {
        if (clsspsa_eq(_class, offered)) {
            /*
             * Update the time range.
             */
            _class->from = offered->from;
            _class->to = offered->to;
        }
        else {
            prod_class_t *pc = dup_prod_class(offered);

            if (pc == NULL) {
                errCode = DOWN6_SYSTEM_ERROR;
            }
            else {
                free_prod_class(_class);  /* NULL safe */

                _class = pc;
            }
        }
    }                                   /* module initialized */

    return errCode;
}


/*
 * Returns a copy of the product-class that this downstream LDM module will
 * accept.  The caller is responsible for invoking free_prod_class(prod_class_t*)
 * on a returned, non-NULL pointer.  This function prints diagnostic messages
 * via ulog(3).
 *
 * Returns:
 *      NULL            The product-class of this module couldn't be duplicated
 *                      or down6_set_prod_class() hasn't been called with a
 *                      valid product-class.
 *      else            A pointer to a copy of the product-class that this 
 *                      downstream LDM module wants.  free_prod_class() when 
 *                      done if non-NULL.
 */
prod_class_t *down6_get_prod_class()
{
    prod_class_t *clone;

    if (NULL == _class) {
        uerror("down6_get_prod_class(): Product-class not set");
        clone = NULL;
    }
    else {
        clone = dup_prod_class(_class);

        if (clone == NULL)
            uerror("Couldn't allocate new product-class: %s",
                strerror(errno));
    }

    return clone;
}


/*
 * Handles a product.  This function prints diagnostic messages via the ulog(3)
 * module.  On successful return, savedInfo_get() will return the metadata 
 * of the product.
 *
 * This function updates "_class->from".
 *
 * Arguments:
 *      prod         Pointer to the data-product.
 * Returns:
 *      0                       Success.
 *      DOWN6_UNWANTED          Unwanted data-product (e.g., duplicate, too 
 *                              old).
 *      DOWN6_PQ                The product couldn't be inserted into the
 *                              product-queue.
 *      DOWN6_PQ_BIG            The product is too big to be inserted into
 *                              the product-queue.
 *      DOWN6_SYSTEM_ERROR      System error.
 *      DOWN6_UNINITIALIZED     Module not initialized.
 */
int
down6_hereis(
    product*    prod)
{
    int         errCode = 0;            /* success */

    if (!_initialized) {
        uerror("down6_hereis(): Module not initialized");
        errCode = DOWN6_UNINITIALIZED;
    }
    else {
        prod_info *infop = &prod->info;

        (void)set_timestamp(&_class->from);
        _class->from.tv_sec -= max_latency;
        dh_setInfo(_info, infop, _upName);

        if (!prodInClass(_class, infop)) {
            if (tvCmp(_class->from, infop->arrival, >)) {
                if (ulogIsVerbose()) {
                    err_log_and_free(
                        ERR_NEW1(0, NULL, "Ignoring too-old product: %s",
                            s_prod_info(NULL, 0, infop, ulogIsDebug())),
                        ERR_INFO);
                }
            }
            else if (ulogIsVerbose()) {
                err_log_and_free(
                    ERR_NEW1(0, NULL, "Ignoring unrequested product: %s",
                        s_prod_info(NULL, 0, infop, ulogIsDebug())),
                    ERR_INFO);
            }
            errCode = savedInfo_set(_info);
            if (errCode) {
                err_log_and_free(
                    ERR_NEW1(0, NULL,
                        "Couldn't save product-information: %s",
                        savedInfo_strerror(errCode)),
                    ERR_FAILURE);

                errCode = DOWN6_SYSTEM_ERROR;
            }
            else {
                errCode = DOWN6_UNWANTED;
            }
        }
        else {
            errCode = dh_saveDataProduct(_pq, _info, prod->data, 1, 1);
        }                               /* product in desired class */
    }                                   /* module initialized */

    return errCode;
}


/*
 * Handles a product notification.  This method should never be called.
 * An informational message is emitted via the ulog(3) module.
 *
 * Arguments:
 *      info                    Pointer to the product metadata.
 *      0                       Sucess.
 *      DOWN6_UNINITIALIZED     Module not initialized.
 */
/*ARGSUSED*/
int down6_notification(prod_info *info)
{
    /*
     * This should never be called.
     */
    int         errCode = 0;            /* success */

    if (!_initialized) {
        uerror("down6_notification(): Module not initialized");
        errCode = DOWN6_UNINITIALIZED;
    }
    else {
        uwarn("notification6: %s", s_prod_info(NULL, 0, info, ulogIsDebug()));
    }

    return errCode;
}


/*
 * Handles a product that will be delivered in pieces.
 *
 * This function updates "_class->from".
 *
 * Arguments:
 *      argp      Pointer to the product delivery parameters.
 * Returns:
 *      0                       Success.
 *      DOWN6_UNWANTED          Unwanted data-product (e.g., duplicate, too 
 *                              old, too big).
 *      DOWN6_PQ                Fatal problem with product-queue.
 *      DOWN6_PQ_BIG            Data-product too big for product-queue
 *      DOWN6_SYSTEM_ERROR      System error.
 *      DOWN6_UNINITIALIZED     Module not initialized.
 */
int
down6_comingsoon(
    comingsoon_args*    argp)
{
    int         errCode = 0;            /* success */

    if (!_initialized) {
        uerror("down6_comingsoon(): Module not initialized");
        errCode = DOWN6_UNINITIALIZED;
    }
    else {
        prod_info *infop = argp->infop;

        if (_expectBlkdata) {
            uwarn("%s:%d: Discarding incomplete product: %s",
                __FILE__, __LINE__, s_prod_info(NULL, 0, infop, ulogIsDebug()));

            _expectBlkdata = 0;
        }

        (void)set_timestamp(&_class->from);
        _class->from.tv_sec -= max_latency;
        dh_setInfo(_info, infop, _upName);

        if (!prodInClass(_class, infop)) {
            if (tvCmp(_class->from, infop->arrival, >)) {
                if (ulogIsVerbose())
                    err_log_and_free(
                        ERR_NEW1(0, NULL, "Ignoring too-old product: %s",
                            s_prod_info(NULL, 0, infop, ulogIsDebug())),
                        ERR_INFO);
            }
            else if (ulogIsVerbose()) {
                err_log_and_free(
                    ERR_NEW1(0, NULL, "Ignoring unrequested product: %s",
                        s_prod_info(NULL, 0, infop, ulogIsDebug())),
                    ERR_INFO);
            }
            errCode = savedInfo_set(_info);
            if (errCode) {
                err_log_and_free(
                    ERR_NEW1(0, NULL,
                        "Couldn't save product-information: %s",
                        savedInfo_strerror(errCode)),
                    ERR_FAILURE);

                errCode = DOWN6_SYSTEM_ERROR;
            }
            else {
                errCode = DOWN6_UNWANTED;
            }
        }                                   /* product isn't in desired class */
        else {
            pqe_index      idx;             /* product-queue index */

            /*
             * Reserve space for the data-product in the product-queue.
             */
            errCode = pqe_new(_pq, _info, (void **)&_datap, &idx);

            if (!errCode) {
                /*
                 * The data-product isn't in the product-queue.  Setup for
                 * receiving the product's data via BLKDATA messages.
                 */
                (void)pqe_discard(_pq, idx);
                _expectBlkdata = 1;

                /*
                 * Use the growable buffer of the "XDR-data" module as the
                 * location into which the XDR layer will decode the data.
                 */
                _remaining = _info->sz;
                _datap = xd_getBuffer(_remaining);
            }                           /* pqe_new() success */
            else if (errCode == EINVAL) {
                /*
                 * The data-product is invalid.
                 */
                err_log_and_free(
                    ERR_NEW1(0, NULL, "Invalid product: %s", 
                        s_prod_info(NULL, 0, _info, ulogIsDebug())),
                    ERR_FAILURE);

                errCode = DOWN6_UNWANTED;
                if (savedInfo_set(_info))
                    errCode = DOWN6_SYSTEM_ERROR;
            }                           /* invalid product */
            else if (errCode == PQUEUE_BIG) {
                /*
                 * The data-product is too big to insert into the
                 * product-queue.
                 */
                uerror("Product too big: %s",
                        s_prod_info(NULL, 0, infop, ulogIsDebug()));

                errCode = DOWN6_PQ_BIG;
                if (savedInfo_set(_info)) {
                    errCode = DOWN6_SYSTEM_ERROR;
                }
            }                           /* product too big */
            else if (errCode == PQUEUE_DUP) {
                /*
                 * The data-product is already in the product-queue.
                 */
                if (ulogIsVerbose() || ulogIsDebug())
                    uinfo("comingsoon6: duplicate: %s",
                        s_prod_info(NULL, 0, infop, ulogIsDebug()));

                errCode = DOWN6_UNWANTED;
                if (savedInfo_set(_info)) {
                    errCode = DOWN6_SYSTEM_ERROR;
                }
                else {
                    /*
                     * Notify the autoshift module of the rejection.
                     * Use the approximate size of the COMINGSOON
                     * argument packet as the amount of data.
                     */
                    int error = as_comingsoon(0, 
                        (size_t)(sizeof(InfoBuf) + 2*sizeof(u_int)));

                    if (error) {
                        err_log_and_free(
                            ERR_NEW1(0, NULL,
                                "Couldn't save rejection of "
                                "data-product: %s", strerror(error)),
                            ERR_FAILURE);

                        errCode = DOWN6_SYSTEM_ERROR;
                    }
                }                   /* "savedInfo" updated */
            }                       /* duplicate data-product */
            else {
                err_log_and_free(
                    ERR_NEW2(0, NULL, "pqe_new() failed: %s: %s", 
                        strerror(errCode), s_prod_info(NULL, 0, _info, 1)),
                    ERR_FAILURE);

                errCode = DOWN6_PQ;     /* fatal product-queue error */
            }                           /* general pqe_new() failure */
        }                               /* product is in desired class */
    }                                   /* module initialized */

    return errCode;
}


/*
 * Accepts a block of data.  When the all of the outstanding data has been
 * successfuly received, savedInfo_get() will return the metadata of the
 * just-received data-product.
 *
 * Arguments:
 *      dpkp             Pointer to the block of data.
 * Returns:
 *      0                       Success.
 *      DOWN6_PQ                Problem with product-queue.
 *      DOWN6_PQ_BIG            Data product too big for product-queue.
 *      DOWN6_BAD_PACKET        The data packet is invalid.
 *      DOWN6_SYSTEM_ERROR      System error.
 *      DOWN6_UNINITIALIZED     Module not initialized.
 */
int
down6_blkdata(
    datapkt*    dpkp)
{
    int         errCode = 0;            /* success */

    if (!_initialized) {
        uerror("down6_blkdata(): Module not initialized");
        errCode = DOWN6_UNINITIALIZED;
    }
    else {
        if (!_expectBlkdata) {
            uwarn("%s:%d: Unexpected BLKDATA", __FILE__, __LINE__);
        }
        else {
            if(memcmp(dpkp->signaturep, _info->signature, sizeof(signaturet))
                != 0) {

                uwarn("%s:%d: Invalid BLKDATA signature", __FILE__, __LINE__);

                errCode = DOWN6_BAD_PACKET;
            }
            else {
                dbuf*    data = &dpkp->data;
                unsigned got = data->dbuf_len;

                if (got > _remaining) {
                    uwarn(
                        "%s:%d: BLKDATA size too large: remaining %u; got %u",
                        __FILE__, __LINE__, _remaining, got);
                    xd_reset();

                    _expectBlkdata = 0;
                    errCode = DOWN6_BAD_PACKET;
                }
                else {
                    /*
                     * The XDR layer has already decoded the packet's data into
                     * the buffer of the "XDR-data" module.
                     */
                    _remaining -= got;

                    if (0 == _remaining) {
                        errCode = dh_saveDataProduct(_pq, _info, _datap, 0, 1);
                        _expectBlkdata = 0;

                        xd_reset();
                    }                   /* received all bytes */
                }                       /* size <= remaining */
            }                           /* right MD5 checksum */
        }                               /* expect BLKDATA message */
    }                                   /* module initialized */

    return errCode;
}


/*
 * Destroys this down6_t module -- freeing any allocated resources.  This
 * function prints diagnostic messages via the ulog(3) module.  This function
 * is idempotent.
 */
void down6_destroy()
{
    free_prod_class(_class);            /* NULL safe */
    _class = NULL;

    if (_expectBlkdata) {
        uinfo("%s:%d: Discarding incomplete product: %s", __FILE__, __LINE__,
            s_prod_info(NULL, 0, _info, ulogIsDebug()));

        _expectBlkdata = 0;
    }

    pi_free(_info);                     /* NULL safe */
    _info = NULL;

    _pq = NULL;
    _initialized = 0;
}
