/*
 * This module contains the "downstream" code for version 6 of the LDM.
 */
/* $Id: DownHelp.c,v 1.1.2.9 2009/06/18 23:37:20 steve Exp $ */

#include "ldmconfig.h"

#include <string.h>

#include "autoshift.h"
#include "down6.h"
#include "error.h"
#include "ldm.h"
#include "ldmprint.h"
#include "peer_info.h"
#include "pq.h"
#include "prod_info.h"
#include "savedInfo.h"
#include "ulog.h"

#ifndef MIN
#  define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif


/*
 * Copy as many characters as possible to a given position in a buffer of
 * fixed size.  A terminating NUL is not added to the buffer.  The buffer and
 * suffix strings may overlap.
 *
 * Arguments:
 *      buf             Pointer to the buffer.
 *      max             Maximum number of characters (excluding any terminating
 *                      NUL) that the buffer can hold.
 *      used            Number of characters already in the buffer.
 *      suffix          Pointer to the string to be appended to the characters
 *                      in the buffer (at least as many characters as possible).
 *      len             Number of characters in "suffix" to be considered for
 *                      appending (excluding any terminating NUL).
 * Returns:
 *      Number of characters actually appended (might be zero).
 */
static size_t
appendMax(
    char* const         buf,
    const size_t        max,
    const size_t        used,
    const char* const   suffix,
    size_t              len)
{
    if (used >= max) {
        len = 0;
    }
    else {
        size_t  left = max - used;

        if (left < len)
            len = left;
    }

    (void)memmove(buf+used, suffix, len);

    return len;
}


/*
 * Returns an "origin" identifier based on a template and the identifier of the
 * upstream host.  The format of the returned origin identifier will be
 *
 *     <creating host>_v_<upstream host>
 *
 * where <creating host> is the name of the computer that originally created
 * the data-product and <upstream_host> is the upstream host as given to this
 * function.
 *
 * Arguments:
 *      oldOrigin       Pointer to the original origin identifier.
 *      hostId          Pointer to the identifier of the upstream host.
 * Returns:
 *      Pointer to a static buffer containing the new origin identifier.
 */
static const char*
getNewOrigin(
    const char* const   origin,
    const char* const   hostId)
{
    static const char   sepStr[] = "_v_";
#   define              SEP_STR_LEN 3
    static char         buf[MAXHOSTNAMELEN + SEP_STR_LEN + MAXHOSTNAMELEN + 1];
    static const size_t max = sizeof(buf) - 1;
    char*               cp = strstr(origin, sepStr);
    size_t              used = appendMax(buf, max, 0, origin,
        cp == NULL ? strlen(origin) : (size_t)(cp - origin));

    used += appendMax(buf, max, used, sepStr, SEP_STR_LEN);
    used += appendMax(buf, max, used, hostId, strlen(hostId));
    buf[used] = '\0';

    return buf;
}


/*
 * Populates a prod_info structure based on a template and an identifier for the
 * upstream host.
 *
 * Arguments:
 *      newInfo         Pointer to prod_info structure to be set.  Must have
 *                      been returned by pi_new().
 *      oldInfo         Pointer to the template prod_info structure.
 *      hostId          Pointer to the identifier of the upstream host.
 */
void
dh_setInfo(
    prod_info* const            newInfo,
    const prod_info* const      oldInfo,
    const char* const           hostId)
{
    (void)pi_copy(newInfo, oldInfo);
    pi_setOrigin(newInfo, getNewOrigin(oldInfo->origin, hostId));
}


/*
 * Tries to write a data-product to the product-queue.  Calls savedInfo_set()
 * on success or if the data-product is already in the product-queue.  Calls
 * as_hereis() or as_comingsoon() appropriately.
 *
 * Arguments:
 *      pq              Pointer to product-queue structure.
 *      info            Pointer to the product-information.
 *      data            Pointer to the product-data.
 *      wasHereis       Whether or not the data-product was received via a
 *                      HEREIS message.
 *      notifyAutoShift Whether or not to notify the autoshift module.
 * Returns:
 *      0                       Success.
 *      DOWN6_SYSTEM_ERROR      System failure.
 *      DOWN6_PQ                Fatal product-queue failure.
 *      DOWN6_PQ_BIG            Product is too big to insert into product-queue.
 *      DOWN6_UNWANTED          Data-product already in product-queue.
 */
int
dh_saveDataProduct(
    struct pqueue* const        pq,
    const prod_info* const      info,
    void* const                 data,
    const int                   wasHereis,
    const int                   notifyAutoShift)
{
    int     retCode = 0;                /* success */
    int     error;
    product newprod;

    newprod.info = *info;
    newprod.data = data;

    error = pq_insert(pq, &newprod);

    if (!error) {
        if (ulogIsVerbose())
            uinfo("%s", s_prod_info(NULL, 0, info, ulogIsDebug()));

        error = savedInfo_set(info);
        if (error) {
            err_log_and_free(
                ERR_NEW1(0, NULL, "Couldn't save product-information: %s",
                    savedInfo_strerror(error)),
                ERR_FAILURE);

            retCode = DOWN6_SYSTEM_ERROR;
        }
        else {
            if (notifyAutoShift) {
                error = 
                    wasHereis
                        ? as_hereis(1, info->sz)
                        : as_comingsoon(1, info->sz);

                if (error) {
                    err_log_and_free(
                        ERR_NEW1(0, NULL,
                            "Couldn't save acceptance of data-product: %s",
                            strerror(error)),
                        ERR_FAILURE);

                    retCode = DOWN6_SYSTEM_ERROR;
                }
            }                           /* "notifyAutoShift" set */
        }                               /* "savedInfo" updated */
    }                                   /* data-product inserted */
    else if (PQUEUE_BIG == error) {
        uerror("Product too big: %s",
            s_prod_info(NULL, 0, info, ulogIsDebug()));
        retCode = DOWN6_PQ_BIG;

        error = savedInfo_set(info);
        if (error) {
            err_log_and_free(
                ERR_NEW1(0, NULL,
                    "Couldn't save product-information: %s",
                    savedInfo_strerror(error)),
                ERR_FAILURE);

            retCode = DOWN6_SYSTEM_ERROR;
        }
    }                                   /* product too big */
    else if (PQUEUE_DUP == error) {
        if (ulogIsVerbose())
            uinfo("%s: duplicate: %s",
                wasHereis ? "hereis" : "comingsoon/blkdata",
                s_prod_info(NULL, 0, info, ulogIsDebug()));

        retCode = DOWN6_UNWANTED;

        error = savedInfo_set(info);
        if (error) {
            err_log_and_free(
                ERR_NEW1(0, NULL,
                    "Couldn't save product-information: %s",
                    savedInfo_strerror(error)),
                ERR_FAILURE);

            retCode = DOWN6_SYSTEM_ERROR;
        }
        else {
            if (notifyAutoShift) {
                error =
                    wasHereis
                        ? as_hereis(0, info->sz)
                        : as_comingsoon(0, info->sz);

                if (error) {
                    err_log_and_free(
                        ERR_NEW1(0, NULL,
                            "Couldn't save rejection of data-product: %s",
                            strerror(error)),
                        ERR_FAILURE);

                    retCode = DOWN6_SYSTEM_ERROR;
                }
            }                       /* "notifyAutoShift" set */
        }                           /* "savedInfo"' updated */
    }                               /* duplicate data-product */
    else {
        uerror("pq_insert() failed: %s: %s",
            strerror(error), s_prod_info(NULL, 0, info, ulogIsDebug()));

        retCode = DOWN6_PQ;             /* fatal product-queue error */
    }                                   /* general insertion failure */

    return retCode;
}
