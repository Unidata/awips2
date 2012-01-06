#include "ldmconfig.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <regex.h>

#include "ldm.h"
#include "error.h"
#include "ldmprint.h"
#include "pattern.h"
#include "StringBuf.h"

#include "UpFilter.h"

typedef struct Element {
    struct Element*     next;
    Pattern*            okPattern;
    Pattern*            notPattern;
    feedtypet           ft;
} Element;

struct UpFilter {
    Element*    head;
    StringBuf*  strBuf;
    int         stringOutOfDate;
    unsigned    count;
};


/*
 * Returns a new upstream filter.
 *
 * Arguments:
 *      upFilter        Pointer to pointer to returned upstream filter.  Must
 *                      not be NULL.  Set on and only on success.
 * Returns:
 *      NULL            Success.
 *      else            Failure error object.
 */
ErrorObj*
upFilter_new(
    UpFilter** const    upFilter)
{
    ErrorObj*            errObj;
    size_t              nbytes = sizeof(UpFilter);
    UpFilter* const     filt = (UpFilter*)malloc(nbytes);

    if (NULL == filt) {
        errObj = ERR_NEW2(0, NULL, "Couldn't allocate %lu-bytes: %s",
            (unsigned long)nbytes, strerror(errno));
    }
    else {
        StringBuf* const strBuf = strBuf_new(132);

        if (NULL == strBuf) {
            errObj = ERR_NEW(0, NULL, strBuf_strerror(strBuf));

            free(filt);
        }
        else {
            filt->strBuf = strBuf;
            filt->head = NULL;
            filt->stringOutOfDate = 1;
            filt->count = 0;
            *upFilter = filt;
            errObj = NULL;              /* success */
        }
    }                                   /* "filt" allocated */

    return errObj;
}


/*
 * Adds a filter-component to an upstream-filter.
 *
 * Arguments:
 *      upFilter        Pointer to the upstream-filter to which to add the 
 *                      component.
 *      feedtype        The feedtype of the filter-component.
 *      okPattern       Pointer to the "OK pattern" of the filter-component to 
 *                      be added.  Caller may free upon return.
 *      notPattern      Pointer to the "not pattern" of the filter-component to
 *                      be added.  May be NULL to indicate that such matching
 *                      should be disabled.  Caller may free upon return.
 * Returns:
 *      NULL            Success.
 *      else            Error object.
 */
ErrorObj*
upFilter_addComponent(
    UpFilter* const             upFilter,
    const feedtypet             feedtype,
    const Pattern* const        okPattern,
    const Pattern* const        notPattern)
{
    ErrorObj*    errObj = NULL;          /* success */
    Element*    elt;

    /*
     * Ensure that the given feedtype is disjoint from all feedtypes already
     * in the filter.
     */
    for (elt = upFilter->head; NULL != elt; elt = elt->next) {
        if (feedtype & elt->ft) {
            char        ftSpec[512];

            (void)sprint_feedtypet(ftSpec, sizeof(ftSpec), feedtype);

            errObj = ERR_NEW2(0, NULL,
                "Feedtype %s overlaps with feedtype %s",
                ftSpec, s_feedtypet(elt->ft));

            break;
        }
    }

    if (NULL == errObj) {
        size_t          nbytes = sizeof(Element);

        elt = (Element*)malloc(nbytes);

        if (NULL == elt) {
            errObj = ERR_NEW2(0, NULL, "Couldn't allocate %lu-bytes: %s",
                (unsigned long)nbytes, strerror(errno));
        }
        else {
            if (errObj = pat_clone(&elt->okPattern, okPattern)) {
                errObj = ERR_NEW(0, errObj, "Couldn't clone \"OK\" pattern");
            }
            else {
                if (NULL == notPattern) {
                    elt->notPattern = NULL;
                }
                else {
                    if (errObj = pat_clone(&elt->notPattern, notPattern)) {
                        errObj = ERR_NEW(0, errObj,
                            "Couldn't clone \"not\" pattern");

                        pat_free(elt->okPattern);
                    }
                }

                if (!errObj) {
                    elt->ft = feedtype;
                    elt->next = upFilter->head;
                    upFilter->head = elt;
                    upFilter->stringOutOfDate = 1;
                    upFilter->count++;
                }
            }                           /* "elt->okPattern" set */
        }                               /* "elt" allocated */
    }

    return errObj;
}


/*
 * Returns the number of components in an UpFilter.
 *
 * Arguments:
 *      upFilter        Pointer to the UpFilter.
 * Returns:
 *      The number of components in "upFilter".
 */
unsigned
upFilter_getComponentCount(
    const UpFilter* const       upFilter)
{
    return upFilter->count;
}


/*
 * Indicates whether or not an upstream filter matches the information on a
 * product.
 *
 * Arguments:
 *      upFilter        Pointer to the upstream filter.
 *      info            Pointer to the product-information structure.
 * Returns:
 *      1               The upstream filter matches the product-information.
 *      0               The upstream filter doesn't match the 
 *                      product-information.
 */
int
upFilter_isMatch(
    const UpFilter* const       upFilter,
    const prod_info* const      info)
{
    int                 matches = 0;
    const Element*      elt;

    for (elt = upFilter->head; elt != NULL; elt = elt->next) {
        /*
         * The feedtypes of the Element-s must be disjoint in order for the
         * following to work.
         */
        if ((elt->ft & info->feedtype)) {
            matches = pat_isMatch(elt->okPattern, info->ident) &&
                (NULL == elt->notPattern || 
                    !pat_isMatch(elt->notPattern, info->ident));

            break;
        }
    }

    return matches;
}


/*
 * Returns a string representation of an UpFilter.
 *
 * Arguments:
 *      upFilter        The UpFilter whose string representation is to be
 *                      returned.
 * Returns:
 *      NULL            Failure to create a string representation of "upFilter".
 *      else            A pointer to a string representation of "upFilter".
 */
const char*
upFilter_toString(
    UpFilter* const     upFilter)
{
    const char* string = NULL;          /* failure */

    if (NULL != upFilter) {
        if (upFilter->stringOutOfDate) {
            StringBuf* const        strBuf = upFilter->strBuf;
            const Element*          elt;

            if (0 == strBuf_setToString(strBuf, "{")) {
                for (elt = upFilter->head; elt != NULL; elt = elt->next) {
                    if (elt != upFilter->head)
                        (void)strBuf_appendString(strBuf, ", ");

                    (void)strBuf_appendString(strBuf, "{");
                    (void)strBuf_appendString(strBuf, s_feedtypet(elt->ft));
                    (void)strBuf_appendString(strBuf, ", (");
                    (void)strBuf_appendString(strBuf,
                        pat_getEre(elt->okPattern));
                    (void)strBuf_appendString(strBuf, ")");

                    if (NULL != elt->notPattern) {
                        (void)strBuf_appendString(strBuf, " - (");
                        (void)strBuf_appendString(strBuf,
                            pat_getEre(elt->notPattern));
                        (void)strBuf_appendString(strBuf, ")");
                    }

                    (void)strBuf_appendString(strBuf, "}");
                }

                (void)strBuf_appendString(strBuf, "}");

                upFilter->stringOutOfDate = 0;
            }                           /* string-buffer correctly started */
        }                               /* string representation out-of-date */

        string = strBuf_toString(upFilter->strBuf);
    }                                   /* NULL != upFilter */

    return string;
}


/*
 * Frees the resources allocated to an upstream filter.
 *
 * Arguments:
 *      upFilter        Pointer to the upsteam filter to have its resources 
 *                      freed.
 */
void
upFilter_free(
    UpFilter* const     upFilter)
{
    if (NULL != upFilter) {
        Element*        elt;
        Element*        next;

        for (elt = upFilter->head; elt != NULL; elt = next) {
            next  = elt->next;

            pat_free(elt->okPattern);

            if (elt->notPattern) {
                pat_free(elt->notPattern);
            }

            free(elt);
        }

        strBuf_free(upFilter->strBuf);
        free(upFilter);
    }
}
