#include "ldmconfig.h"

#include <errno.h>
#include <string.h>

#include "prod_info.h"
#include "error.h"


static prod_info*       _info = NULL;   /* last saved product-info */
static int              wasSet = 0;


/*
 * Set the product-information for the data-product that was last successfully
 * received.
 *
 * Arguments:
 *      info    The data-product metadata to save.  May be NULL.  Caller may
 *              free upon return.
 * Returns:
 *      0       Success.  A subsequent call to savedInfo_wasSet() will return 1.
 *      ENOMEM  Out of memory.
 */
int
savedInfo_set(
    const prod_info* const      info)
{
    int         error = 0;              /* success */

    if (NULL == info) {
        pi_free(_info);                 /* NULL safe */
        _info = NULL;
    }
    else {
        if (NULL != _info) {
            error = pi_copy(_info, info);
        }
        else {
            _info = pi_clone(info);

            if (NULL == _info) {
                err_log_and_free(
                    ERR_NEW1(0, NULL,
                        "Couldn't clone product-information: %s",
                        strerror(errno)),
                    ERR_FAILURE);

                error = errno;
            }
        }
    }

    if (!error)
        wasSet = 1;

    return error;
}


/*
 * Returns a pointer to the most recently saved product-information.
 *
 * Returns:
 *      NULL    No data-product information has been saved.
 *      else    Pointer to metadata of the most recently saved 
 *              product-information.
 */
const prod_info*
savedInfo_get()
{
    return _info;
}


/*
 * Indicates whether or not savedInfo_set() was invoked since a previous
 * savedInfo_reset().
 *
 * Returns:
 *      0       No.
 *      1       Yes.
 */
int
savedInfo_wasSet(void)
{
    return wasSet;
}


/*
 * Resets savedInfo_wasSet().
 */
void
savedInfo_reset(void)
{
    wasSet = 0;
}


/*
 * Returns an error message.
 *
 * Arguments:
 *      error   Error code of the failed function.
 * Returns:
 *      Pointer to the 0-terminated error message.  Caller must not free.
 */
const char*
savedInfo_strerror(
    const int   error)
{
    static char buf[256];

    (void)sprintf(buf, "Couldn't clone product-information: %s",
        strerror(error));

    return buf;
}
