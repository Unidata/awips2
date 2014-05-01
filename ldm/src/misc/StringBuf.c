#include "ldmconfig.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "StringBuf.h"


struct StringBuf {
    char*       buf;
    size_t      len;
    size_t      max;
    int         errCode;
};


/*
 * Returns an allocated StringBuf.
 *
 * Arguments:
 *      initMax         Initial capacity of the StringBuf, in characters,
 *                      excluding any terminating NUL.
 * Returns:
 *      NULL            Failure.
 *      else            Pointer to an allocated StringBuf.
 */
StringBuf*
strBuf_new(
    size_t      initMax )
{
    StringBuf*  strBuf = (StringBuf*)malloc(sizeof(StringBuf));

    if (NULL != strBuf) {
        char*   buf = (char*)malloc(initMax+1);

        if (NULL == buf) {
            free(strBuf);
            strBuf = NULL;
        }
        else {
            *buf = 0;
            strBuf->buf = buf;
            strBuf->len = 0;
            strBuf->max = initMax;
            strBuf->errCode = 0;
        }
    }

    return strBuf;
}


/*
 * Appends a string to the contents of a StringBuf.  If the StringBuf is valid
 * and the string cannot be appended, then the StringBuf is rendered invalid.
 *
 * Arguments:
 *      strBuf          Pointer to the StringBuf or NULL.
 *      string          String to be appended.  Caller may free upon return.
 * Returns:
 *      0               Success.
 *      EINVAL          "strBuf" is NULL or "*strBuf" is invalid.
 *      ENOMEM          Out-of-memory.
 */
int
strBuf_appendString(
    StringBuf* const    strBuf,
    const char* const   string)
{
    int         errCode = 0;            /* success */

    if (NULL == strBuf) {
        errCode = EINVAL;
    }
    else {
        if (0 != strBuf->errCode) {
            errCode = EINVAL;
        }
        else {
            size_t      len = strlen(string);
            size_t      newLen = strBuf->len + len;

            if (newLen > strBuf->max) {
                size_t  newMax = (size_t)(1.618034*newLen + 0.5);
                char*   newBuf = (char*)realloc(strBuf->buf, newMax+1);

                if (NULL == newBuf) {
                    strBuf->errCode = errno;
                    errCode = ENOMEM;
                }
                else {
                    strBuf->buf = newBuf;
                    strBuf->max = newMax;
                }
            }

            if (0 == errCode) {
                (void)strcpy(strBuf->buf+strBuf->len, string);

                strBuf->len += len;
            }
        }
    }

    return errCode;
}


/*
 * Clears a StringBuf.  On success, a subsequent strBuf_toString() will return
 * the empty string.
 *
 * Arguments:
 *      strBuf          Pointer to the StringBuf to be cleared.
 * Returns:
 *      0               Success.
 *      EINVAL          "strBuf" is NULL or "*strBuf" is invalid.
 */
int
strBuf_clear(
    StringBuf* const    strBuf)
{
    int         errCode;

    if (NULL == strBuf || 0 != strBuf->errCode) {
        errCode = EINVAL;
    }
    else {
        strBuf->buf[0] = 0;
        strBuf->len = 0;
        errCode = 0;
    }

    return errCode;
}


/*
 * Sets a StringBuf to a string.
 *
 * Arguments:
 *      strBuf          Pointer to the StringBuf.
 *      string          Pointer to the string to which "strBuf" will be set.
 *                      Caller may free upon return.
 * Returns:
 *      0               Success.
 *      EINVAL          "strBuf" is NULL or "*strBuf" is invalid.
 *      ENOMEM          Out-of-memory.
 */
int
strBuf_setToString(
    StringBuf* const    strBuf,
    const char* const   string)
{
    int         errCode = strBuf_clear(strBuf);

    if (0 == errCode)
        errCode = strBuf_appendString(strBuf, string);

    return errCode;
}


/*
 * Returns the contents of a StringBuf as a NUL-terminated string.
 *
 * Arguments:
 *      strBuf          Pointer to the StringBuf.
 * Returns:
 *      NULL            "strBuf" is NULL or "*strbuf" is invalid.
 *      else            The contents of "strBuf" as a NUL-terminated string.
 */
const char*
strBuf_toString(
    const StringBuf* const      strBuf)
{
    return
        NULL == strBuf || 0 != strBuf->errCode
            ? NULL
            : strBuf->buf;
}


/*
 * Returns an error-message string corresponding to the state of a StringBuf.
 *
 * Arguments:
 *      strBuf          Pointer to the StringBuf.  It must have been returned by
 *                      strBuf_new().
 * Returns:
 *      Pointer to a string describing the state of "strBuf".
 */
const char*
strBuf_strerror(
    const StringBuf* const      strBuf)
{
    return
        NULL == strBuf
            ? "The StringBuf couldn't be allocated: out-of-memory"
            : 0 == strBuf->errCode
                ? "No error"
                : strerror(strBuf->errCode);
}


/*
 * Releases any resouces associated with a StringBuf.
 *
 * Arguments:
 *      strBuf          Pointer to the StringBuf or NULL.
 */
void
strBuf_free(
    StringBuf* const    strBuf)
{
    if (NULL != strBuf) {
        free(strBuf->buf);
        free(strBuf);
    }
}
