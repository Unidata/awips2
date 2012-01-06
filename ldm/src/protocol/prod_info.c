#include "ldmconfig.h"
#include "ldm.h"
#include "ldmprint.h"
#include "prod_info.h"
#include "timestamp.h"
#include "log.h"
#include "atofeedt.h"

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAGIC_VALUE     0x60231023U


int
isValid(
    const prod_info* const      info)
{
    return MAGIC_VALUE == ((InfoBuf*)info)->magic;
}


/*
 * Initializes an InfoBuf.  An InfoBuf must not be used until it has been
 * initialized.  The members of the prod_info structure are set as follows:
 *      arrival:        TS_NONE
 *      feedtype:       NONE
 *      seqno:          0
 *      sz:             0
 *      origin:         ""
 *      ident:          ""
 *      signature:      {0}
 *
 * Arguments:
 *      buf     Pointer to the InfoBuf to be initialized.
 * Returns:
 *      NULL    "buf" is NULL.
 *      else    Pointer to the prod_info member of "buf".
 */
prod_info*
ib_init(
    InfoBuf* const      buf)
{
    prod_info*  info;

    if (NULL == buf) {
        info = NULL;
    }
    else {
        buf->magic = MAGIC_VALUE;
        info = &buf->info;

        info->arrival = TS_NONE;
        info->feedtype = NONE;
        info->seqno = 0;
        info->sz = 0;
        info->origin = buf->origin;
        info->origin[0] = 0;
        info->ident = buf->ident;
        info->ident[0] = 0;

        (void)memset(info->signature, 0, sizeof(signaturet));
    }

    return info;
}


/*
 * Returns a completely-allocated prod_info.  "Completely-allocated" means that
 * the "origin" and "ident" members point to buffers sufficient to hold any
 * valid value.  The members are initialized as follows:
 *      arrival:        TS_NONE
 *      feedtype:       NONE
 *      seqno:          0
 *      sz:             0
 *      origin:         ""
 *      ident:          ""
 *      signature:      {0}
 *
 * Returns:
 *      NULL    Failure.  errno is set.
 *      else    A completely-allocated prod_info.
 */
prod_info*
pi_new(void)
{
    prod_info*  info = NULL;            /* failure */
    InfoBuf*    buf = (InfoBuf*)malloc(sizeof(InfoBuf));

    if (NULL != buf)
        info = ib_init(buf);

    return info;
}


/*
 * Sets the "origin" member of a prod_info.  Only the first HOSTNAMESIZE 
 * characters will be used.  The "origin" member will be 0-terminated.
 *
 * Arguments:
 *      info    The prod_info to have its "origin" member set.  It should have
 *              been created by pi_new() or pi_clone() or be a member of an
 *              InfoBuf initialized by ib_init().  May not be NULL.
 *      origin  The value to which the "origin" member of "info" should be set.
 *              May be freed by caller upon return.  May not be NULL.
 */
void
pi_setOrigin(
    prod_info* const    info,
    const char* const   origin)
{
    assert(isValid(info));
    (void)strncpy(info->origin, origin, HOSTNAMESIZE);

    info->origin[HOSTNAMESIZE] = 0;
}


/*
 * Copies one prod_info into another.
 *
 * Arguments:
 *      dest    The destination prod_info.  It should have been created by
 *              pi_new() or pi_clone() or be a member of an InfoBuf initialized
 *              by ib_init().
 *      src     The source prod_info.  The caller may free upon return.
 * Returns:
 *      0       Success.
 *      EINVAL  Either "dest" or "src" is NULL.
 */
int
pi_copy(
    prod_info* const            dest,
    const prod_info* const      src)
{
    int                         error;

    if (NULL == dest || NULL == src) {
        error = errno = EINVAL;
    }
    else {
        assert(isValid(dest));

        dest->arrival = src->arrival;
        dest->feedtype = src->feedtype;
        dest->seqno = src->seqno;
        dest->sz = src->sz;

        (void)memcpy(dest->signature, src->signature, sizeof(signaturet));
        pi_setOrigin(dest, src->origin);
        (void)strncpy(dest->ident, src->ident, KEYSIZE + 1);

        dest->ident[KEYSIZE] = 0;

        error = 0;
    }

    return error;
}


/*
 * Clones a prod_info.  On success, the caller is responsible for invoking
 * pi_free() on the returned prod_info.  This function is equivalent to
 * the sequential use of pi_new() and pi_copy().
 *
 * Arguments:
 *      info    The prod_info to be cloned.
 * Returns:
 *      NULL    Failure.  errno is set:
 *                      ENOMEM  Out of memory.
 *                      EINVAL  "info" is NULL.
 *      else    A clone of "info".  The caller is responsible for invoking
 *              pi_free() on the returned prod_info.
 */
prod_info*
pi_clone(
    const prod_info* const      info)
{
    prod_info*                  clone = pi_new();

    if (NULL != clone) {
        if (0 != pi_copy(clone, info)) {
            pi_free(clone);

            clone = NULL;
        }
    }

    return clone;
}


/*
 * Indicates whether or not two product-information structures are equal.
 *
 * Arguments:
 *      info1   Pointer to one product-information structure.  May be NULL or
 *              "info2".
 *      info2   Pointer to the other product-information structure.  May be
 *              NULL or "info1".
 * Returns:
 *      0       info1 doesn't equal info2 or *info1 doesn't equal *info2.
 *      1       info1 equals info2 or *info1 equals *info2.
 */
int
pi_equals(
    const prod_info* const      info1,
    const prod_info* const      info2)
{
    return
        info1 == info2
            ? 1
            : info1 == NULL || info2 == NULL
                ?   0
                :   info1->feedtype == info2->feedtype &&
                    info1->seqno == info2->seqno &&
                    info1->sz == info2->sz &&
                    tvEqual(info1->arrival, info2->arrival) &&
                    memcmp(info1->signature, info2->signature,
                        sizeof(signaturet)) == 0 &&
                    strcmp(info1->origin, info2->origin) == 0 &&
                    strcmp(info1->ident, info2->ident) == 0;
}


/*
 * Frees a prod_info allocated by pi_new() or pi_clone().
 *
 * Arguments:
 *      info    The prod_info to be freed.  May be NULL; otherwise, must have
 *              been allocated by pi_new() or pi_clone().
 */
void
pi_free(
    prod_info* const    info)
{
    if (NULL != info) {
        assert(isValid(info));
        free(info);
    }
}


/*
 * Prints a prod_info to an output stream.
 *
 * Arguments:
 *      info    Pointer to the prod_info to be written to "file".
 *      file    Pointer to the output stream.
 * Returns:
 *      -1      Failure.  log_errno() called.
 *      else    Number of bytes written.
 */
pi_print(
    const prod_info* const      info,
    FILE*                       file)
{
    int nbytes = fprintf(file, "{%s,%s,%u,{%d,%s},%u,{%d,%s},%s}",
        tsFormat(&info->arrival),
        s_feedtypet(info->feedtype),
        info->seqno,
        strlen(info->origin), info->origin,
        info->sz,
        strlen(info->ident), info->ident,
        s_signaturet(NULL, 0, info->signature));

    if (nbytes < 0)
        log_errno();

    return nbytes;
}


/*
 * Scans an input stream for a sequence of bytes.
 *
 * Arguments:
 *      file    Pointer to the input stream.
 *      buf     Pointer to the byte buffer.
 *      len     Number of bytes to scan.
 * Returns:
 *      -1      Failure.  log_*() called.  Contents of "buf" are undefined.
 *      else    Number of bytes scanned ("len").
 */
static int
scanBytes(
    FILE*       file,
    char* const buf,
    size_t      len)
{
    int         nbytes = -1;            /* error */
    int         i;

    errno = 0;

    for (i = 0; i < len; i++) {
        int     c = fgetc(file);

        if (c == EOF)
            break;

        buf[i] = (char)c;
    }

    if (feof(file)) {
        log_start("scanBytes(): Premature EOF");
    }
    else if (ferror(file)) {
        log_errno();
    }
    else {
        nbytes = len;
    }

    return nbytes;
}


/*
 * Scans an input stream for a string.
 *
 * Arguments:
 *      file    Pointer to the input stream.
 *      buf     Pointer to the string buffer.
 *      max     Maximum length of the string in bytes excluding the terminating
 *              NUL, which is automatically added.
 * Returns:
 *      -1      Failure.  log_*() called.  Contents of "buf" are undefined.
 *      else    Number of bytes scanned.
 */
static int
scanString(
    FILE*       file,
    char* const buf,
    size_t      max)
{
    int         nbytes = -1;            /* error */
    off_t       start = ftello(file);

    if (start == (off_t)-1) {
        log_errno();
    }
    else {
        int     len;

        errno = 0;

        if (fscanf(file, "{%d,", &len) != 1) {
            log_errno();
            log_add("scanString(): Couldn't scan string-length");
        }
        else if (len > max) {
            log_start("scanString(): String is too long: %d > %lu",
                len, (unsigned long)max);
        }
        else if (scanBytes(file, buf, len) < 0) {
            log_add("scanString(): Couldn't scan %d characters", len);
        }
        else {
            buf[len] = 0;

            if (fgetc(file) != '}') {
                log_start("scanString(): String isn't terminated");
            }
            else {
                nbytes = (int)(ftello(file) - start);
            }
        }                               /* string not too long */
    }                                   /* got initial file position */

    return nbytes;
}


/*
 * Scans a prod_info from an input stream.
 *
 * Arguments:
 *      info    Pointer to prod_info created by pi_new() or pi_clone().
 *      file    Pointer to the input stream.
 * Returns:
 *      -1      Failure.  log_*() called.
 *      else    Number of bytes scanned.
 */
pi_scan(
    prod_info* const    info,
    FILE*               file)
{
    int         nbytes = -1;            /* failure */
    off_t       start = ftello(file);

    if (start == (off_t)-1) {
        log_errno();
    }
    else {
        char    buf[512];

        (void)memset(buf, 0, sizeof(buf));

        errno = 0;

        if (fscanf(file, "{%80[^,]", buf) != 1 ||
                tsParse(buf, &info->arrival) < 0) {
            log_errno();
            log_add("pi_scan(): Couldn't scan product creation-time");
        }
        else if (fscanf(file, ",%511[^,]", buf) != 1 ||
                strfeedtypet(buf, &info->feedtype) != FEEDTYPE_OK) {
            log_errno();
            log_start("pi_scan(): Couldn't scan product feedtype");
        }
        else if (fscanf(file, ",%u", &info->seqno) != 1) {
            log_errno();
            log_add("pi_scan(): Couldn't scan product sequence-number");
        }
        else if (fgetc(file) != ',' ||
                scanString(file, info->origin, HOSTNAMESIZE) < 0) {
            log_add("pi_scan(): Couldn't scan product origin");
        }
        else if (fscanf(file, ",%u", &info->sz) != 1) {
            log_errno();
            log_add("pi_scan(): Couldn't scan product size");
        }
        else {
            if (fgetc(file) != ',' ||
                    scanString(file, info->ident, KEYSIZE) < 0) {
                log_add("pi_scan(): Couldn't scan product identifier");
            }
            else {
                log_clear();

                if (fgetc(file) != ',' ||
                        scanBytes(file, buf, 2*sizeof(signaturet)) < 0 ||
                        sigParse(buf, &info->signature) < 0) {
                    log_add("pi_scan(): Couldn't scan product signature");
                }
                else {
                    if (fgetc(file) != '}') {
                        log_start("pi_scan(): Information isn't terminated");
                    }
                    else {
                        nbytes = (int)(ftello(file) - start);
                    }
                }
            }
        }
    }

    return nbytes;
}
