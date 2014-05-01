/*
 *   Copyright 2002, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: error.c,v 1.10.2.3.6.6 2006/12/17 22:06:26 steve Exp $ */

#include <ldmconfig.h>

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>   /* NULL */
#include <stdio.h>    /* vsnprintf(), snprintf() */
#include <stdlib.h>   /* malloc(), free(), abort() */
#include <string.h>
#include <strings.h>  /* strdup() */

#include "ulog.h"

#include "error.h"


struct error {
    char        msg[512];
    ErrorObj*    cause;
    const char* file;
    size_t      msglen;
    int         code;
    unsigned    line;
};


/******************************************************************************
 * Public API:
 ******************************************************************************/


ErrorObj* err_new(
    const int           code,
    ErrorObj    *const   cause,
    const char *const   file,
    const unsigned      line, 
    const char *const   fmt,
    ...)
{
    ErrorObj *err;

    assert(file != NULL);

    err = (ErrorObj*)malloc(sizeof(ErrorObj));

    if (NULL == err) {
        serror("err_new(): malloc(%lu) failure",
            (unsigned long)sizeof(ErrorObj));
    }
    else {
        char   *cp = err->msg;
        int     len = 0;

        err->line = line;
        err->file = file;

        if (NULL == fmt) {
            err->msg[0] = 0;
            err->msglen = 0;
            err->code = code;
            err->cause = cause;
        }
        else {
            va_list     args;
            int         nbytes;

            va_start(args, fmt);

            nbytes = vsnprintf(err->msg, sizeof(err->msg)-1, fmt, args);
            if (nbytes < 0) {
                nbytes = snprintf(err->msg, sizeof(err->msg)-1, 
                    "err_new(): vsnprintf() failure: \"%s\"", fmt);
            }

            va_end(args);

            err->msg[sizeof(err->msg)-1] = 0;
            err->msglen = (size_t)nbytes;

            assert(err->msglen < sizeof(err->msg));

            err->code = code;
            err->cause = cause;
        }
    }                                   /* "err" allocated */

    return err;
}


int
err_code(const ErrorObj* err)
{
    assert(err != NULL);

    return err->code;
}


ErrorObj*
err_cause(const ErrorObj *err)
{
    assert(err != NULL);

    return err->cause;
}


const char*
err_message(const ErrorObj *err)
{
    return err->msg;
}


/*
 * This function is not re-entrant because it contains static variables that are
 * potentially modified on every invocation.
 */
void err_log(
    const ErrorObj       *const err,
    const enum err_level level)
{
    static const unsigned logMasks[] = {
        LOG_MASK(LOG_ERR),
        LOG_MASK(LOG_WARNING),
        LOG_MASK(LOG_NOTICE), 
        LOG_MASK(LOG_INFO),
        LOG_MASK(LOG_DEBUG)
    };

    if (getulogmask() & logMasks[level]) {
        const ErrorObj*          e;
        const char* const       stdErrSep = "; ";
        const char* const       stdLocSep = ": ";
        const char*             errSep;
        const char*             locSep;

        static char             initialBuf[1024];
        static char*            buf = initialBuf;
        static size_t           buflen = sizeof(initialBuf);
        static const int        pris[] =
            {LOG_ERR, LOG_WARNING, LOG_NOTICE, LOG_INFO, LOG_DEBUG};

        assert(err != NULL);

        {
            size_t          totlen = 0;

            errSep = "";

            for (e = err; e != NULL; e = e->cause) {
                totlen += strlen(errSep);

                errSep = "";

                if (ERR_DEBUG != level) {
                    locSep = "";
                }
                else {
                    totlen += sprintf(buf, "%s@%u", e->file, e->line);
                    locSep = stdLocSep;
                    errSep = stdErrSep;
                }

                if (0 < e->msglen) {
                    totlen += strlen(locSep) + e->msglen;
                    errSep = stdErrSep;
                }
            }

            totlen++;                   /* + '\0' */

            if (NULL == buf) {
                buf = (char*)malloc(totlen);
                buflen = totlen;
            }
            else if (totlen > buflen) {
                if (buf != initialBuf)
                    free(buf);

                buf = (char*)malloc(totlen);
                buflen = totlen;
            }

            assert(NULL != buf);
        }

        {
            char           *cp = buf;

            errSep = "";

            for (e = err; e != NULL; e = e->cause) {
                (void)strcpy(cp, errSep);
                cp += strlen(errSep);

                errSep = "";

                if (ERR_DEBUG != level) {
                    locSep = "";
                }
                else {
                    cp += sprintf(cp, "%s@%u", e->file, e->line);
                    locSep = stdLocSep;
                    errSep = stdErrSep;
                }

                if (0 < e->msglen) {
                    (void)strcpy(cp, locSep);
                    cp += strlen(locSep);

                    (void)memcpy(cp, e->msg, e->msglen);
                    cp += e->msglen;

                    errSep = stdErrSep;
                }
            }

            *cp = 0;
        }

        /*
         * NB: The message is not printed using "ulog(pris[level], buf)"
         * because "buf" might have formatting characters in it (e.g., "%")
         * from, for example, a call to "s_prod_info()" with a dangerous
         * product-identifier.
         */
        ulog(pris[level], "%s", buf);
    }
}


void err_free(ErrorObj *const err)
{
    if (err != NULL) {
        if (err->cause != NULL)
            err_free(err->cause);

        free((void*)err);
    }
}


/*
 * Logs the error and then frees it.  This function is equivalent to
 *      err_log(err);
 *      err_free(err);
 *
 * Arguments:
 *      err     The error.
 *      level   The logging level.
 */
void err_log_and_free(
    ErrorObj             *const err,
    const enum err_level level)
{
    err_log(err, level);
    err_free(err);
}
