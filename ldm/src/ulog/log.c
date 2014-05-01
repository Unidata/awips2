/*
 *   Copyright 2006, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 *
 * Provides for the accumulation of log-messages and the printing of all,
 * accumlated log-messages at a single priority.
 *
 * This module uses the ulog(3) module.
 *
 * This module is not thread-safe.
 */
/* $Id: log.c,v 1.1.2.4 2006/12/17 22:07:19 steve Exp $ */

#include <ldmconfig.h>

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>   /* NULL */
#include <stdio.h>    /* vsnprintf(), snprintf() */
#include <stdlib.h>   /* malloc(), free(), abort() */
#include <string.h>
#include <strings.h>  /* strdup() */

#include "ulog.h"

#include "log.h"


/*
 * A log-message:
 */
typedef struct message {
    char                   string[512];
    struct message*        nextMessage;
} Message;

/*
 * The first (i.e., most fundamental) log-message.
 */
static Message      firstMessage;

/*
 * The tail of the message-list.  Points to the last added message.  Will be
 * NULL if and only if the message-list is empty.
 */
static Message* tail;


/******************************************************************************
 * Public API:
 ******************************************************************************/


/*
 * Deletes the accumulated log-messages.  If log_log() is invoked after this
 * function, then no messages will be logged.
 */
void log_clear()
{
    tail = NULL;
}


/*
 * Adds a log-message.  If the format is NULL, then no message will
 * be added.
 * Arguments:
 *      fmt     The format for the message or NULL.
 *      args    The arguments referenced by the format.
 */
void log_vadd(
    const char *const   fmt,
    va_list             args)
{
    if (fmt != NULL) {
        Message*        msg;

        if (tail == NULL) {
            msg = &firstMessage;
        }
        else {
            msg = tail->nextMessage;

            if (msg == NULL) {
                msg = (Message*)malloc(sizeof(Message));

                if (msg == NULL) {
                    serror("log_vadd(): malloc(%lu) failure",
                        (unsigned long)sizeof(Message));
                }
                else {
                    msg->string[0] = 0;
                    msg->nextMessage = NULL;
                }
            }
        }                               /* tail != NULL */

        if (msg != NULL) {
            char*       cp = msg->string;
            int         len = 0;
            int         nbytes;

            nbytes = vsnprintf(msg->string, sizeof(msg->string)-1, fmt, args);
            if (nbytes < 0) {
                nbytes = snprintf(msg->string, sizeof(msg->string)-1, 
                    "log_vadd(): vsnprintf() failure: \"%s\"", fmt);
            }

            msg->string[sizeof(msg->string)-1] = 0;

            if (tail != NULL)
                tail->nextMessage = msg;

            tail = msg;
        }                               /* msg != NULL */
    }                                   /* format string != NULL */
}


/*
 * Resets this module and adds the first log-message.  This function
 * is equivalent to calling log_clear() followed by log_add().
 * Arguments:
 *      fmt     The format for the message or NULL.
 *      ...     The arguments referenced in the format.
 */
void log_start(
    const char* const   fmt,
    ...)
{
    va_list     args;

    log_clear();
    va_start(args, fmt);
    log_vadd(fmt, args);
    va_end(args);
}


/*
 * Adds a log-message.
 * Arguments:
 *      fmt     The format for the message or NULL.
 *      ...     The arguments referenced in the format.
 */
void log_add(
    const char *const   fmt,
    ...)
{
    va_list     args;

    va_start(args, fmt);
    log_vadd(fmt, args);
    va_end(args);
}


/*
 * Resets this module and adds an "errno" message.  Calling this function is
 * equivalent to calling log_start(strerror(errno)).
 */
void log_errno(void)
{
    log_start(strerror(errno));
}


/*
 * Logs the currently-accumulated log-messages and resets this module.
 *
 * This function is thread-safe.
 * Arguments:
 *      level   The level at which to log the messages.  One of LOG_ERR,
 *              LOG_WARNING, LOG_NOTICE, LOG_INFO, or LOG_DEBUG; otherwise,
 *              the behavior is undefined.
 */
void log_log(
    const int   level)
{
    if (tail != NULL) {
        static const unsigned       allPrioritiesMask = 
            LOG_MASK(LOG_ERR) |
            LOG_MASK(LOG_WARNING) |
            LOG_MASK(LOG_NOTICE) | 
            LOG_MASK(LOG_INFO) |
            LOG_MASK(LOG_DEBUG);
        const int                   priorityMask = LOG_MASK(level);

        if ((priorityMask & allPrioritiesMask) == 0) {
            uerror("log_log(): Invalid logging-level (%d)", level);
        }
        else if (getulogmask() & priorityMask) {
            const Message*     msg;

            for (msg = &firstMessage; ; msg = msg->nextMessage) {
                /*
                 * NB: The message is not printed using "ulog(level,
                 * msg->string)" because "msg->string" might have formatting
                 * characters in it (e.g., "%") from, for example, a call to
                 * "s_prod_info()" with a dangerous product-identifier.
                 */
                ulog(level, "%s", msg->string);

                if (msg == tail)
                    break;
            }
        }

        log_clear();
    }
}
