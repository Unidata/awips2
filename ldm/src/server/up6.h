/* $Id: up6.h,v 1.2.10.1.4.5 2008/04/15 16:34:14 steve Exp $ */

#ifndef UP6_H
#define UP6_H

#include <signal.h>  /* sig_atomic_t */

#include "ldm.h"     /* prod_class */
#include "UpFilter.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    UP6_SUCCESS = 0,
    UP6_CLIENT_FAILURE,
    UP6_VERSION_MISMATCH,
    UP6_TIME_OUT,
    UP6_INTERRUPT,
    UP6_UNAVAILABLE,
    UP6_SYSTEM_ERROR,
    UP6_CLOSED,
    UP6_PQ,
    UP6_NO_CLASS
} up6_error_t;

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
    UpFilter* const			upFilter);

int
up6_new_notifier(
    const int                           socket, 
    const char* const                   downName,
    const struct sockaddr_in* const     downAddr,
    const prod_class_t* const             prodClass,
    const signaturet* const             signature,
    const char*                         pqPath, 
    const unsigned                      interval,
    UpFilter* const			upFilter);


/*
 * Closes the socket.
 */
void
up6_close();

#ifdef __cplusplus
}
#endif

#endif
