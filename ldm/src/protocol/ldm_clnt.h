/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldm_clnt.h,v 5.2.2.3.6.4 2007/02/22 19:43:16 steve Exp $ */

#ifndef LDM_CLNT_H
#define LDM_CLNT_H

#include <signal.h>
#include <rpc/rpc.h>

#include "error.h"

typedef enum {
    LDM_CLNT_UNKNOWN_HOST = 1,
    LDM_CLNT_TIMED_OUT,
    LDM_CLNT_BAD_VERSION,
    LDM_CLNT_NO_CONNECT,
    LDM_CLNT_SYSTEM_ERROR,
    LDM_CLNT_DONE
} ldm_clnt_error_t;

ErrorObj*
ldm_clnt_addr(
    const char* const		name,
    struct sockaddr_in*		addr);

ErrorObj*
ldm_clnttcp_create_vers(
    const char*                  upName,
    const unsigned		 port,
    unsigned                     version,
    CLIENT**                     client,
    int*                         socket,
    struct sockaddr_in*          upAddr);

#endif
