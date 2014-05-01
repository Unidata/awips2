/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: abbr.c,v 1.4.18.2 2006/02/28 21:03:26 steve Exp $ */

#include "ldmconfig.h"

#include <ctype.h>
#include <string.h>

#include "abbr.h"
#include "ldm.h"
#include "ulog.h"


/*
 * Sets the ulog(3) identifer based on a remote-host identifier and an optional
 * suffix.
 * Arguments:
 *      remote  Pointer to identifier of remote host.  May be a hostname or a
 *              dotted-quad IP address.
 *      suffix  Pointer to suffix to be added to ulog(3) identifier (e.g.,
 *              "(feed)") or NULL.
 */
void
set_abbr_ident(
    const char* const   remote,
    const char* const   suffix)
{
    char        newident[HOSTNAMESIZE+1];
    size_t      remoteLen = strlen(remote);

    if (remoteLen > HOSTNAMESIZE) {
        (void)strncpy(newident, remote, HOSTNAMESIZE);
    }
    else {
        (void)strcpy(newident, remote);

        if (suffix != NULL)
            (void)strncpy(newident+remoteLen, suffix, HOSTNAMESIZE-remoteLen);
    }

    newident[HOSTNAMESIZE] = 0;

    setulogident(newident);
}
