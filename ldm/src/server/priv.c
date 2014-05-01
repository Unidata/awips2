/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: priv.c,v 1.10.18.3 2005/09/23 21:52:10 steve Exp $ */

#include "ldmconfig.h"

#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#if __linux || __linux__ || linux
#include <sys/prctl.h>
#endif

#include "error.h"
#include "priv.h"

/*
 * N.B.: You won't be able to get root priv
 * back after the call to unpriv()
 * unless the OS supports the "saved set-user-ID"
 * feature and has seteuid().
 */
#if NO_SETEUID
#define seteuid(uid) setuid(uid)
#else
/* Is there a standard place where this is declared? missing on IRIX */
extern int seteuid(uid_t euid);
#endif

#define UID_NONE ((uid_t)-1)
static uid_t sav_uid = UID_NONE;


/*
 * Ensures that the process may dump core.
 */
void
ensureDumpable()
{
#if __linux || __linux__ || linux
    /*
     * Workaround non-standard behavior of Linux regarding the creation
     * of a core file for a setuid program.
     */
    if (-1 == prctl(PR_SET_DUMPABLE, 1, 0, 0, 0))
    {
        err_log_and_free(
            ERR_NEW1(0, NULL,
                "Couldn't give process the ability to create a core file: %s",
                strerror(errno)),
            ERR_WARNING);
    }
#endif
}


/*
 * If either euid or uid is unprivileged, stash it,
 * for later use by unpriv().
 * Set euid to root if possible and necessary.
 */
void
rootpriv(void)
{
        const uid_t euid = geteuid();
        
        if(sav_uid == UID_NONE)
        {
                /* first time */
                const uid_t uid = getuid();

                /* if either euid or uid is unprivileged, stash it */
                if(euid > 0)
                        sav_uid = euid;
                else if(uid > 0)
                        sav_uid = uid;
                else
                        sav_uid = 0;
        }

        if(euid > 0)
        {
                (void) seteuid(0);
                ensureDumpable();
        }
}


/*
 * Set to non-root privilege if possible.
 * Assumes rootpriv() was called prior.
 */
void
unpriv(void)
{
#ifndef NO_SETEUID
        if(sav_uid != UID_NONE && sav_uid != 0)
        {
                (void) seteuid(sav_uid);
                ensureDumpable();
        }
#endif
}


/*
 * Permanently set to non-root privilege if possible.
 * Do it in such a way that it is safe to fork.
 */
void
endpriv(void)
{
    uid_t       uid = geteuid();

    if (uid <= 0)
        uid = getuid();

    if (uid > 0)
    {
        (void)setuid(uid);
        ensureDumpable();               /* can't hurt */
    }
    else
    {
        err_log_and_free(
            ERR_NEW(0, NULL,
                "Couldn't permanently remove root privilege of process"),
            ERR_WARNING);
    }
}


#ifdef TEST_PRIV

#include <stdio.h>

main()
{
        (void)printf("BEGIN euid %d, uid %d (sav %d)\n",
                         geteuid(), getuid(), sav_uid);
        rootpriv();
        (void)printf("R1    euid %d, uid %d (sav %d)\n",
                         geteuid(), getuid(), sav_uid);
        unpriv();
        (void)printf("U1    euid %d, uid %d (sav %d)\n",
                         geteuid(), getuid(), sav_uid);
        rootpriv();
        (void)printf("R2    euid %d, uid %d (sav %d)\n",
                         geteuid(), getuid(), sav_uid);
        unpriv();
        (void)printf("U2    euid %d, uid %d (sav %d)\n",
                         geteuid(), getuid(), sav_uid);
        exit(0);
}
#endif
