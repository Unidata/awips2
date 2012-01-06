/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: action.c,v 1.78.16.4.2.3 2005/09/21 18:37:12 steve Exp $ */

#include "ldmconfig.h"

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <signal.h>
#include <fcntl.h>

#include "ldm.h"
#include "ldmalloc.h"
#include "action.h"
#include "error.h"
#include "filel.h"
#include "globals.h"
#include "pq.h"
#include "ulog.h"


/*ARGSUSED*/
static int
prod_noop(
        const product *prod,
        int argc, char **argv,
        const void *xprod, size_t xlen)
{
        return 0;
}


/*
 * Execute a program.
 *
 * Arguments:
 *      prod    Pointer to the data-product that caused this action.
 *      argc    Number of arguments in the command-line.
 *      argv    Pointer to pointers to command-line arguments.
 *      xprod   Pointer to XDR-ed data-product.
 *      xlen    Size of "xprod" in bytes.
 * Returns:
 *      -1      Failure.  An error--message is logged.
 *      else    PID of the child process.
 */
/*ARGSUSED*/
static int
exec_prodput(
     const product*     prod,
     int                argc,
     char**             argv,
     const void*        xprod,
     size_t             xlen)
{
    pid_t       pid;
    int         waitOnChild = 0;        /* default is not to wait */

    if (strcmp(argv[0], "-wait") == 0)
    {
        waitOnChild = 1;                /* => wait for child */
        argc--; argv++;
    }

    pid = fork();
    if (-1 == pid)
    {
        err_log_and_free(
            ERR_NEW1(0, NULL,
                "Couldn't fork() child-process: %s", strerror(errno)),
            ERR_FAILURE);
    }
    else
    {
        if (0 == pid)
        {
            /*
             * Child process.
             */

            /*
             * Detach the child process from the parents process group??
            (void) setpgid(0,0);
             */

            (void)signal(SIGTERM, SIG_DFL);
            (void)pq_close(pq);

            /*
             * It is assumed that the standard input, output, and error streams
             * are correctly established and should not be modified.
             */

            /*
             * Don't let the child process get any inappropriate privileges.
             */
            endpriv();

            (void) execvp(argv[0], &argv[0]);
            err_log_and_free(
                ERR_NEW2(0, NULL, "Couldn't exec(%s): %s",
                    argv[0], strerror(errno)),
                ERR_FAILURE);
            exit(EXIT_FAILURE);
        }
        else
        {
            /*
             * Parent process.
             */

            if (!waitOnChild)
            {
                udebug("    exec %s[%d]", argv[0], pid);
            }
            else
            {
                udebug("    exec -wait %s[%d]", argv[0], pid);
                (void)reap(pid, 0);
            }
        }
    }

    return -1 == pid ? -1 : 1;
}


int
atoaction(
        const char *str,
        actiont *resultp)
{
#define MAXACTIONLEN 12
        char buf[MAXACTIONLEN];
        const char *in;
        char *cp;
        actiont *ap;

        static actiont assoc[] = {
                {"noop",
                        0,
                        prod_noop},
                {"file",
                        0,
                        unio_prodput},
                {"stdiofile",
                        0,
                        stdio_prodput},
                {"dbfile",
                        0,
#ifndef NO_DB
                        ldmdb_prodput},
#else
                        prod_noop},
#endif
                {"pipe",
                        0,
                        pipe_prodput},
                {"spipe",
                        0,
                        spipe_prodput},
                {"xpipe",
                        0,
                        xpipe_prodput},
                {"exec",
                        0,
                        exec_prodput},

        };

        if(str == NULL || *str == 0)
        {
                udebug("atoaction: Invalid string argument");
                return -1;
        }

        for(in = str, cp = buf; *in != 0 && cp < &buf[MAXACTIONLEN] ; cp++, in++)
        {
                *cp = (char)(isupper(*in) ? tolower((*in)) : (*in));
        }
        *cp = 0;

        for(ap = &assoc[0]; ap < &assoc[sizeof(assoc)/sizeof(assoc[0])] ; ap ++)
        {
                if(strcmp(ap->name, buf) == 0)
                {
                        (void) memcpy((char *)resultp, (char *)ap,
                                      sizeof(actiont));
                        return 0;
                }
        }
        /* uerror("Unknown action \"%s\"", str); */
        return -1; /* noop */
}


char *
s_actiont(actiont *act)
{
        if(act == NULL || act->name == NULL)
                return "";
        /* else */
        return act->name;
}
