/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: remote.c,v 1.15.18.11 2009/07/24 16:06:32 steve Exp $ */

/*LINTLIBRARY*/

#include "ldmconfig.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <string.h>
#include <ctype.h>

#include "error.h"
#include "ldm.h"        /* needed by following */
#include "ulog.h"
#include "peer_info.h"
#include "globals.h"
#include "inetutil.h"
#include "acl.h"
#include "timestamp.h"

/* global */
peer_info remote; 


static const char *
s_optname(int optname)
{
        switch (optname) {
        case SO_SNDBUF : return "SO_SNDBUF";
        case SO_RCVBUF : return "SO_RCVBUF";
        }
        return "";
}


static u_int
so_buf(int sock, int optname)
{
        int optval = 0;
        socklen_t optlen = sizeof(optval);

        if(getsockopt(sock, SOL_SOCKET, optname,
                        (void*) &optval, &optlen) < 0)
        {
                serror("getsockopt %s", s_optname(optname));
                return 0;
        }
        /* else */
        if(optval < 4096) /* arbitrary */
        {
                /*
                 * SunOS 5. All others have a default of 4096 or greater
                 * Why does SunOS 5 default to zero,
                 * and what does it mean?
                 *
                 * Would we be better off just setting to 
                 * MAX_RPC_BUF_NEEDED for all systems?
                 */

                udebug("%s %d, setting to %d",
                         s_optname(optname), optval, MAX_RPC_BUF_NEEDED);
                optval = MAX_RPC_BUF_NEEDED;
                if(setsockopt(sock, SOL_SOCKET, optname,
                                (char *) &optval, optlen) < 0)
                {
                        serror("setsockopt %s %d", s_optname(optname), optval);
                        return 0;
                }
#if 0 /* DEBUG verify */
                optval = 0;
                if(getsockopt(sock, SOL_SOCKET, SO_SNDBUF,
                                (char *) &optval, &optlen) < 0)
                {
                        serror("getsockopt 2 %s", s_optname(optname));
                }
#endif
        }
        udebug("%s %d", s_optname(optname), optval);
        return optval;
}


void
free_remote_clss(void)
{
        if(remote.clssp == NULL)
                return;
        free_prod_class(remote.clssp);
        remote.clssp = NULL;
}


/*
 * Ensures that the "name" member of the global structure "remote" is set.  Does
 * nothing if the name is already set.  Calls the potentially extremely
 * expensive function hostbyaddr() to set the name.
 *
 * Arguments:
 *      paddr   Pointer to the IP address of the remote host.
 */
void
ensureRemoteName(
    const struct sockaddr_in* const     paddr)
{
    /*
     * Because this function might be called often and the hostbyaddr()
     * function can be extremely expensive (it might time-out), something is
     * done only if necessary.
     */
    if (remote.name[0] == 0) {
        (void)strncpy(remote.name, hostbyaddr(paddr), HOSTNAMELEN-1);
        remote.printname = remote.name;
    }
}


/*
 * Sets most of the members of the global structure "remote".  Does not set the
 * "name" member.  Does not set the structure if its IP address is the same as
 * the argument's.
 *
 * Arguments:
 *      paddr   Pointer to the IP address of the remote host.
 *      sock    The socket that's connected to the remote host.
 */
void
setremote(
    const struct sockaddr_in* const     paddr,
    const int                           sock)
{
    if (paddr->sin_addr.s_addr != 0 && 
        paddr->sin_addr.s_addr == remote.addr.s_addr) {
        /*
         * Same as the last guy.
         *
         * N.B. this can be confusing.
         * For example, if he fixed his nameserver
         * between now and the last time.
         */
    }
    else {
        free_remote_clss();

        /*
         * Ensure that ensureRemoteName() can tell that the name member is
         * unset.
         */
        (void)memset(&remote, 0, sizeof(remote));
        remote.addr.s_addr = paddr->sin_addr.s_addr;
        (void)strcpy(remote.astr, inet_ntoa(remote.addr));
        remote.printname = remote.astr;

        remote.sendsz = (u_int) so_buf(sock, SO_SNDBUF);

        if(remote.sendsz > MAX_RPC_BUF_NEEDED || remote.sendsz == 0)
            remote.sendsz = MAX_RPC_BUF_NEEDED;

        remote.recvsz = (u_int) so_buf(sock, SO_RCVBUF);

        if(remote.recvsz > MAX_RPC_BUF_NEEDED || remote.recvsz == 0)
            remote.recvsz = MAX_RPC_BUF_NEEDED;

        uinfo("RPC buffer sizes for %s: send=%u; recv=%u",
            remote_name(), remote.sendsz, remote.recvsz);
    }                                   /* remote host differs from previous */

    return;
}

void
svc_setremote(struct svc_req *rqstp)
{
        struct sockaddr_in *paddr = 
            (struct sockaddr_in *)svc_getcaller(rqstp->rq_xprt);
        setremote(paddr, rqstp->rq_xprt->xp_sock);
        ensureRemoteName(paddr);
}


/*
 * Sets the remote identifier structure to an identifier.
 * Arguments:
 *      id      The identifier as a hostname or IP address in dotted-quad
 *              format.
 */
void
str_setremote(const char *id)
{
    if(remote.printname != 0 && *remote.printname != 0
                     && strcmp(remote.printname, id) == 0) {
        /* same as the last guy */
        return;
    }

    free_remote_clss();
    (void)memset(&remote, 0, sizeof(remote));

    if (inet_addr(id) == (in_addr_t)(-1))
    {
        /*
         * The identifier isn't a dotted-quad IP address.
         */
        (void)strncpy(remote.name, id, HOSTNAMELEN-1);
        remote.name[HOSTNAMELEN-1] = 0;
        remote.printname = remote.name;
    }
    else
    {
        (void)strncpy(remote.astr, id, DOTTEDQUADLEN-1);
        remote.astr[DOTTEDQUADLEN-1] = 0;
        remote.name[0] = 0;
        remote.printname =  remote.astr;
    }
}


const char *
remote_name(void)
{
        return remote.printname;
}

int
update_remote_clss(prod_class_t *want)
{
        int status = ENOERR;

        if(!clsspsa_eq(remote.clssp, want))
        {
                /* request doesn't match cache */

                free_remote_clss();

                status = forn_acl_ck(&remote, want);
        }
        else
        {
                /* Update the time range */
                remote.clssp->from = want->from;
                remote.clssp->to = want->to;
        }
        return status;
}
