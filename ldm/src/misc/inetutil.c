/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: inetutil.c,v 1.41.2.1.6.10 2009/07/23 23:18:31 steve Exp $ */

/* 
 * Miscellaneous functions to make dealing with internet addresses easier.
 */

#include <ldmconfig.h>

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
/*
 * On FreeBSD 4.10-RELEASE-p2 the following order is necessary.
 */
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>
#include "error.h"
#include "ulog.h"
#include "inetutil.h"
#include "../protocol/timestamp.h"

/*
 * Host names are limited to 255 bytes by the The Single UNIX®
 * Specification, Version 2, for the function gethostname().  MAXHOSTNAMELEN
 * is not required by that specification to be defined.
 */
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

#ifndef h_errno /* AIX 4.3 */
extern int      h_errno;        /* error number for gethostby...() functions */
#endif

/*
 * Return a string indicating the problem with one of the gethostby...()
 * functions.
 */
static const char*
host_err_str(void)
{
    static char msgstr[200];

    switch (h_errno)
    {
    case 0:
        msgstr[0] = 0;
        break;
#ifdef HOST_NOT_FOUND
    case HOST_NOT_FOUND:
        (void) strcpy(msgstr, "no such host is known");
        break;
#endif
#ifdef TRY_AGAIN
    case TRY_AGAIN:
        (void) strcpy(msgstr,
            "local server did not receive authoritative response");
        break;
#endif
#ifdef NO_RECOVERY
    case NO_RECOVERY:
        (void) strcpy(msgstr, "nonrecoverable error");
        break;
#endif
#ifdef NO_ADDRESS
    case NO_ADDRESS:
        (void) strcpy(msgstr, "valid name has no IP address");
        break;
#endif
    default:
        (void)sprintf(msgstr, "h_errno = %d", h_errno);
    }

    return msgstr;
}


/*
 * convenience wrapper around gethostname(2)
 * !NO_INET_FQ_KLUDGE ==> try to make sure it is "fully qualified"
 */
char *
ghostname(void)
{

        static char hostname[MAXHOSTNAMELEN+1];

        if (hostname[0])
                return hostname;

        /*
         * Since the ldm programs require fully qualified
         * hostnames in an internet environment AND users
         * often don't have control over the system admin
         * conventions, we allow override of the 
         * hostname with an environment variable.
         * This is meant to be the fully qualified
         * hostname of the current host.
         */
        {
                char *cp;
                cp = getenv("LDMHOSTNAME");
                if(cp != NULL)
                {
                        (void)strncpy(hostname, cp, MAXHOSTNAMELEN);
                        return hostname;
                }
        }

        if(gethostname(hostname, MAXHOSTNAMELEN) < 0)
                return NULL;
#ifndef NO_INET_FQ_KLUDGE
        if(strchr(hostname, '.') == NULL)
        {
                /* gethostname return not fully qualified */
                struct hostent *hp;
                hp = gethostbyname(hostname);
                if(hp != NULL && hp->h_addrtype == AF_INET) 
                {
                        /* hopefully hp->h_name is fully qualified */
                        (void)strncpy(hostname, hp->h_name, MAXHOSTNAMELEN);
                }
        }
        /* 
         * On some systems, neither gethostname() nor
         * the hp->h_name is fully qualified.
         * If you can't shoot the Systems Administrator and fix it,
         * hardwire the trailing path here.
         * (Uncomment and replace ".unversity.edu" with your domain.)
         */
/* #define HARDWIRED_LOCAL_DOMAIN ".unversity.edu" */
#ifdef HARDWIRED_LOCAL_DOMAIN
        if(strchr(hostname, '.') == NULL)
        {
                strcat(hostname, HARDWIRED_LOCAL_DOMAIN);
        }
#endif /* HARDWIRED_LOCAL_DOMAIN */
#endif
        return hostname;
}


/*
 * Returns a string identifying the Internet host referred to by an Internet
 * address. If the hostname lookup fails, then the "dotted quad" form of the
 * address is returned.
 * Arguments:
 *      paddr   Pointer to the Internet address structure.
 * Returns:
 *      Pointer to static buffer containing the identifying string.
 */
const char*
hostbyaddr(
    const struct sockaddr_in* const     paddr)
{
    struct hostent*     hp;
    const char*         identifier;
    struct in_addr      addr;
    timestampt          start;
    timestampt          stop;
    double              elapsed;
    ErrorObj*           error;

    addr.s_addr = paddr->sin_addr.s_addr;

    (void)set_timestamp(&start);
    hp = gethostbyaddr((char *)&addr, sizeof (addr), AF_INET);
    (void)set_timestamp(&stop);

    elapsed = d_diff_timestamp(&stop, &start);

    if(hp == NULL) {
        identifier = inet_ntoa(paddr->sin_addr);
        error = ERR_NEW2(0, NULL,
            "Couldn't resolve \"%s\" to a hostname in %g seconds",
            identifier, elapsed);
    }
    else {
        identifier = hp->h_name;

        if (elapsed < RESOLVER_TIME_THRESHOLD && !ulogIsVerbose()) {
            error = NULL;
        }
        else {
            error = ERR_NEW3(0, NULL,
                "Resolving %s to %s took %g seconds",
                inet_ntoa(paddr->sin_addr), identifier, elapsed);
        }
    }

    if (error) {
        err_log_and_free(error, 
            elapsed >= RESOLVER_TIME_THRESHOLD ? ERR_WARNING : ERR_INFO);
    }

    return identifier;
}


/*
 * Gets the IP address corresponding to a host identifier.
 *
 * Arguments:
 *      id      The host identifier as either a name or an IP address in
 *              dotted-quad form (NNN.NNN.NNN.NNN).
 *      paddr   Pointer to structure to be set to the IP address of the given
 *              host.  Modified on and only on success.  The port is set to
 *              zero, the address family to AF_INET, and the rest is cleared.
 * Returns:
 *      0       Success.
 *      -1      Failure.  No IP address could be found for the given host
 *              identifier.
 */
int
addrbyhost(
    const char* const           id,
    struct sockaddr_in* const   paddr)
{
    int                 errCode = 0;    /* success */
    in_addr_t           ipAddr;
    
    ipAddr = inet_addr(id);

    if (ipAddr != (in_addr_t)-1) {
        /*
         * The identifier is a dotted-quad IP address.
         */
        paddr->sin_addr.s_addr = ipAddr;
    }
    else {
        timestampt      start;
        timestampt      stop;
        double          elapsed;
        ErrorObj*        error;
        struct hostent* hp;
        
        (void)set_timestamp(&start);
        hp = gethostbyname(id);
        (void)set_timestamp(&stop);

        elapsed = d_diff_timestamp(&stop, &start);

        if (hp == NULL) {
            error = ERR_NEW2(0, NULL,
                "Couldn't resolve \"%s\" to an Internet address in %g seconds",
                id, elapsed);
            errCode = -1;               /* failure */
        }
        else if (hp->h_addrtype != AF_INET) {
            err_log_and_free(
                ERR_NEW1(0, NULL, "\"%s\" isn't an Internet host identifier",
                    id),
                ERR_WARNING);
            error = NULL;
            errCode = -1;               /* failure */
        } else {
            (void) memcpy((char *)&paddr->sin_addr, hp->h_addr_list[0],
                (size_t)hp->h_length);

            if (elapsed < RESOLVER_TIME_THRESHOLD && !ulogIsVerbose()) {
                error = NULL;
            }
            else {
                error = ERR_NEW3(0, NULL, "Resolving %s to %s took %g seconds",
                    id, inet_ntoa(paddr->sin_addr), elapsed);
            }
        }

        if (error)
            err_log_and_free(error, 
                elapsed >= RESOLVER_TIME_THRESHOLD ? ERR_WARNING : ERR_INFO);
    }

    if (errCode == 0) {
        paddr->sin_family= AF_INET;
        paddr->sin_port= 0;

        (void) memset(paddr->sin_zero, 0, sizeof (paddr->sin_zero));
    }

    return errCode;
}


/*
 * Indicates if a host identifier has a given IP address.
 * Potentially lengthy operation.
 * Arguments:
 *      id              Name of the host or dotted-quad IP address.
 *      targetAddr      Target IP address.
 *      hasAddress      Pointer to result.  Set on and only on success. Set to 0
 *                      if and only if the given host has the given IP address.
 * Returns:
 *      NULL    Success.  See *hasAddress for the result.
 *      else    An error occurred.  *hasAddress is not set.  Error codes:
 *                      1       gethostbyname() failure
 *                      2       "id" isn't an Internet host identifier
 */
ErrorObj*
hostHasIpAddress(
    const char* const   id,
    const in_addr_t     targetAddr,
    int* const          hasAddress)
{
    ErrorObj*           error = NULL;   /* success */
    const in_addr_t     ipAddr = inet_addr(id);
    
    if (ipAddr != (in_addr_t)-1) {
        *hasAddress = targetAddr == ipAddr;
    }
    else {
        /*
         * The identifier is not a dotted-quad IP address.
         */
        timestampt              start;
        timestampt              stop;
        double                  elapsed;
        const struct hostent*   hp;

        (void)set_timestamp(&start);
        hp = gethostbyname(id);
        (void)set_timestamp(&stop);

        elapsed = d_diff_timestamp(&stop, &start);

        if (hp == NULL) {
            error = ERR_NEW2(1, 
                ERR_NEW(h_errno, NULL,
                    h_errno == HOST_NOT_FOUND   ? "host not found" :
                    h_errno == NO_DATA          ? "no data on host" :
                    h_errno == NO_RECOVERY      ? "unrecoverable server error" :
                    h_errno == TRY_AGAIN        ? "hostname lookup timeout" :
                                                  "unknown error"),
            "Couldn't resolve \"%s\" to an Internet address in %g seconds",
                id, elapsed);
        }
        else if (hp->h_addrtype != AF_INET) {
            error = ERR_NEW1(2, NULL,
                "\"%s\" isn't an Internet host identifier", id);
        } else {
            const struct in_addr* const*        in_addr_pp;

            for (in_addr_pp = (const struct in_addr* const*)hp->h_addr_list;
                     *in_addr_pp != NULL;
                     in_addr_pp++) {
                if ((*in_addr_pp)->s_addr == targetAddr)
                    break;
            }

            *hasAddress = *in_addr_pp != NULL;

            if (elapsed >= RESOLVER_TIME_THRESHOLD || ulogIsVerbose()) {
                err_log_and_free(
                    ERR_NEW2(0, NULL,
                        "Resolving %s to an IP address took %g seconds",
                        id, elapsed),
                    elapsed >= RESOLVER_TIME_THRESHOLD
                        ? ERR_WARNING : ERR_INFO);
            }
        }
    }

    return error;
}


char *
s_sockaddr_in(
        struct sockaddr_in *paddr
)
{
        static char buf[64];
        (void) sprintf(buf,
                "sin_port %5d, sin_addr %s",
                paddr->sin_port,
                inet_ntoa(paddr->sin_addr));
        return buf;
}


/*
 * Puts the address of the current host into *paddr
 * Returns 0 on success, -1 failure
 */
int
gethostaddr_in(
        struct sockaddr_in *paddr
)
{
        char hostname[MAXHOSTNAMELEN];
        struct hostent *hp;

        if(gethostname(hostname,MAXHOSTNAMELEN) == -1)
                return -1;

        return addrbyhost(hostname, paddr);
}


/*
 * Return the well known port for (servicename, proto)
 * or -1 on failure.
 */
int
getservport(
        const char *servicename,
        const char *proto
)
{
        struct servent *se;
        se = getservbyname(servicename, proto);
        if(se == NULL)
                return -1;
        /* else */
        return se->s_port;
}


/*
 * Attempt to connect to a unix domain socket.
 * Create & connect.
 * Returns (socket) descriptor or -1 on error.
 */
int
usopen(
        const char *name /* name of socket */
)
{
        int sock = -1;
        struct sockaddr addr;   /* AF_UNIX address */
        int logType = SOCK_DGRAM;


        while(sock == -1)
        {
                sock = socket(AF_UNIX, logType, 0);
                if(sock == -1)
                        return -1;
                /* else */

                addr.sa_family = AF_UNIX;
                (void) strncpy(addr.sa_data, name,
                        sizeof(addr.sa_data));
                if (connect(sock, &addr, sizeof(addr)) == -1)
                {
                        const int errnum = errno;
                        (void) close(sock);
                        sock = -1;
#ifdef EPROTOTYPE /* Linux ulog */
                        if(logType == SOCK_DGRAM && errnum == EPROTOTYPE)
                        {
                                /* retry with stream socket type */
                                logType = SOCK_STREAM;
                                errno = 0;
                                continue;
                        }
                        /* else */
#endif
                        return -1;
                }
                break; /* normal loop exit */
        }

        return sock;
}


/*
 * Attempt to connect to a internet domain udp socket.
 * Create & connect.
 * Returns (socket) descriptor or -1 on error.
 */
int
udpopen(
        const char *hostname, 
        const char *servicename
)
{
        int sock;
        int port;
        struct sockaddr_in addr;        /* AF_INET address */

        sock = socket(AF_INET, SOCK_DGRAM, 0);
        if(sock == -1)
                return -1;
        /* else */

        if(addrbyhost(hostname, &addr) == -1)
        {
                (void) close(sock);
                return -1;
        }
        /* else */

        if((port = getservport(servicename, "udp")) == -1)
        {
                (void) close(sock);
                return -1;
        }
        /* else */
        addr.sin_port = (unsigned short) port;

        if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) == -1)
        {
                (void) close(sock);
                return -1;
        }
        /* else */

        return sock;
}


/*
 * Macro for rounding-up the positive value x to the nearest multiple of n:
 */
#undef  ROUNDUP
#define ROUNDUP(x,n)    ((x % n) ? (x + (n - (x % n))) : x)


/*
 * Return a new (allocated) host entry.
 */
static
struct hostent*
hostent_new(
    const char          *name
)
{
    struct hostent      *new = NULL;
    struct hostent      *entry;

    /*
     * Retrieve the host's entry.
     */
    entry = gethostbyname(name);
    if (NULL == entry)
        uerror("Couldn't get information on host %s: %s", name, host_err_str());
    else
    {
        int             num_aliases;
        int             num_addr;
        char            **from_alias;
        char            **to_alias;
        char            *cp;
        size_t          nbytes;
        size_t          h_name_off;
        size_t          h_aliases_off;
        size_t          addrs_off;
        size_t          h_addr_list_off;
        struct in_addr  **from_addr;
        struct in_addr  **to_addr;
        struct in_addr  *addrp;

        /*
         * Compute the size requirements and offsets for the new host entry.
         */

        /* Basic size of host entry structure: */
        nbytes = sizeof(struct hostent);

        /* Offset and size of official name string: */
        h_name_off = nbytes;
        nbytes += strlen(entry->h_name) + 1;

        /* Offset and length of aliases: */
        nbytes = (size_t)ROUNDUP(nbytes, sizeof(char*));
        h_aliases_off = nbytes;
        for (from_alias = entry->h_aliases; NULL != *from_alias; from_alias++)
             nbytes += strlen(*from_alias) + 1;
        num_aliases = (int)(from_alias - entry->h_aliases);
        nbytes += sizeof(char*) * (num_aliases + 1);

        /* Offset and length of addresses: */
        nbytes = (size_t)ROUNDUP(nbytes, sizeof(struct in_addr*));
        h_addr_list_off = nbytes;
        for (from_addr = (struct in_addr**)entry->h_addr_list;
                        NULL != *from_addr; )
            from_addr++;
        num_addr = (int)(from_addr - (struct in_addr**)entry->h_addr_list);
        nbytes += sizeof(struct in_addr*) * (num_addr + 1);
        nbytes = (size_t)ROUNDUP(nbytes, sizeof(struct in_addr));
        addrs_off = nbytes;
        nbytes += sizeof(struct in_addr) * num_addr;

        /*
         * Allocate the new host entry.
         */
        new = (struct hostent *) malloc(nbytes);
        if (NULL == new)
            serror(
            "Couldn't allocate %lu bytes for information on host \"%s\"", 
                   (unsigned long) nbytes, name);
        else
        {
            /* Copy non-pointer members. */
            new->h_addrtype = entry->h_addrtype;
            new->h_length = entry->h_length;

            /* Copy official host name. */
            new->h_name = (char *) new + h_name_off;
            (void) strcpy(new->h_name, entry->h_name);

            /* Copy aliases. */
            new->h_aliases = (char**)((char*)new + h_aliases_off);
            cp = (char *) (new->h_aliases + num_aliases);
            for (from_alias = entry->h_aliases, to_alias = new->h_aliases;
                 NULL != *from_alias;
                 from_alias++, to_alias++)
            {
                *to_alias = cp;
                (void) strcpy(*to_alias, *from_alias);
                cp += strlen(*to_alias) + 1;
            }
            *to_alias = NULL;

            /* Copy addresses. */
            new->h_addr_list = (char**)((char*)new + h_addr_list_off);
            addrp = (struct in_addr*)((char*)new + addrs_off);
            for (from_addr = (struct in_addr**)entry->h_addr_list,
                    to_addr = (struct in_addr**)new->h_addr_list;
                 NULL != *from_addr;
                 from_addr++, to_addr++)
            {
                *to_addr = addrp++;
                **to_addr = **from_addr;
            }
            *to_addr = NULL;
        }                                       /* new host entry allocated */
    }                                           /* host entry retrieved */

    return new;
}


/*
 * Compare two (possibly fully-qualified) hostnames.  Indicate if they
 * refer to the same host.  If one of them isn't fully-qualified, then
 * assume it's in the same domain as the other.
 *
 * Returns:
 *      0       Different hosts
 *      1       Same host
 */
static int
same_host(
    const char  *name1,
    const char  *name2
)
{
    return (name1 == name2) ||
           (strcmp(name1, name2) == 0) ||
           (strstr(name1, name2) == name1 && name1[strlen(name2)] == '.') ||
           (strstr(name2, name1) == name2 && name2[strlen(name1)] == '.');
}


/*
 * Attempt to determine if "remote" is the same as this host.
 * Could be lots smarter...
 */
int
isMe(
        const char *remote
)
{
        static char *names[] = {
                "localhost",
                "loopback",
                NULL /* necessary terminator */
        };
        char *me;
        char **npp;
        static struct hostent *hp;

        /* Check `local host' aliases. */
        for (npp = names; *npp != NULL; npp++)
                if (same_host(remote, *npp))
                        return 1;

        me = ghostname();
        if (me == NULL)
                return 0;

        /* Check my nominal hostname. */
        if (same_host(me, remote))
                return 1;

        /* Cache host information on myself. */
        if (NULL == hp)
                hp = hostent_new(me);

        /* Check my aliases. */
        if (hp != NULL)
        {
                for(npp = hp->h_aliases; *npp != NULL; npp++)
                        if (same_host(*npp, remote))
                                return 1;
        }

        return 0;
}


/*
 * Sets the socket Internet address of the local host.
 *
 * Returns:
 *   0      Success
 *  !0      Failure.  errno set.
 */
int
local_sockaddr_in(struct sockaddr_in* addr)
{
    int                error;
    static int         cached = 0;
    struct sockaddr_in cachedAddr;

    if (cached) {
        (void)memcpy(addr, &cachedAddr, sizeof(cachedAddr));

        error = 0;
    }
    else {
        char name[256];

        (void)memset(&cachedAddr, 0, sizeof(cachedAddr));

        if (gethostname(name, sizeof(name))) {
            serror("gethostname()");

            error = errno;
        }
        else {
            error = 0;

            if (addrbyhost(name, &cachedAddr)) {
                if (addrbyhost("localhost", &cachedAddr)) {
                    if (addrbyhost("0.0.0.0", &cachedAddr)) {
                        serror("addrbyhost()");

                        error = errno;
                    }
                }
            }

            if (!error)
                (void)memcpy(addr, &cachedAddr, sizeof(cachedAddr));
        }
    }

    return error;
}


#ifndef TIRPC
/*
 * Create a socket of type "udp" or "tcp" and bind it
 * to port.
 * Return the socket or -1 on error.
 */
int
sockbind(
        const char *type,
        unsigned short port
)
{
        int sock = -1;
        struct sockaddr_in addr;
        size_t len = sizeof(struct sockaddr_in);

        if(type == NULL)
                return -1;

        if(*type == 't')
                sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        else if(*type == 'u')
                sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

        if(sock == -1) 
                return -1;

        /*
         * Eliminate problem with EADDRINUSE for reserved socket.
         * We get this if an upstream data source hasn't tried to
         * write on the other and we are in FIN_WAIT_2
         */
        if(*type == 't')
        {
                int on = 1;
                (void) setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
                        (char *)&on, sizeof(on));
        }

        (void) memset((char *)&addr, 0, len);
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = INADDR_ANY;
        addr.sin_port = (short)htons((short)port);

        if(bind(sock, (struct sockaddr *)&addr, len) < 0)
        {
                (void) close(sock);             
                return -1;
        }

        return sock;
}
#else
/*
 * TLI version of above function.
 * Create a TLI transport endpoint of type "udp" or "tcp"
 * and bind it to port.
 * Return the descriptor or -1 on error.
 * Only tested on SunOS 5.x
 */
#include <tiuser.h>
#include <fcntl.h>

int
sockbind(
        const char *type,
        unsigned short port
)
{
        int sock = -1;
        struct sockaddr_in sin_req;
        struct t_bind *req, *ret;
        extern void terror();

        if(type == NULL)
                return -1;

        if(*type == 't')
                sock = t_open("/dev/tcp", O_RDWR, NULL);
        else if(*type == 'u')
                sock = t_open("/dev/udp", O_RDWR, NULL);

        if((sock == -1) || ( t_getstate(sock) != T_UNBND) )
        {
                terror("sockbind: t_open");
                goto err0;
        }

        req = (struct t_bind *)t_alloc(sock, T_BIND, T_ADDR);
        if(req == NULL)
        {
                terror("sockbind: t_alloc req");
                goto err1;
        }
        ret = (struct t_bind *)t_alloc(sock, T_BIND, T_ADDR);
        if(ret == NULL)
        {
                terror("sockbind: t_alloc ret");
                goto err2;
        }

        (void) memset((char *)&sin_req, 0, sizeof(sin_req));
        sin_req.sin_family = AF_INET;
        sin_req.sin_addr.s_addr = INADDR_ANY;
        sin_req.sin_port = htons(port);

        (void) memcpy(req->addr.buf, (char *)&sin_req, sizeof(sin_req));
        req->addr.len = sizeof(sin_req);
        req->qlen = 32; /* rpc_soc.c uses 8 */

        if(t_bind(sock, req, ret) < 0)
        {
                terror("sockbind: t_bind");
                goto err3;
        }
        if(memcmp(req->addr.buf, ret->addr.buf, ret->addr.len) != 0)
        {
                uerror("sockbind: memcmp: t_bind changed address");
        }

        (void) t_free((char *)req, T_BIND);
        (void) t_free((char *)ret, T_BIND);
        return sock;

err3 :
        (void) t_free((char *)ret, T_BIND);
err2 :
        (void) t_free((char *)req, T_BIND);
err1 :
        (void) close(sock);             
err0 :
        return -1;
}

#endif /* !TIRPC */
