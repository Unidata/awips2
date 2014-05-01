/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: feed.c,v 1.60.2.1.6.2 2006/02/24 22:42:59 steve Exp $ */

#include <ldmconfig.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <limits.h> /* PATH_MAX */
#ifndef PATH_MAX
#define PATH_MAX 255
#endif /* !PATH_MAX */

#include "inetutil.h"
#include "feed.h"
#include "ulog.h"
#include "rawfile.h"
#ifndef MCIDAS_ONLY
#include "xbuf.h"
#endif


#if NET
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h> /* sockaddr_in */
#include <netdb.h>

int server_port = 0; /* set in main */

/*
 * Returns:
 *   0             Success.
 *   ENOENT        gethostbyname() failure.
 *   EAFNOSUPPORT  AF_INET address-family not supported.
 *   EMFILE        No more file descriptors available for this process.
 *   ENFILE        No more file descriptors available for the system.
 *   EACCES        The process does not have appropriate privileges.
 *   ENOBUFS       Insufficient resources were available in the system to
 *                 perform the operation.
 *   ENOMEM        Insufficient memory was available to fulfill the request.
 *   ENOSR         There were insufficient STREAMS resources available for the 
 *                 operation to complete.
 *   EADDRNOTAVAIL The specified address is not available from the local 
 *                 machine.
 *   ECONNREFUSED  The target address was not listening for connections or 
 *                 refused the connection request.
 *   EINTR         The attempt to establish a connection was interrupted by
 *                 delivery of a signal that was caught; the connection will be
 *                 established asynchronously.
 *   ENETUNREACH   No route to the network is present.
 *   EPROTOTYPE    The specified address has a different type than the socket 
 *                 bound to the specified peer address.
 *   ETIMEDOUT     The attempt to connect timed out before a connection was 
 *                 made.
 *   ECONNRESET    Remote host reset the connection request.
 *   EHOSTUNREACH  The destination host cannot be reached (probably because the
 *                 host is down or a remote router cannot reach it).
 *   ENETDOWN      The local interface used to reach the destination is down.
 */   
int
port_open(const char *remote, unsigned short port, int *const fdp)
{
        int status = ENOERR;
        int sock;
        struct sockaddr_in addr;

        if (addrbyhost(remote, &addr) != 0)
        {
                status = ENOENT;
                serror("gethostbyname(%s)", remote);
                return status;
        }

        sock = socket(AF_INET, SOCK_STREAM, 0);
        if(sock < 0)
        {
                status = errno;
                serror("socket");
                return status;
        }
#if 0 /* doesnt seem to help. See VOODOO in pqing.c */
        {
                const int optval = 1;
                if(setsockopt(sock, SOL_SOCKET, SO_KEEPALIVE,
                                (char *) &optval, (int) sizeof(optval)) < 0)
                {
                        status = errno;
                        serror("setsockopt SO_KEEPALIVE");
                        return status;
                }
        }
#endif

        addr.sin_port = htons(port);
        if(connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0)
        {
                status = errno;
                serror("connect");
                (void) close(sock);
                return status;
        }
        
        unotice("NET \"%s\" %hu", remote, port);
        *fdp = sock;
        return status;
}

#endif /* NET */


static void
null_stats(void)
{
        unotice("  No feed statistics: never opened feed");
}

/* pointer set to the right function by open_feed() */
void (*feed_stats)(void) = null_stats;
/* pointer set to the right function by open_feed() */
int (*feed_close)(int) = 0;

/*
 *    Given a path, decide what type of device we are inputing from.
 *  Assummes we have already determine that it is not a plain file.
 *
 *  N.B. Depends on the "conventional" names for devices
 *  No safety net.
 *
 *  You should decide which devices (TTY, SUNLINK, CLEO) to support
 *  by editing the "Configuration Section" of feed.h.
 *  Not all devices make sense for all systems.
 */
static int
which(const char *path)
{
        char *cp;

#define SEP     '/' /* separates components of path */

        /* strip off leading path */
        if ((cp = strrchr(path, SEP)) != NULL)
        {
                path = cp + 1;
        }
#ifdef TTY
        if(strncmp(path,"tty",3)== 0 )
                return(TTY);
#endif /* TTY */
#ifdef SUNLINK
        if(strncmp(path,"ifd",3)== 0 || strncmp(path,"dcpa",4) == 0 )
                return(SUNLINK);
#endif /* SUNLINK */
#ifdef CLEO
        /* kludge alert, requires link or logical */
        if(strncmp(path,"cleo",4)== 0 || strncmp(path,"CLEO",4) == 0 )
                return(CLEO);
#endif /* CLEO */
        return(UNKNOWN);
}


/*
 *    Given a feedname (path in the file system),
 * determine what type of input feed it is.  Attempt to open
 * the feed. Hook up the appropriate read_,  _stats, and (optional) _close
 * function pointers.  Return errno on failure else set *fdp to the descriptor
 * and return 0.
 *
 * Returns (if INPUT_IS_SOCKET):
 *   ENOENT        gethostbyname() failure.
 *   EAFNOSUPPORT  AF_INET address-family not supported.
 *   EMFILE        No more file descriptors available for this process.
 *   ENFILE        No more file descriptors available for the system.
 *   EACCES        The process does not have appropriate privileges.
 *   ENOBUFS       Insufficient resources were available in the system to
 *                 perform the operation.
 *   ENOMEM        Insufficient memory was available to fulfill the request.
 *   ENOSR         There were insufficient STREAMS resources available for the 
 *                 operation to complete.
 *   EADDRNOTAVAIL The specified address is not available from the local 
 *                 machine.
 *   ECONNREFUSED  The target address was not listening for connections or 
 *                 refused the connection request.
 *   EINTR         The attempt to establish a connection was interrupted by
 *                 delivery of a signal that was caught; the connection will be
 *                 established asynchronously.
 *   ENETUNREACH   No route to the network is present.
 *   EPROTOTYPE    The specified address has a different type than the socket 
 *                 bound to the specified peer address.
 *   ETIMEDOUT     The attempt to connect timed out before a connection was 
 *                 made.
 *   ECONNRESET    Remote host reset the connection request.
 *   EHOSTUNREACH  The destination host cannot be reached (probably because the
 *                 host is down or a remote router cannot reach it).
 *   ENETDOWN      The local interface used to reach the destination is down.
 */
int
open_feed(const char *feedfname, int *const fdp)
{
        extern char *rawfname; /* declared in main() module */
        int status = ENOERR;

#if NET
        if(INPUT_IS_SOCKET)
        {
                status = port_open(feedfname,
                                (unsigned short) server_port, fdp);
                if(status != ENOERR)
                        return status;
                if((status = initTheXbuf(read_file)) != ENOERR)
                        return status;
                feed_stats = file_stats;
                feed_close = file_close;
        }
        else    /* dangling else */
#endif /* NET */
        switch( which(feedfname) ) {
#ifdef TTY
        case TTY :
                if((status = tty_open(feedfname, fdp)) != ENOERR)
                        return status;
                if((status = initTheXbuf(read_tty)) != ENOERR)
                        return status;
                feed_stats = tty_stats;
                feed_close = tty_close;
                break;
#endif /* TTY */
        default :
                /* just treat it like a file */
                if(*feedfname == '-' && feedfname[1] == 0)
                {
                        /* use stdin */
                        *fdp = fileno(stdin);
                        unotice("FILE stdin");
                }
                else
                {
                        if((status = file_open(feedfname, fdp)) != ENOERR)
                                return status;
                }
                if((status = initTheXbuf(read_file)) != ENOERR)
                        return status;
                feed_stats = file_stats;
                feed_close = file_close;
                break;
        }

        if(rawfname != NULL)
        {
                (void) open_rawfile( rawfname );
        }

        return status;
}
