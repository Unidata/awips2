
/*
 *   Copyright 2003, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldm_clnt.c,v 5.9.2.4.6.7 2007/02/22 19:43:16 steve Exp $ */

#include <ldmconfig.h>

#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <assert.h>
#include <netdb.h>
#include <signal.h>
#include <stddef.h>
#include <unistd.h>

#include "error.h"
#include "inetutil.h"
#include "ldm.h"
#include "ldm_clnt.h"
#include "rpcutil.h"
#include "ulog.h"
#include "globals.h"


/*
 * Potentially lengthy operation.
 */
ErrorObj*
ldm_clnt_addr(const char* const name, struct sockaddr_in* addr)
{
    struct sockaddr_in  ad;
    ErrorObj*            error;

    assert(NULL != name);
    assert(NULL != addr);

    if (addrbyhost(name, &ad)) {
        const char* msg;

        if (HOST_NOT_FOUND == h_errno) {
            msg = "no such host is known";
        }
        else if (NO_DATA == h_errno) {
            msg = "no address for name";
        }
        else if (NO_RECOVERY == h_errno) {
            msg = "unexpected server failure";
        }
        else if (TRY_AGAIN == h_errno) {
            msg = "try again later";
        }
        else {
            msg = "unknown error";
        }

        error = ERR_NEW(h_errno, NULL, msg);
    }
    else {
        *addr = ad;
        error = NULL;
    }

    return error;
}


static ErrorObj*
ldm_clnt_tcp_create(
    struct sockaddr_in* const addr,             /* modified <=> success */
    unsigned                  version,
    unsigned                  port,             /* 0 => consult portmapper */
    CLIENT** const            client,           /* modified <=> success */
    int* const                sock)             /* modified <=> success */
{
    struct sockaddr_in        ad;
    CLIENT*                   clnt;
    int                       sck;
    ErrorObj*                  error;

    assert(NULL != addr);
    assert(NULL != client);
    assert(NULL != sock);

    ad = *addr;
    sck = RPC_ANYSOCK;
    ad.sin_port = (short)htons((short)port);
    clnt = clnttcp_create(&ad, LDMPROG, version, &sck, 0, 0);

    if (clnt) {
        *client = clnt;
        *addr = ad;
        *sock = sck;
        error = NULL;
    }
    else {
        int     code;

        if (rpc_createerr.cf_stat == RPC_TIMEDOUT) {
            code = LDM_CLNT_TIMED_OUT;
        }
        else if (rpc_createerr.cf_stat == RPC_UNKNOWNHOST) {
            code = LDM_CLNT_UNKNOWN_HOST;
        }
        else if (rpc_createerr.cf_stat == RPC_PROGVERSMISMATCH) {
            code = LDM_CLNT_BAD_VERSION;
        }
        else {
            code = LDM_CLNT_NO_CONNECT;
        }

        error = ERR_NEW(code, NULL, clnt_spcreateerror(""));
    }

    return error;
}


static ErrorObj*
ldm_clnt_nullproc(CLIENT* const clnt)
{
    struct timeval timeout;
    ErrorObj*       error;

    assert(NULL != clnt);

    timeout.tv_sec = 25;        /* RPC default */
    timeout.tv_usec = 0;

    if (clnt_call(clnt, NULLPROC, xdr_void, NULL, xdr_void,
            NULL, timeout) == 0) {
        error = NULL;
    }
    else {
        struct rpc_err rpcErr;

        clnt_geterr(clnt, &rpcErr);

        error = ERR_NEW(rpcErr.re_status, NULL, clnt_errmsg(clnt));
    }

    return error;
}


/*
 * Attempts to connect to an upstream LDM using a range of LDM versions.  The
 * versions are tried, in order, from highest to lowest.  This function returns
 * on the first successful attempt.  If the host is unknown or the RPC call
 * times-out, then the version-loop is prematurely terminated and this function
 * returns immediately.
 *
 * The client is responsible for freeing the client resources set by this
 * function on success.  Calls exitIfDone() after potentially lengthy
 * operations.
 *
 * Arguments:
 *   upName                The name of the upstream LDM host.
 *   port                  The port on which to connect.
 *   version               Program version.
 *   *client               Pointer to CLIENT structure. Set on success.
 *   *socket               The socket used for the connection.  May be NULL.
 *   *upAddr               The IP address of the upstream LDM host.  Set on
 *                         success.  May be NULL.
 * Returns:
 *    NULL                 Success.  *vers_out, *client, *sock_out, and *upAddr
 *                         set.
 *   !NULL                 Error.  err_code(RETURN_VALUE):
 *       LDM_CLNT_UNKNOWN_HOST         Unknown upstream host.
 *       LDM_CLNT_TIMED_OUT            Call to upstream host timed-out.
 *       LDM_CLNT_BAD_VERSION          Upstream LDM isn't given version.
 *       LDM_CLNT_NO_CONNECT           Other connection-related error.
 *       LDM_CLNT_SYSTEM_ERROR         A fatal system-error occurred.
 */
ErrorObj*
ldm_clnttcp_create_vers(
    const char* const            upName,
    const unsigned               port,
    unsigned const               version,
    CLIENT** const               client,
    int* const                   socket,
    struct sockaddr_in*          upAddr)
{
    ErrorObj*           error;
    struct sockaddr_in  addr;

    assert(upName != NULL);
    assert(client != NULL);

    /*
     * Get the IP address of the upstream LDM.  This is a potentially
     * lengthy operation.
     */
    error = ldm_clnt_addr(upName, &addr);

    if (error) {
        error = ERR_NEW1(LDM_CLNT_UNKNOWN_HOST, error, 
            "Couldn't get IP address of host %s", upName);
    }
    else {
        int                     sock;
        int                     errCode;
        CLIENT*                 clnt = NULL;

        (void)exitIfDone(0);

        /*
         * Connect to the remote port.  This is a potentially lengthy
         * operation.
         */
        error = ldm_clnt_tcp_create(&addr, version, port, &clnt, &sock);

        if (error) {
            errCode = err_code(error);

            if (LDM_CLNT_NO_CONNECT != errCode) {
                error =
                    ERR_NEW3(errCode, error, 
                        "Couldn't connect to LDM %d on %s "
                            "using port %d",
                        version, upName, port);
            }
            else {
                err_log_and_free(
                    ERR_NEW3(0, error, 
                        "Couldn't connect to LDM %d on %s using port "
                            "%d",
                        version, upName, port),
                    LOG_INFO);

                (void)exitIfDone(0);

                /*
                 * Connect using the portmapper.  This is a
                 * potentially lengthy operation.
                 */
                error = ldm_clnt_tcp_create(&addr, version, 0,
                    &clnt, &sock);

                if (error) {
                    error =
                        ERR_NEW2(err_code(error), error, 
                            "Couldn't connect to LDM on %s "
                            "using either port %d or portmapper",
                            upName, port);
                }                       /* portmapper failure */
            }                           /* non-fatal port failure */
        }                               /* port failure */

        if (NULL != clnt) {
            assert(!error);

            (void)exitIfDone(0);

            /*
             * Test the connection with a NULLPROC call.
             * This is a potentially lengthy operation.
             */
            error = ldm_clnt_nullproc(clnt);

            if (error) {
                errCode = err_code(error);

                if (RPC_TIMEDOUT == errCode) {
                    errCode = LDM_CLNT_TIMED_OUT;
                }
                else if (RPC_PROGVERSMISMATCH == errCode) {
                    errCode = LDM_CLNT_BAD_VERSION;
                }
                else {
                    errCode = LDM_CLNT_NO_CONNECT;
                }

                error = ERR_NEW2(errCode, error,
                    "nullproc_%u failure to %s", version, upName);
            }                       /* NULLPROC failure */
            else {
                /*
                 * Success.  Set the return arguments.
                 */
                *client = clnt;
                if (socket)
                    *socket = sock;
                if (upAddr)
                    *upAddr = addr;
            }

            if (error) {
                auth_destroy(clnt->cl_auth);
                clnt_destroy(clnt);
                clnt = NULL;
                /*
                 * The socket should have been closed by clnt_destroy()
                 * -- but this can't hurt.
                 */
                (void)close(sock);
            }
        }                                   /* clnt != NULL */
    }                                       /* got upstream IP address */

    return error;
}
