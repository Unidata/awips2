#include "ldmconfig.h"
#include <rpc/rpc.h>
#include <sys/time.h>
#include "ldm.h"
#include <string.h>

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 60, 0 };
static struct timeval ZERO_TIMEOUT = { 0, 0 };

fornme_reply_t *
feedme_6(argp, clnt)
        feedpar_t *argp;
        CLIENT *clnt;
{
        static fornme_reply_t res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, FEEDME, xdr_feedpar_t, argp, xdr_fornme_reply_t, &res, TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return (&res);
}


fornme_reply_t *
notifyme_6(argp, clnt)
        prod_class_t *argp;
        CLIENT *clnt;
{
        static fornme_reply_t res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, NOTIFYME, xdr_prod_class_t, argp, xdr_fornme_reply_t, &res, TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return (&res);
}


bool_t *
is_alive_6(argp, clnt)
        u_int *argp;
        CLIENT *clnt;
{
        static bool_t res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, IS_ALIVE, xdr_u_int, argp, xdr_bool, &res, TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return (&res);
}


hiya_reply_t *
hiya_6(argp, clnt)
        prod_class_t *argp;
        CLIENT *clnt;
{
        static hiya_reply_t res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, HIYA, xdr_prod_class_t, argp, xdr_hiya_reply_t, &res, TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return (&res);
}


void *
notification_6(argp, clnt)
        prod_info *argp;
        CLIENT *clnt;
{
        static char res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, NOTIFICATION, xdr_prod_info, argp, NULL, &res, ZERO_TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return ((void *)&res);
}


void *
hereis_6(argp, clnt)
        product *argp;
        CLIENT *clnt;
{
        static char res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, HEREIS, xdr_product, argp, NULL, &res, ZERO_TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return ((void *)&res);
}


comingsoon_reply_t *
comingsoon_6(argp, clnt)
        comingsoon_args *argp;
        CLIENT *clnt;
{
        static comingsoon_reply_t res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, COMINGSOON, xdr_comingsoon_args, argp, xdr_comingsoon_reply_t, &res, TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return (&res);
}


void *
blkdata_6(argp, clnt)
        datapkt *argp;
        CLIENT *clnt;
{
        static char res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, BLKDATA, xdr_datapkt, argp, NULL, &res, ZERO_TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return ((void *)&res);
}


void*
nullproc_6(void *argp, CLIENT *clnt)
{
        static char clnt_res;
        if (clnt_call(clnt, NULLPROC,
                (xdrproc_t) xdr_void, (caddr_t) argp,
                (xdrproc_t) xdr_void, (caddr_t) &clnt_res,
                TIMEOUT) != RPC_SUCCESS) {
            return NULL;
        }
        return ((void *)&clnt_res);
}


enum clnt_stat clnt_stat(CLIENT *clnt)
{
    struct rpc_err rpcErr;

    clnt_geterr(clnt, &rpcErr);

    return rpcErr.re_status;
}
