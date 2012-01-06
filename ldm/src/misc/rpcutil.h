#ifndef RPCUTIL_H
#define RPCUTIL_H

#include <rpc/rpc.h>

char *clnt_errmsg(CLIENT* clnt);
int local_portmapper_running();

#endif
