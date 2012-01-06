/* $Id: down6.h,v 1.4.10.1.4.4 2009/05/21 20:30:47 steve Exp $ */

#ifndef DOWN6_H
#define DOWN6_H

#include "ldm.h"  /* prod_class */
#include "pq.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    DOWN6_SUCCESS = 0,
    DOWN6_CLIENT_FAILURE,
    DOWN6_VERSION_MISMATCH,
    DOWN6_TIME_OUT,
    DOWN6_INTERRUPT,
    DOWN6_UNKNOWN_HOST,
    DOWN6_UNAVAILABLE,
    DOWN6_SYSTEM_ERROR,
    DOWN6_SERVICE_FAILURE,
    DOWN6_CLOSED,
    DOWN6_PQ,
    DOWN6_PQ_BIG,
    DOWN6_BAD_PACKET,
    DOWN6_UNWANTED,
    DOWN6_UNINITIALIZED
} down6_error_t;

int
down6_init(
    const char*			upName,
    const struct sockaddr_in*	upAddr,
    const char*			pqPath,
    pqueue* 			pq);

const prod_info*
down6_getLastInfo();

int
down6_set_prod_class(
    prod_class_t*			prod_class);

prod_class_t*
down6_get_prod_class();

int
down6_hereis(
    product*			prod);

int
down6_notification(
    prod_info*			info);

int
down6_comingsoon(
    comingsoon_args*		argp);

int
down6_blkdata(
    datapkt*			block);

void
down6_destroy();

#ifdef __cplusplus
}
#endif

#endif
