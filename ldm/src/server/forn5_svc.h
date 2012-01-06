#ifndef _FORN_SVC_H_
#define _FORN_SVC_H_
/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: forn5_svc.h,v 1.2.20.1 2008/04/15 16:34:13 steve Exp $ */

#include "pq.h"

pq_seqfunc
feed5_sqf;

pq_seqfunc
noti5_sqf;

ldm_replyt *
forn_5_svc(prod_class_t *want, struct svc_req *rqstp, const char *ident,
	pq_seqfunc *doit);

#endif /*!_FORN_SVC_H_*/
