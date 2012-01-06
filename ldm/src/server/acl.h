/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: acl.h,v 1.24.18.8 2008/04/15 16:34:12 steve Exp $ */
#ifndef _ACL_H
#define _ACL_H

#include <sys/types.h>
#include <regex.h>
#include "ldm.h"
#include "peer_info.h"
#include "wordexp.h"
#include "error.h"
#include "UpFilter.h"

#ifndef ENOERR
#define ENOERR 0
#endif

typedef struct RequestEntry	RequestEntry;

enum host_set_type { HS_NONE, HS_NAME, HS_DOTTED_QUAD, HS_REGEXP };
typedef struct {
	enum host_set_type type;
	const char *cp;	/* hostname or pattern */
	regex_t rgx; 
} host_set;

extern void
free_host_set(host_set *hsp);

extern host_set *
new_host_set(enum host_set_type type, const char *cp, const regex_t *rgxp);

extern int
allow_acl_add(
    feedtypet			ft,
    const char* const		pattern,
    regex_t* const		regex,
    const host_set*		hsp);

extern int
forn_acl_ck(peer_info *rmtip, prod_class_t *want);

int acl_product_intersection(const char *name, const struct in_addr *addr, 
    const prod_class_t *want, prod_class_t **const intersect);

extern int
accept_acl_add(feedtypet ft, const char *pattern,
	 const regex_t *rgxp, const host_set *hsp, int isPrimary);

extern int
hiya_acl_ck(peer_info *rmtip, prod_class_t *offered);

int acl_check_hiya(const char *name, const char *dotAddr, 
    prod_class_t *offerd, prod_class_t **accept, int *isPrimary);

extern int
request_add(feedtypet ft, const char *pattern,
	const regex_t *rgxp, const host_set *hsp, int isPrimary);

/*
 * Saves information on the last, successfully-received product under a key
 * that comprises the relevant components of the data-request.
 */
void
savePreviousProdInfo(void);

extern int
invert_request_acl(unsigned port);

extern int
exec_add(wordexp_t *wrdexpp);

extern void
exec_free(
    const pid_t         pid);

extern int
exec_getCommandLine(
    const pid_t		pid,
    char* const		buf,
    size_t		size);

extern int
host_ok(const peer_info *rmtip);

int
requestEntry_get(
    RequestEntry** const	entry,
    const feedtypet		ft,
    const char* const		pattern,
    const regex_t* const	regex);

int
requestEntry_addHost(
    RequestEntry* const		entry,
    const char* const		hostId,
    const unsigned		port);

ErrorObj*
acl_getUpstreamFilter(
    const char*			name,
    const struct in_addr*	addr, 
    const prod_class_t*		want,
    UpFilter** const		upFilter);

ErrorObj*
acl_addAllow(
    const feedtypet		ft,
    const host_set* const	hostSet,
    const char* const		okEre,
    const char* const		notEre);

#endif /* !_ACL_H */
