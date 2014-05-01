/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: acl.c,v 1.98.2.1.6.1.2.4.2.28 2008/04/15 16:34:12 steve Exp $ */

/*LINTLIBRARY*/

#include <ldmconfig.h>

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>             /* UINT_MAX */
#include <netdb.h>
#include <rpc/rpc.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <regex.h>
#include <unistd.h>

#include "abbr.h"
#include "acl.h"
#include "autoshift.h"
#include "down6.h"
#include "error.h"
#include "feedTime.h"
#include "globals.h"            /* global "pq"; defined in ldmd.c */
#include "inetutil.h"
#include "ldm5_clnt.h"
#include "ldmprint.h"
#include "pattern.h"
#include "peer_info.h"
#include "pq.h"
#include "priv.h"
#include "prod_class.h"
#include "prod_info.h"
#include "RegularExpressions.h"
#include "requester6.h"
#include "rpcutil.h"
#include "savedInfo.h"
#include "timestamp.h"
#include "ulog.h"
#include "log.h"
#include "UpFilter.h"
#include "md5.h"

extern const char*      ldm_version;


void
free_host_set(host_set *hsp)
{
        if(hsp == NULL)
                return;
        if(hsp->type == HS_REGEXP)
                regfree(&hsp->rgx); /* cast away const */
        if(hsp->cp != NULL)
                free((void *)hsp->cp); /* cast away const */
        free(hsp);
}

/*
 * Arguments
 *      cp      Pointer to host(s) specification.  Caller must not free on
 *              return if and only if call is successful and "type" is
 *              HS_REGEXP.
 *      rgxp    Pointer to regular-expression structure.  Caller may free
 *              on return but must not call regfree() if and only if call is
 *              successful and "type" is HS_REGEXP.
 */
host_set *
new_host_set(enum host_set_type type, const char *cp, const regex_t *rgxp)
{
        host_set *hsp = (host_set *)malloc(sizeof(host_set));
        if(hsp == NULL)
                return NULL;

        hsp->type = type;
        hsp->cp = NULL;
        (void)memset(&hsp->rgx, 0, sizeof(regex_t));

        if(cp != NULL)
        {
                switch (type) {
                case HS_NAME:
                case HS_DOTTED_QUAD:
                        hsp->cp = strdup(cp);           
                        if(hsp->cp == NULL)
                                goto unwind_alloc;
                        break;
                case HS_REGEXP:
                        /* private copies already allocate in the lexer */
                        hsp->cp = cp;
                        hsp->rgx = *rgxp;
                        break;
                }
        }

        return hsp;

unwind_alloc:
        free(hsp);
        return NULL;
}

static int
host_set_match(const peer_info *rmtip, const host_set *hsp)
{
        if(rmtip == NULL || hsp == NULL)
                return 0;
        if(rmtip->astr == hsp->cp || rmtip->name == hsp->cp)
                return 1;
        switch (hsp->type) {
        case HS_NAME:
                if(strcasecmp(rmtip->name, hsp->cp) == 0)
                        return 1;
                break;
        case HS_DOTTED_QUAD:
                if(strcmp(rmtip->astr, hsp->cp) == 0)
                        return 1;
                break;
        case HS_REGEXP:
                if(regexec(&hsp->rgx, rmtip->astr, 0, NULL, 0) == 0)
                        return 1;
                /* else */
                if(regexec(&hsp->rgx, rmtip->name, 0, NULL, 0) == 0)
                        return 1;
                break;
        }
        return 0;
}


struct aclt {
        feedtypet       ft;
        const char*     pattern;
        regex_t         rgx;
        const host_set* hsp;
        struct aclt*    next;
        int             isPrimary;
};
typedef struct aclt aclt;

typedef struct AllowEntry {
    struct AllowEntry*          next;
    const host_set*             hsp;
    Pattern*                    okPattern;
    Pattern*                    notPattern;
    feedtypet                   ft;
} AllowEntry;

typedef struct {
    char*       hostId;
    unsigned    port;
} RemoteLocation;

struct RequestEntry {
    regex_t                     regex;
    struct RequestEntry*        next;
    char*                       pattern;
    RemoteLocation*             remoteLocations;
    feedtypet                   ft;
    unsigned                    hostCount;
};


static AllowEntry*      allowEntryHead = NULL;
static AllowEntry*      allowEntryTail = NULL;
static aclt*            accept_acl;
static RequestEntry*    requestEntryHead = NULL;
static RequestEntry*    requestEntryTail = NULL;


/*
 * Returns the RequestEntry corresponding to a feedtype and pattern -- creating
 * it if necessary.
 *
 * Arguments:
 *      entry           Pointer to pointer to be set to new RequestEntry.
 *      ft              Feedtype.
 *      pattern         Pointer to ERE pattern for product-identifier.  Caller 
 *                      may free on return.
 *      regex           Pointer to compiled ERE for product-identifier.  On 
 *                      success, caller must not call regfree(regex) or modify
 *                      *regex.
 * Returns:
 *      0       Success
 *      ENOMEM  Out of memory.
 */
int
requestEntry_get(
    RequestEntry** const        entry,
    const feedtypet             ft,
    const char* const           pattern,
    const regex_t* const        regex)
{
    int                 errCode = 0;    /* success */
    RequestEntry*       ent;

    for (ent = requestEntryHead; ent != NULL; ent = ent->next)
        if (ent->ft == ft && strcmp(pattern, ent->pattern) == 0)
            break;

    if (NULL != ent) {
        *entry = ent;
    }
    else {
        ent = (RequestEntry*)malloc(sizeof(RequestEntry));

        if (NULL == ent) {
            errCode = ENOMEM;
        }
        else {
            if (NULL == (ent->pattern = strdup(pattern))) {
                errCode = ENOMEM;
                free(ent);
            }
            else {
                ent->ft = ft;
                ent->regex = *regex;
                ent->hostCount = 0;
                ent->remoteLocations = NULL;
                ent->next = NULL;
                
                if (NULL == requestEntryHead) {
                    requestEntryHead = ent;
                    requestEntryTail = ent;
                }
                else {
                    requestEntryTail->next = ent;
                    requestEntryTail = ent;
                }

                *entry = ent;
            }
        }                               /* "ent" allocated */
    }                                   /* relevant entry doesn't exist */

    return errCode;
}


/*
 * Adds a host-identifier to a RequestEntry.
 *
 * Arguments:
 *      entry   Pointer to RequestEntry to be added to.
 *      hostId  Pointer to host-identifier.  May be hostname or IP address in
 *              dotted-quad format.  Caller may free on return.
 *      port    The port on the remote host to which to connect.
 * Returns:
 *      0       Success.
 *      else    System error code.
 */
int
requestEntry_addHost(
    RequestEntry* const entry,
    const char* const   hostId,
    unsigned            port)
{
    int                 errCode = 0;            /* success */
    RemoteLocation*     remoteLocations = 
        (RemoteLocation*)realloc(entry->remoteLocations, 
            (size_t)((entry->hostCount+1)*sizeof(RemoteLocation)));

    if (NULL == remoteLocations) {
        errCode = errno;
    }
    else {
        char*   id = strdup(hostId);

        if (NULL == id) {
            errCode = errno;
            free(remoteLocations);
        }
        else {
            remoteLocations[entry->hostCount].hostId = id;
            remoteLocations[entry->hostCount].port = port;
            entry->hostCount++;
            entry->remoteLocations = remoteLocations;
        }
    }                                   /* "remoteLocations" allocated */

    return errCode;
}


#if 0
static void
free_aclt(aclt *ap)
{
        if(ap == NULL)  
                return;
        if(ap->pattern != NULL)
        {
                free((void *)ap->pattern); /* cast away const */
                regfree(&ap->rgx);
        }
        if(ap->hsp != NULL)
                free_host_set((host_set *)ap->hsp); /* cast away const */
        free(ap);
}
#endif


/*
 * Allocates and initializes an access-control entry.
 *
 * Arguments:
 *      ft              The feedtype.
 *      pattern         Pointer to the pattern for matching the data-product
 *                      identifier.  The string is not copied.
 *      rgxp            Pointer to the regular-expression for matching the
 *                      data-product identifier.  The caller may free upon
 *                      return.
 *      hsp             Pointer to the allowed set of hosts.  The set is not
 *                      copied.
 *      isPrimary       Whether or not the initial data-product exchange-mode is
 *                      primary (i.e., uses HEREIS) or alternate (i.e., uses
 *                      COMINGSOON/BLKDATA).
 * Returns:
 *      NULL            Failure.  errno is set.
 *      !NULL           Success
 */
static aclt *
new_aclt(
    feedtypet           ft,
    const char*         pattern,
    const regex_t*      rgxp,
    const host_set*     hsp,
    int                 isPrimary)
{
        aclt *ap = (aclt *)malloc(sizeof(aclt));
        if(ap == NULL)
                return NULL;
        ap->ft = ft;
        ap->pattern = pattern;
        if(rgxp != NULL)
                ap->rgx = *rgxp;
        ap->hsp = hsp;
        ap->isPrimary = isPrimary;
        ap->next = NULL;
        return ap;
}


/*
 * Adds an entry to the access-control list.
 *
 * Arguments:
 *      app             Address of the pointer to the access-control list.  May
 *                      not be NULL.
 *      ft              The feedtype.
 *      pattern         Pointer to the pattern for matching the data-product
 *                      identifier.  The string is not copied.
 *      rgxp            Pointer to the regular-expression for matching the
 *                      data-product identifier.  The caller may free upon
 *                      return.
 *      hsp             Pointer to the allowed set of hosts.  The set is not
 *                      copied.
 *      isPrimary       Whether or not the initial data-product exchange-mode is
 *                      primary (i.e., uses HEREIS) or alternate (i.e., uses
 *                      COMINGSOON/BLKDATA).
 * Returns:
 *      0               Success
 *      !0              <errno.h> error-code.
 */
static int
acl_add(
    aclt**              app,
    feedtypet           ft,
    const char*         pattern,
    const regex_t*      rgxp,
    const host_set*     hsp, 
    int                 isPrimary)
{
        aclt *ap = new_aclt(ft, pattern, rgxp, hsp, isPrimary);
        if(ap == NULL)
                return errno;

        if(*app == NULL)
        {
                *app = ap;
        }
        else
        {
                aclt *ep = *app;
                while(ep->next != NULL)
                    ep = ep->next;
                ep->next = ap;
        }
        return ENOERR;
}


/*
 * Arguments:
 *      ft              The feedtype.
 *      hostSet         Pointer to set of allowed downstream hosts.  On success,
 *                      caller must neither free nor modify upon return.
 *      okEre           Pointer to the ERE that data-product identifiers
 *                      must match.  Caller may free upon return.
 *      notEre          Pointer to the ERE that data-product identifiers
 *                      must not match or NULL if such matching should be
 *                      disabled.  Caller may free upon return.
 * Returns:
 *      NULL            Success.
 *      else            Failure error object.
 */
ErrorObj*
acl_addAllow(
    const feedtypet             ft,
    const host_set* const       hostSet,
    const char* const           okEre,
    const char* const           notEre)
{
    ErrorObj*            errObj = NULL;  /* success */
    AllowEntry* const   entry = (AllowEntry*)malloc(sizeof(AllowEntry));

    if (NULL == entry) {
        errObj = ERR_NEW1(0, NULL, "Couldn't allocate new allow-entry",
            strerror(errno));
    }
    else {
        Pattern*        okPattern;

        errObj = pat_new(&okPattern, okEre, 0);

        if (errObj) {
            errObj = ERR_NEW(0, errObj, "Couldn't create OK-pattern");

            free(entry);
        }
        else {
            Pattern*    notPattern;

            if (NULL == notEre) {
                notPattern = NULL;
            }
            else {
                if (errObj = pat_new(&notPattern, notEre, 0))
                    errObj = ERR_NEW(0, errObj, "Couldn't create not-pattern");
            }
            
            if (!errObj) {
                entry->hsp = hostSet;
                entry->okPattern = okPattern;
                entry->notPattern = notPattern;
                entry->ft = ft;
                entry->next = NULL;

                if (NULL == allowEntryHead) {
                    allowEntryHead = entry;
                    allowEntryTail = entry;
                }
                else {
                    allowEntryTail->next = entry;
                    allowEntryTail = entry;
                }
            }
        }
        
        if (errObj)
            free(entry);
    }                                   /* "entry" allocated */

    return errObj;
}


/*
 * @param *rmtip            Information on the remote host.  rmtip->clssp will
 *                          be set to the intersection unless there's an
 *                          error, or there are no matching host entries in the
 *                          ACL, or the intersection is the empty set, in which
 *                          case it will be unmodified.
 * @param *want             The product-class that the host wants.
 * @return 0                if successful.
 * @return <errno.h>ENOMEM  if out-of-memory.
 * @return <errno.h>EINVAL  if a regular expression of a product specification
 *                          couldn't be compiled.
 */
int
forn_acl_ck(peer_info *rmtip, prod_class_t *want)
{
    /*
     * The logic of this function seems very odd to me -- but I kept it the same
     * as Glenn's original forn_acl_ck().  SRE 2002-11-04
     */

    int         error;

    if (allowEntryHead == NULL || want == NULL || want->psa.psa_len == 0) {
        error = ENOERR;
    }
    else {
        prod_class_t *inter;  /* intersection of want and allowed */

        error =
            acl_product_intersection(rmtip->name, &rmtip->addr, want, &inter);

        if (!error) {
            if (inter->psa.psa_len == 0) {
                (void)free_prod_class(inter);
            }
            else {
                rmtip->clssp = inter;
            }
        }
    }

    return error;
}


static int contains(const host_set *hsp, const char *name, const char *dotAddr)
{
    int contains;

    if (hsp->type == HS_NAME) {
        contains = strcasecmp(name, hsp->cp) == 0;
    }
    else if (hsp->type == HS_DOTTED_QUAD) {
        contains = strcmp(dotAddr, hsp->cp) == 0;
    }
    else if (hsp->type == HS_REGEXP) {
        contains = 
            regexec(&hsp->rgx, dotAddr, 0, NULL, 0) == 0 ||
            regexec(&hsp->rgx, name, 0, NULL, 0) == 0;
    }
    else {
        contains = 0;
    }

    return contains;
}


/*
 * Returns the class of products that a host is allowed to receive based on the
 * host and the feed-types of products that it wants to receive.  The pointer
 * to the product-class structure will reference allocated space on success;
 * otherwise, it won't be modified.  The returned product-class may be the empty
 * set.  The client is responsible for invoking free_prod_class(prod_class_t*) on
 * a non-NULL product-class structure when it is no longer needed.
 *
 * Arguments:
 *      name            Pointer to the name of the host.
 *      addr            Pointer to the IP address of the host.
 *      want            Pointer to he class of products that the host wants.
 *      intersect       Pointer to a pointer to the intersection of the
 *                      wanted product class and the allowed product class.
 *                      References allocated space on success; otherwise won't
 *                      be modified.  Referenced product-class may be empty.
 *                      On success, the caller should eventually invoke
 *                      free_prod_class(*intersect).
 * Returns:
 *      0               Success.
 *      EINVAL          The regular expression pattern of a 
 *                      product-specification couldn't be compiled.
 *      ENOMEM          Out-of-memory.
 */
int
acl_product_intersection(
    const char           *name,
    const struct in_addr *addr, 
    const prod_class_t     *want,
    prod_class_t          **const intersect)
{
    int              error = 0;          /* default success */
    size_t           nhits = 0;          /* number of matching ACL entries */
#   define           MAXHITS 128         /* TODO */
    feedtypet        feedType[MAXHITS];  /* matching feed-types */
    prod_class_t      *inter;              /* want and allow intersection */

    /*
     * Find the number of matching entries in the ACL and save their
     * feed-types.
     */
    if (allowEntryHead == NULL || want->psa.psa_len == 0) {
        /*
         * If there's no access-control-list (ACL) or the host wants nothing, 
         * then the intersection is the empty set.
         */
        uwarn("%s:%d: no ACL or empty request", __FILE__, __LINE__);
        nhits = 0;
    }
    else {
        AllowEntry*     entry;                  /* ACL entry */
        char            dotAddr[DOTTEDQUADLEN]; /* dotted-quad IP address */

        (void)strcpy(dotAddr, inet_ntoa(*addr));

        for(entry = allowEntryHead; entry != NULL; entry = entry->next) {
            if (contains(entry->hsp, name, dotAddr)) {
                feedType[nhits++] = entry->ft;

                if (nhits >= MAXHITS) {
                    uerror("%s:%d: nhits (%u) >= MAXHITS (%d)",
                        __FILE__, __LINE__, nhits, MAXHITS);
                    break;
                }
            }
        }
    }

    /*
     * Allocate a product-class for the intersection.
     */
    inter = new_prod_class(nhits == 0 ? 0 : want->psa.psa_len);

    if(inter == NULL) {
        error = ENOMEM;
    }
    else if (nhits != 0) {
        error = cp_prod_class(inter, want, 0);

        if (error == 0) {
            /*
             * Compute the intersection.
             */
            int       ii;

            for (ii = 0; ii < inter->psa.psa_len; ii++) {
                feedtypet ft;
                size_t    jj;
                char      s1[255], s2[255], s3[255];

                sprint_feedtypet(s1, sizeof(s1),
                    inter->psa.psa_val[ii].feedtype);

                for (jj = 0; jj < nhits; jj++) {
                    sprint_feedtypet(s2, sizeof(s2), feedType[jj]);

                    ft = inter->psa.psa_val[ii].feedtype &
                        feedType[jj];

                    sprint_feedtypet(s3, sizeof(s3), ft);

                    if (ft) {
                        udebug("hit %s = %s & %s", s3, s1, s2);

                        inter->psa.psa_val[ii].feedtype = ft;

                        break; /* first match priority */
                    }
                }

                if (ft == NONE) {
                    udebug("miss %s", s1);

                    inter->psa.psa_val[ii].feedtype = NONE;
                }
            }

            clss_scrunch(inter);
        }

        if (error)
            (void)free_prod_class(inter);
    }

    if (!error)
        *intersect = inter;

    return error;
}


/*
 * Returns the product-class appropriate for filtering data-products on the
 * upstream LDM before sending them to the downstream LDM.
 *
 * Arguments:
 *      name            Pointer to the name of the downstream host.
 *      addr            Pointer to the IP address of the downstream host.
 *      want            Pointer to he class of products that this host wants.
 *      filter          Pointer to a pointer to the upstream filter.
 *                      *filter is set on and only on success.  Caller
 *                      should call upFilter_free(*filter). *filter is set to
 *                      NULL if and only if no data-products should be sent to
 *                      the downstream LDM.
 * Returns:
 *      NULL            Success.
 *      else            Failure error object.
 */
ErrorObj*
acl_getUpstreamFilter(
    const char*                 name,
    const struct in_addr*       addr, 
    const prod_class_t*           want,
    UpFilter** const            upFilter)
{
    ErrorObj*    errObj;
    UpFilter*   filt;
    
    if (errObj = upFilter_new(&filt)) {
        errObj = ERR_NEW(0, errObj, "Couldn't get new upstream filter");
    }
    else {
        int             i;
        char            dotAddr[DOTTEDQUADLEN];
        AllowEntry*     entry;

        (void)strcpy(dotAddr, inet_ntoa(*addr));

        for (i = 0; i < want->psa.psa_len; ++i) {
            for (entry = allowEntryHead; entry != NULL; entry = entry->next) {
                feedtypet       feedtype =
                    entry->ft & want->psa.psa_val[i].feedtype;

                if (feedtype && contains(entry->hsp, name, dotAddr)) {
                    if (errObj = upFilter_addComponent(filt, feedtype, 
                        entry->okPattern, entry->notPattern)) {

                        errObj = ERR_NEW2(0, errObj,
                            "Couldn't add upstream filter component for host "
                                "%s [%s]",
                            name, dotAddr);
                    }

                    break;              /* first match controls */
                }                       /* feedtype & host match */
            }                           /* ACL entry loop */
        }                               /* wanted product-specification loop */

        if (errObj) {
            upFilter_free(filt);
        }
        else {
            if (upFilter_getComponentCount(filt) > 0) {
                *upFilter = filt;
            }
            else {
                upFilter_free(filt);
                *upFilter = NULL;
            }
        }
    }                                   /* "filt" allocated */

    return errObj;
}


/*
 * Arguments:
 *      ft              Feedtype.
 *      pattern         Pointer to extended regular-expression for
 *                      product-identifier.  On success, caller must not free.
 *      rgxp            Pointer to regular-expression structure.  Caller may
 *                      free on return but must not call regfree() if and only
 *                      if call is successful.
 *      hsp             Pointer to host-set.  Caller must not free on return nor
 *                      call free_host_set() if and only if call is successful.
 *      isPrimary       Whether or not the initial data-product exchange-mode
 *                      is primary (i.e., uses HEREIS) or alternate (i.e., uses
 *                      COMINGSOON/BLKDATA).
 * Returns:
 *      0               Success
 *      !0              <errno.h> error-code.
 *
 */
int
accept_acl_add(
    feedtypet           ft,
    const char*         pattern,
    const regex_t*      rgxp,
    const host_set*     hsp,
    int                 isPrimary)
{
        return acl_add(&accept_acl, ft, pattern, rgxp, hsp, isPrimary);
}


/*
 * Checks the access-control-list (ACL) for a HIYA.
 *
 * @param *rmtip           Information on the remote host.
 * @param *offerd          The product-class that the remote host is offering to
 *                         send.
 * @return 0               if successful.
 * @return <errno.h>ENOMEM if out-of-memory.
 */
int
hiya_acl_ck(peer_info *rmtip, prod_class_t *offerd)
{
    int         error;

    if ((accept_acl == NULL) || offerd == NULL || offerd->psa.psa_len == 0) {
        /*
         * I don't understand why, if there are no REQUEST or ACCEPT entries in
         * the ACL, that the acceptable product-class wouldn't become the empty
         * set.  SRE 2002-11-07
         */
        error = ENOERR;
    }
    else {
        prod_class_t*     prodClass;
        int             isPrimary;

        error = acl_check_hiya(rmtip->name, inet_ntoa(rmtip->addr),
            offerd, &prodClass, &isPrimary);

        if (!error) {
            /*
             * I don't understand why an empty product-class set isn't a valid
             * value for rmtip->clssp.  SRE 2002-11-07
             */
            if (prodClass->psa.psa_len == 0) {
                free_prod_class(prodClass);
            }
            else {
                rmtip->clssp = prodClass;
            }
        }
    }

    return error;
}


/*
 * Determines the set of acceptable products given the upstream host and the
 * offered set of products.
 *
 * Arguments:
 *      name            Pointer to name of host.
 *      addr            Pointer to Internet address of host.
 *      dotAddr         Pointer to the dotted-quad form of the IP address of the
 *                      host.
 *      offered         Pointer to the class of offered products.
 *      accept          Address of pointer to set of acceptable products.
 *                      On success, the pointer will be set to reference
 *                      allocated space; otherwise, it won't be modified.
 *                      Acceptable product set may be empty. Invoke
 *                      free_prod_class(prod_class_t*) on non-NULL pointer when
 *                      done.  In general, the class of acceptable products will
 *                      be a subset of *offered.
 *      isPrimary       Pointer to flag indicating whether or not the
 *                      data-product exchange-mode should be primary (i.e., use
 *                      HEREIS) or alternate (i.e., use COMINGSOON/BLKDATA).
 * Returns:
 *      0               Success.
 *      ENOMEM          Failure.  Out-of-memory.
 */
int
acl_check_hiya(
    const char*         name,
    const char*         dotAddr, 
    prod_class_t*         offerd,
    prod_class_t**        accept,
    int*                isPrimary)
{
    int         error = 0;
    prod_class_t *prodClass;
    u_int       nhits = 0;
    aclt       *hits[MAXHITS];

    if (NULL != accept_acl) {
        aclt       *ap;

        /*
         * Find ACCEPT entries with matching identifiers.
         */
        for (ap = accept_acl; ap != NULL; ap = ap->next) {
            if (contains(ap->hsp, name, dotAddr)) {
                hits[nhits++] = ap;

                if (nhits >= MAXHITS) {
                        uerror("nhits (%u) >= MAXHITS (%d)",
                                nhits, MAXHITS);
                        break;
                }
            }
        }                               /* ACCEPT entries loop */
    }                                   /* ACCEPT list exists */

    prodClass = new_prod_class(nhits);  /* nhits may be 0 */

    if (prodClass == NULL) {
        error = ENOMEM;
    }
    else {
        int       ii;

        prodClass->from = offerd->from;
        prodClass->to = offerd->to;

        /*
         * Iterate over ACCEPT entries with matching identifiers.
         */
        for (ii = 0; ii < nhits; ii++) {
            int         jj;
            feedtypet   fi = NONE;
            char        s1[255];

            /*
             * Find first offered feedtype/pattern element whose feedtype
             * intersects the ACCEPT entry's.
             */
            for (jj = 0; jj < offerd->psa.psa_len; jj++) {
                fi = offerd->psa.psa_val[jj].feedtype & hits[ii]->ft;
                if (fi)
                    break;
            }

            prodClass->psa.psa_val[ii].feedtype = fi;

            if (ulogIsDebug())
                (void)sprint_feedtypet(s1, sizeof(s1), hits[ii]->ft);

            if (NONE == fi) {
                udebug("miss %s", s1);
            }
            else {
                char    s2[255], s3[255];

                if (ulogIsDebug()) {
                    (void)sprint_feedtypet(s2, sizeof(s2),
                        offerd->psa.psa_val[jj].feedtype);
                    (void)sprint_feedtypet(s3, sizeof(s3), fi);
                    udebug("hit %s = %s & %s", s3, s1, s2);
                    udebug("    %s was %s", hits[ii]->pattern,
                         offerd->psa.psa_val[jj].pattern);
                }

                prodClass->psa.psa_val[ii].pattern =
                    strdup((char *)hits[ii]->pattern);

                if (prodClass->psa.psa_val[ii].pattern == NULL)
                    error = ENOMEM;
            }                           /* offered feedtype intersects ACCEPT */

            if (error)
                break;
        }                               /* matching ACCEPT entries loop */

        if (error) {
            free_prod_class(prodClass);
            prodClass = NULL;
        }
        else {
            prodClass->psa.psa_len = (u_int)ii;

            clss_scrunch(prodClass);
            clss_regcomp(prodClass);
        }
    }                                   /* "prodClass" allocated */

    if (!error) {
        *accept = prodClass;
        *isPrimary = 1;                 /* always use primary mode for HIYA-s */
    }

    return error;
}


struct requester {
        pid_t                   pid;      
        const char*             source;
        unsigned                port;
        prod_class_t*             clssp;
        struct requester*       next;
        int                     isPrimary;
};
typedef struct requester requester;


/*
 * Ensures that the "from" time isn't too long ago.
 *
 * Arguments:
 *      from            Pointer to "from" time to be vetted.
 *      backoff         Maximum "backoff" time interval in seconds.
 */
static void
vetFromTime(
    timestampt* const   from,
    const int           backoff)
{
    timestampt                  defaultFrom;

    (void)set_timestamp(&defaultFrom);
    defaultFrom.tv_sec -= backoff;

    if (tvCmp(defaultFrom, *from, >))
        *from = defaultFrom;
}


/*
 * Extracts the metadata of a data-product.  Called by pq_sequence() in
 * restoreFromQueue().
 *
 * Arguments:
 *      infop           Pointer to data-product metadata.
 *      datap           Pointer to data-product.
 *      xprod           Pointer to XDR-encoded data-product.
 *      len             Length of XDR-encoded data-product.
 *      arg             Pointer completely-allocated prod_info structure for
 *                      copied data-product metadata.
 * Returns:
 *      PQ_END          Always.
 */
/*ARGUSED*/
static int
getInfo(
    const prod_info* const      infop,
    const void* const           datap,
    void* const                 xprod,
    const size_t                len,
    void* const                 arg)
{
    (void)pi_copy((prod_info*)arg, infop);

    return PQ_END;                      /* use first matching data-product */
}


/*
 * Returns the product-information of the last data-product in the
 * product-queue that matches the product-class.
 *
 * Calls exitIfDone() at potential termination points.
 *
 * Arguments:
 *      pq              Pointer to the product-queue structure.
 *      prodClass       Pointer to the request product-class.
 *      info            Pointer to product-information created by pi_new() or
 *                      pi_clone().
 * Returns:
 *      -1              Error.  log_*() called.
 *      0               Success.  "info" is set.
 *      1               No matching product-information exists.
 */
static int
getQueueProdInfo(
    pqueue* const               pq,
    const prod_class_t* const     prodClass,
    prod_info* const            info)
{
    int     status = -1;                /* error */

    assert(pq != NULL);
    assert(prodClass != NULL);
    assert(info != NULL);

    for (pq_cset(pq, &TS_ENDT);
        !(status = pq_sequence(pq, TV_LT, prodClass, getInfo, info));)
    {
        timestampt  cursor;

        (void)exitIfDone(0);

        pq_ctimestamp(pq, &cursor);

        if (d_diff_timestamp(&prodClass->from, &cursor) > interval)
            break;                      /* gone too far back */
    }

    if (status && PQ_END != status) {
        log_start("getQueueProdInfo(): %s", pq_strerror(pq, status));
    }
    else {
        status = 
            0 == status || tvIsNone(info->arrival)
                ? 1
                : 0;
    }

    return status;
}


/*
 * Initialize the "savedInfo" module with the product-information of the last
 * data-product in the product-queue that matches the product-class.  If no
 * such data-product exists in the product-queue, then a subsequent
 * savedInfo_get() call will return NULL.
 *
 * Calls exitIfDone() at potential termination points.
 *
 * Arguments:
 *      pq              Pointer to the product-queue structure.
 *      prodClass       Pointer to the product-class structure against which the
 *                      data-products will be matched.
 * Returns:
 *      NULL    Success.  savedInfo_get() will return the product-information
 *              of the last, matching data-product in the product-queue or NULL
 *              if no such data-product was found.
 *      else    Failure.
 */
static ErrorObj*
restoreFromQueue(
    pqueue* const               pq,
    const prod_class_t* const     prodClass)
{
    ErrorObj*           errObj = NULL;  /* success */
    prod_info* const    info = pi_new();

    if (NULL == info) {
        errObj = ERR_NEW1(errno, NULL,
            "Couldn't allocate new product-information structure: %s",
            strerror(errno));
    }
    else {
        int     errCode;

        for (pq_cset(pq, &TS_ENDT);
            !(errCode = pq_sequence(pq, TV_LT, prodClass, getInfo, info));)
        {
            timestampt  cursor;

            (void)exitIfDone(0);

            pq_ctimestamp(pq, &cursor);

            if (d_diff_timestamp(&prodClass->from, &cursor) > interval)
                break;
        }

        if (errCode && PQ_END != errCode) {
            errObj =
                ERR_NEW1(errCode, NULL,
                    "Couldn't get last matching data-product in product-queue: "
                        "%s",
                    pq_strerror(pq, errCode));
        }
        else {
            if (0 == errCode || tvIsNone(info->arrival)) {
                err_log_and_free(
                    ERR_NEW(errCode, NULL,
                        "No matching data-product in product-queue"),
                    ERR_INFO);
                (void)savedInfo_set(NULL);
            }
            else {
                if (errCode = savedInfo_set(info))
                    errObj = ERR_NEW1(errCode, NULL,
                        "Couldn't save product-information: %s",
                        strerror(errCode));
            }
        }

        pi_free(info);
    }                                   /* "info" allocated */

    return errObj;
}


static char     statePath[_POSIX_PATH_MAX];


/*
 * Returns the product-information of the last, successfully-received
 * data-product for a given data-request from the previous session.
 *
 * Arguments:
 *      upId            Identifier of upstream LDM host.
 *      port            Port number of upstream LDM host.
 *      prodClass       Requested product-class.
 *      info            Pointer to product-information structure created by
 *                      pi_new() or pi_clone();
 * Returns:
 *      -1              Error.  log_*() called.
 *      0               Success.  "*info" is set.
 *      1               No product-information available for the given request.
 */
static int
getPreviousProdInfo(
    const char* const           upId,
    const unsigned              port,
    const prod_class_t* const     prodClass,
    prod_info* const            info)

{
    int         status = -1;            /* failure */
    MD5_CTX*    context = new_MD5_CTX();

    if (context == NULL) {
        log_errno();
        log_add("getPreviousProdInfo(): Couldn't allocate MD5 structure");
    }
    else {
        /*
         * Create a filename based on a hash of the request.
         */
        unsigned char           hash[16];
        const prod_spec*        prodSpec = prodClass->psa.psa_val;
        FILE*                   file;

        MD5Update(context, (unsigned char*)upId, strlen(upId));
        MD5Update(context, (unsigned char*)&port, sizeof(port));

        for (; prodSpec < prodClass->psa.psa_val + prodClass->psa.psa_len;
                prodSpec++) {
            const feedtypet     feedtype = prodSpec->feedtype;

            if (feedtype != NONE) {
                const char* const       pattern = prodSpec->pattern;

                MD5Update(context, (unsigned char*)&feedtype,
                    sizeof(feedtypet));

                if (pattern != NULL) {
                    MD5Update(context, (unsigned char*)pattern,
                        strlen(pattern));
                }
            }
        }

        MD5Final(hash, context);
        (void)snprintf(statePath, sizeof(statePath)-1, ".%s.info", 
            s_signaturet(NULL, 0, hash));

        file = fopen(statePath, "r");

        if (file == NULL) {
            if (errno == ENOENT) {
                unotice("Previous product-information file \"%s\" "
                    "doesn't exist", statePath);
                status = 1;
            }
            else {
                log_errno();
                log_add("getPreviousProdInfo(): Couldn't open \"%s\"",
                    statePath);
            }
        }
        else {
            /*
             * The file is open.  Read in the information on the last,
             * successfully-received product.
             */
            int         c;
            char        comment[1];

            /*
             * Skip any comments.
             */
            while ((c = fgetc(file)) == '#')
                (void)fscanf(file, "%*[^\n]\n", comment);

            if (ferror(file)) {
                log_errno();
                log_add("getPreviousProdInfo(): Couldn't skip comments in "
                    "\"%s\"", statePath);
            }
            else {
                (void)ungetc(c, file);

                if (pi_scan(info, file) < 0) {
                    log_add("getPreviousProdInfo(): "
                        "Couldn't scan product-information in \"%s\"",
                        statePath);
                }
                else {
                    status = 0;
                }
            }                           /* comments skipped */

            (void)fclose(file);
        }                               /* "file" open */

        free_MD5_CTX(context);
    }                                   /* "context" allocated */

    return status;
}


/*
 * Saves information on the last, successfully-received product under a key
 * that comprises the relevant components of the data-request.
 */
void
savePreviousProdInfo(void)
{
    const prod_info*    info = savedInfo_get();

    if (info != NULL && statePath[0] != 0) {
        FILE*   file;
        char    tmpStatePath[_POSIX_PATH_MAX];

        (void)strcpy(tmpStatePath, statePath);
        (void)strcat(tmpStatePath, ".tmp");

        file = fopen(tmpStatePath, "w");

        if (file == NULL) {
            serror("savePreviousProdInfo(): Couldn't open \"%s\" for writing",
                tmpStatePath);
        }
        else {
            if (fputs(
"# The following is the product-information of the last,\n"
"# successfully-received data-product.  Do not modify it unless\n"
"# you know exactly what you're doing!\n", file) < 0) {
                log_errno();
                log_add("savePreviousProdInfo(): "
                    "Couldn't write comment to \"%s\"", tmpStatePath);
            }
            else {
                if (pi_print(info, file) < 0 || fputc('\n', file) == EOF) {
                    log_add("Couldn't write product-information to \"%s\"",
                        tmpStatePath);
                }
                else {
                    if (fclose(file) != 0) {
                        serror("savePreviousProdInfo(): Error closing \"%s\"",
                            tmpStatePath);
                    }
                    else if (rename(tmpStatePath, statePath) == -1) {
                        serror("savePreviousProdInfo(): Couldn't rename "
                            "\"%s\" to \"%s\"",
                            tmpStatePath, statePath);
                    }

                    file = NULL;
                }                       /* product-information written */
            }                           /* comment written */

            if (file != NULL) {
                (void)fclose(file);
                (void)unlink(tmpStatePath);
            }
        }                               /* "file" open */
    }                                   /* info != NULL && statePath[0] */
}


/*
 * Initializes the "savedInfo" module.
 *
 * Arguments:
 *      upId            Identifier of upstream LDM host.
 *      port            Port number of upstream LDM host.
 *      pqPath          Pathname of the product-queue.
 *      prodClass       Requested product-class.
 * Returns:
 *      -1              Error.  "log_*()" called.
 *      0               Success.
 */
static int
initSavedInfo(
    const char* const           upId,
    const unsigned              port,
    const char* const           pqPath,
    const prod_class_t* const     prodClass)
{
    int         status;
    prod_info*  info = pi_new();

    if (info == NULL) {
        log_errno();
        log_add("initSavedInfo(): "
            "Couldn't allocate product-information structure");
    }
    else {
        /*
         * Try getting product-information from the previous session.
         */
        status = getPreviousProdInfo(upId, port, prodClass, info);

        if (status == 1) {
            /*
             * There's no product-information from the previous session.
             * Try getting product-information from the most recent data-
             * product in the product-queue that matches the product-class.
             */
            pqueue*     pq;

            status = pq_open(pqPath, PQ_READONLY, &pq);

            if (status) {
                log_start("initSavedInfo(): Couldn't open product-queue "
                    "\"%s\" for reading: %s", pqPath, pq_strerror(pq, status));

                status = -1;
            }
            else {
                status = getQueueProdInfo(pq, prodClass, info);

                if (status == 1) {
                    pi_free(info);
                    info = NULL;
                    status = 0;
                }                       /* no matching data-product in queue */

                (void)pq_close(pq);
            }                           /* "pq" open */
        }                               /* no previous product-information */

        if (status == 0) {
            if (savedInfo_set(info)) {
                log_errno();
                log_add("initSavedInfo(): Couldn't set product-information");

                status = -1;
            }
        }

        pi_free(info);                  /* NULL safe */
    }                                   /* "info" allocated */

    return status;
}


/*
 * This function calls exit(): it does not return.
 *
 * Arguments:
 *      source          Pointer to the name of the upstream host.
 *      port            The port on which to connect.
 *      clssp           Pointer to the class of desired data-products.  The
 *                      caller may free upon return.
 *      isPrimary       Whether or not the initial transfer-mode should be
 *                      primary (uses HEREIS) or not (uses COMINGSOON/BLKDATA).
 *      hostCount       The number of hosts to which the same request will be
 *                      made.
 */
static void
prog_requester(
    const char*         source,
    const unsigned      port,
    prod_class_t*         clssp,
    int                 isPrimary,
    const unsigned      hostCount)
{
    int                 errCode = 0;    /* success */
    /*
     * Maximum silence, in seconds, from upstream LDM before checking
     * that it's still alive.  NOTE: Generally smaller than ldmd.c's
     * "inactive_timeo".  TODO: Make configurable.
     */
    unsigned int        maxSilence = 2 * interval;
    int                 backoffTime =
        toffset == TOFFSET_NONE
            ? max_latency
            : toffset;
    ErrorObj*           errObj;

    set_abbr_ident(source, NULL);
    str_setremote(source);

    /*
     * Set the "from" time in the data-class to the default value.
     */
    vetFromTime(&clssp->from, backoffTime);

    unotice("Starting Up(%s): %s:%u %s", ldm_version, source, port,
        s_prod_class(NULL, 0, clssp));

    (void)as_setLdmCount(hostCount);

    /*
     * Initialize the "savedInfo" module with the product-information
     * of the last, successfully-received data-product.
     *
     * NB: Potentially lengthy and CPU-intensive.
     */
    if (initSavedInfo(source, port, pqfname, clssp) != 0) {
        log_add("prog_requester(): "
            "Couldn't initialize saved product-information module");
        log_log(LOG_ERR);

        errCode = EXIT_FAILURE;
    }
    else {
        (void)exitIfDone(0);

        /*
         * Open the product-queue for writing.  It will be closed by
         * cleanup() at process termination.
         */
        errCode = pq_open(pqfname, PQ_DEFAULT, &pq);
        if (errCode) {
            err_log_and_free(
                ERR_NEW2(errCode, NULL,
                    "Couldn't open product-queue \"%s\" for writing: %s",
                    pqfname, pq_strerror(pq, errCode)),
                ERR_FAILURE);

            errCode = EXIT_FAILURE;
        }
        else while (!errCode && exitIfDone(0)) {
            static unsigned sleepAmount = 0;

            /*
             * Ensure that the "from" time in the data-class isn't too
             * long ago.
             */
            vetFromTime(&clssp->from, backoffTime);

            savedInfo_reset();

            /*
             * Try LDM version 6.
             */
            errObj = req6_new(source, port, clssp, maxSilence, pqfname, pq,
                isPrimary);

            exitIfDone(0);

            if (!errObj) {
                errCode = 0;

                /*
                 * as_shouldSwitch() must be true.  Switch data-product
                 * transfer-mode.
                 */
                err_log_and_free(
                    ERR_NEW1(0, NULL,
                        "Switching data-product transfer-mode to %s",
                            isPrimary
                                ? "alternate"
                                : "primary"),
                    ERR_NOTICE);

                isPrimary = !isPrimary;
                sleepAmount = 0;/* immediate retry */
            }                       /* req6_new() success */
            else {
                errCode = err_code(errObj);

                if (errCode == REQ6_TIMED_OUT) {
                    log_log(LOG_NOTICE);
                    err_log(errObj, ERR_NOTICE);
                    errCode = 0;
                    sleepAmount = 0;
                }
                else if (errCode == REQ6_SYSTEM_ERROR) {
                    log_log(LOG_ERR);
                    errObj = ERR_NEW(0, errObj,
                        "Terminating due to system failure");
                    err_log(errObj, ERR_FAILURE);
                    errCode = EXIT_FAILURE;
                }
                else if (errCode != REQ6_BAD_VERSION) {
                    log_log(LOG_ERR);
                    errObj = ERR_NEW(0, errObj,
                        "Disconnecting due to LDM failure");
                    err_log(errObj, ERR_FAILURE);
                    errCode = 0;
                }
                else {
                    log_log(LOG_NOTICE);
                    err_log(errObj, ERR_NOTICE);
                    free_prod_class(remote.clssp);

                    if ((remote.clssp = dup_prod_class(clssp)) == NULL) {
                        err_log_and_free(
                            ERR_NEW1(0, NULL,
                                "Couldn't duplicate product-class: %s",
                                strerror(errno)),
                            ERR_FAILURE);

                        errCode = EXIT_FAILURE;
                    }
                    else {
                        /*
                         * Try LDM version 5.
                         */
                        errCode = forn5(FEEDME, source, &remote.clssp,
                            rpctimeo, inactive_timeo, ldmprog_5);

                        udebug("forn5(...) = %d", errCode);

                        if (errCode == ECONNABORTED) {
                            unotice("Connection aborted");
                            errCode = 0;
                        }
                        else if (errCode == ECONNRESET) {
                            unotice("Connection closed by upstream LDM");
                            errCode = 0;
                        }
                        else if (errCode == ETIMEDOUT) {
                            unotice("Connection timed-out");
                            sleepAmount = 0;
                            errCode = 0;
                        }
                        else if (errCode == ECONNREFUSED) {
                            unotice("Connection refused by upstream LDM");
                            errCode = 0;
                        }
                        else if (errCode != 0){
                            uerror("Unexpected forn5() return: %d",
                                errCode);

                            errCode = EXIT_FAILURE;
                        }
                    }               /* remote.clssp set */
                }                   /* LDM-6 protocol not supported */

                log_clear();
                err_free(errObj);
            }                       /* req6_new() error */

            if (!errCode && exitIfDone(0)) {
                /*
                 * Pause before reconnecting if appropriate.
                 */
                if (savedInfo_wasSet()) {
                    savedInfo_reset();
                    sleepAmount = 0;    /* immediate retry */
                }
                if (0 == sleepAmount) {
                    sleepAmount = 1;    /* for next time */
                }
                else {
                    uinfo("Sleeping %d seconds before retrying...",
                        sleepAmount);
                    (void)sleep(sleepAmount);

                    /*
                     * Close any connection to the network host database
                     * so that any name resolution starts from scratch next
                     * time.  This allows DNS updates to have an effect on
                     * a running downstream LDM.
                     */
                    endhostent();

                    /*
                     * Golden ratio backoff for next time.
                     */
                    sleepAmount = (int)(1.618034 * sleepAmount + 0.5);
                }
                if (sleepAmount > interval)
                    sleepAmount = interval;
            }                           /* no error and not done */
        }                               /* connection loop */
    }                                   /* savedInfo module initialized */
    
    exit(errCode);
}


/*
 * Arguments:
 *      hostId          Pointer to identifier of upstream host.
 *      port            The port on which to connect.
 *      clssp           Pointer to desired class of products.  Caller may free
 *                      upon return.
 *      isPrimary       Whether or not the data-product exchange-mode should be
 *                      primary (i.e., use HEREIS) or alternate (i.e., use
 *                      COMINGSOON/BLKDATA).
 *      hostCount       The number of hosts to which the same request will be
 *                      made.
 * Returns:
 *      -1              Failure.  errno is set.
 *      else            Success.
 */
static pid_t
run_requester(
    const char*         hostId,
    const unsigned      port,
    prod_class_t*         clssp,
    const int           isPrimary,
    const unsigned      hostCount)
{
        pid_t pid = fork();
        if(pid == -1)
        {
                serror("run_requester: fork");
                return -1;
        }

        if(pid == 0)
        {
                /* child */
                endpriv();
                prog_requester(hostId, port, clssp, isPrimary, hostCount);
                /*NOTREACHED*/
        }

        /* else, parent */
        return pid;
}


/*
 * Allocates and initializes a requester structure.
 *
 * Arguments:
 *      remoteLocation  Pointer to remote location to which to connect.  Caller
 *                      may free on return.  Must not be NULL.
 *      clssp           Pointer to desired class of data-products.  Caller may
 *                      free upon return.  Must not be NULL.
 *      isPrimary       Whether or not the data-product exchange-mode should be
 *                      primary (i.e., use HEREIS) or alternate (i.e., use
 *                      COMINGSOON/BLKDATA).
 *      hostCount       The number of hosts to which the same request will be
 *                      made.
 * Returns:
 *      NULL            Failure.  errno is set.
 *      else            Pointer to initialized requester structure.  The
 *                      associated requester is executing.
 */
static requester*
new_requester(
    const RemoteLocation*       remoteLocation,
    prod_class_t*                 clssp,
    const int                   isPrimary,
    const unsigned              hostCount)
{
    requester*  reqstrp = (requester*)malloc(sizeof(requester));

    assert(remoteLocation != NULL);
    assert(clssp != NULL);

    if (reqstrp != NULL) {
        int     error = 0;              /* success */

        (void)memset(reqstrp, 0, sizeof(requester));

        reqstrp->clssp = NULL;
        reqstrp->pid = -1;
        reqstrp->isPrimary = isPrimary;
        reqstrp->source = strdup(remoteLocation->hostId);

        if (reqstrp->source == NULL) {
            error = 1;
        }
        else {
            reqstrp->port = remoteLocation->port;
            reqstrp->clssp = clssp;
            reqstrp->pid =
                run_requester(reqstrp->source, reqstrp->port, reqstrp->clssp,
                    isPrimary, hostCount);
        }                               /* "reqstrp->source" allocated */

        if (error) {
            free(reqstrp);
            reqstrp = NULL;
        }
    }                                   /* "reqstrp" allocated */

    return reqstrp;
}


static requester *requesters;

/*
 * Creates a new requester and adds it to the list of requesters.  The new
 * requester is executing
 *
 * Arguments:
 *      remoteLocation  Pointer to remote location to which to connect.  Caller
 *                      may free on return.  Must not be NULL.
 *      clssp           Pointer to desired class of data-products.  Caller may
 *                      free upon return.  Must not be NULL.
 *      isPrimary       Whether or not the data-product exchange-mode should be
 *                      primary (i.e., use HEREIS) or alternate (i.e., use
 *                      COMINGSOON/BLKDATA).
 *      hostCount       The number of hosts to which the same request will be
 *                      made.
 * Returns:
 *      0               Success.
 *      else            <errno.h> error-code.
 */
static int
requester_add(
    const RemoteLocation*       remoteLocation,
    prod_class_t*                 clssp,
    const int                   isPrimary,
    unsigned                    hostCount)
{
    int         error = 0;              /* success */
    requester*  reqstrp =
        new_requester(remoteLocation, clssp, isPrimary, hostCount);

    if(reqstrp == NULL) {
        error = errno;
    }
    else {
        if (requesters == NULL) {
            requesters = reqstrp;
        }
        else {
            requester*  ep = requesters;

            while(ep->next != NULL)
                ep = ep->next;

            ep->next = reqstrp;
        }
    }                                   /* "reqstrp" allocated */

    return error;
}

int
invert_request_acl(
    unsigned    ldmPort)
{
    int status = ENOERR;

    if (requestEntryHead != NULL) {
        RequestEntry*   entry;

        for (entry = requestEntryHead; entry != NULL; entry = entry->next) {
            unsigned    hostIndex;

            for (hostIndex = 0; hostIndex < entry->hostCount; hostIndex++) {
                prod_class_t*             clssp;
                const RemoteLocation*   remoteLocation =
                    entry->remoteLocations + hostIndex;

                clssp = new_prod_class(1);

                if (clssp == NULL) {
                    status = errno;
                }
                else {
                    prod_spec*  sp;

                    clssp->from = TS_ZERO;      /* prog_requester() adjusts */
                    clssp->to = TS_ENDT;

                    sp = clssp->psa.psa_val;
                    sp->feedtype = entry->ft;
                    sp->pattern = strdup(entry->pattern);

                    if (sp->pattern == NULL) {
                        status = errno;
                    }
                    else {
                        (void)re_vetSpec(sp->pattern);

                        if (regcomp(&sp->rgx, sp->pattern,
                                REG_EXTENDED|REG_NOSUB) != 0) {
                            status = EINVAL;
                        }   
                        else {
                            status =
                                requester_add(remoteLocation, clssp,
                                    hostIndex == 0, entry->hostCount);
                        }
                    }                       /* "sp->pattern" allocated */

                    free_prod_class(clssp);

                    if (status)
                        break;
                }                       /* "clssp" allocated */
            }                           /* host loop */
        }                               /* request-entry loop */
#if 0   /* DEBUG */
        {
            char buf[1984];
            requester *reqstrp;
    
            for (reqstrp = requesters; reqstrp != NULL; reqstrp = reqstrp->next)
            {
                unotice("%s: %s",
                     reqstrp->source,
                     s_prod_class(buf, sizeof(buf), reqstrp->clssp));
            }
        }
#endif
    }                                   /* non-NULL request-list */

    return status;
}

/** EXEC */

struct proct {
        pid_t pid;
        wordexp_t wrdexp;
        struct proct *next;
};
typedef struct proct proct ;


static void
free_proct(proct *proc)
{
        if(proc == NULL)
                return;
        if(proc->wrdexp.we_wordc)
        {
                wordfree(&proc->wrdexp);
                if(proc->pid > 0)
                {
                  /*EMPTY*/
                  /* TODO */ ;
                }
        }
        free(proc);
}


static int
close_rest(int bottom)
{
        static long open_max = 0; /* number of descriptors */
        int ii;
        if(!open_max)
        {
#ifdef _SC_OPEN_MAX
                open_max = sysconf(_SC_OPEN_MAX);
#else
                open_max = 32 ; /* punt */
#endif
        }
        for(ii = bottom; (long)ii < open_max ; ii++)
                (void)close(ii) ;
        return ii ;
}


static int
exec_proct(proct *proc)
{
        assert(proc->pid == -1);
        assert(proc->wrdexp.we_wordv[proc->wrdexp.we_wordc] == NULL);

        proc->pid = fork() ;
        if(proc->pid == -1)
        {       /* failure */
                serror("fork") ;
                return -1;
        }
        /* else */

        if(proc->pid == 0)
        {       /* child */

                /* restore signals */
                {
                        struct sigaction sigact;
                
                        (void)sigemptyset(&sigact.sa_mask);
                        sigact.sa_flags = 0;
                        sigact.sa_handler = SIG_DFL;
                        
                        (void) sigaction(SIGPIPE, &sigact, NULL);
                        (void) sigaction(SIGHUP, &sigact, NULL);
                        (void) sigaction(SIGUSR1, &sigact, NULL);
                        (void) sigaction(SIGUSR2, &sigact, NULL);
                        (void) sigaction(SIGCHLD, &sigact, NULL);
                        (void) sigaction(SIGALRM, &sigact, NULL);
                        (void) sigaction(SIGINT, &sigact, NULL);
                        (void) sigaction(SIGTERM, &sigact, NULL);
                }

                /* Set up fd 0,1 */
                (void)close(0);
                {
                        int fd = open("/dev/null", O_RDONLY);
                        if(fd > 0)
                        {
                                (void) dup2(fd, 0);
                                (void) close(fd);
                        }
                }
                (void)close(1);
                {
                        int fd = open("/dev/console", O_WRONLY);
                        if(fd < 0)
                                fd = open("/dev/null", O_WRONLY);
                        if(fd > 1)
                        {
                                (void) dup2(fd, 1);
                                (void) close(fd);
                        }
                }
                if(logfname == NULL)
                {
                        (void)close(2);
                        {
                                int fd = open("/dev/console", O_WRONLY);
                                if(fd < 0)
                                        fd = open("/dev/null", O_WRONLY);
                                if(fd > 2)
                                {
                                        (void) dup2(fd, 2);
                                        (void) close(fd);
                                }
                        }
                } /* else, the logFd is stderr, and that's ok */
                close_rest(3);
                endpriv(); 
                (void) execvp(proc->wrdexp.we_wordv[0], proc->wrdexp.we_wordv);
                serror("execvp: %s", proc->wrdexp.we_wordv[0]) ;
                _exit(127) ;
        }
        /* else, parent */

        return (int)proc->pid;
}


static proct *
new_proct(
    wordexp_t*  wrdexpp)
{
    proct *proc = (proct*)malloc(sizeof(proct));        

    if (proc != NULL) {
        proc->wrdexp = *wrdexpp;
        proc->pid = -1;
        proc->next = NULL;

        if (exec_proct(proc) < 0) {
            free_proct(proc);
            proc = NULL;
        }
    }

    return proc;
}

static proct *procs;

/*
 * On Success:
 *      Copies *wrdexpp structure and accepts responsibility for calling 
 *      wordfree() on copy;
 */
int
exec_add(wordexp_t *wrdexpp)
{
        proct *procp = new_proct(wrdexpp);
        if(procp == NULL)
                return errno;

        if(procs == NULL)
        {
                procs = procp;
        }
        else
        {
                proct *ep = procs;
                while (ep->next != NULL)
                    ep = ep->next;
                ep->next = procp;
        }
        return ENOERR;
}


/*
 * Frees an entry in the child process list.
 *
 * Arguments:
 *      pid     The process identifier of the child process whose entry is
 *              to be freed.
 */
void
exec_free(
    const pid_t pid)
{
    if (procs != NULL) {
        proct** prev = &procs;
        proct*  entry;

        for (entry = *prev; entry != NULL; entry = *prev) {
            if (entry->pid == pid)
                break;

            prev = &entry->next;
        }

        if (entry != NULL) {
            *prev = entry->next;

            free_proct(entry);
        }
    }
}


/*
 * Returns the command-line of a child process.
 *
 * Arguments:
 *      pid     The process identifier of the child process.
 *      buf     The buffer into which to write the command-line.
 *      size    The size of "buf".
 * Returns:
 *      -2      The child process wasn't found.
 *      -1      Write error.  See errno.  Error-message(s) written via
 *              log_*().
 *      else    The number of characters written into "buf" excluding any
 *              terminating NUL.  If the number of characters equals "size",
 *              then no terminating NUL was written.
 */
int
exec_getCommandLine(
    const pid_t         pid,
    char* const         buf,
    size_t              size)
{
    int                 status;
    const proct*        ep;

    for (ep = procs; ep != NULL; ep = ep->next) {
        if (ep->pid == pid)
            break;
    }

    if (ep == NULL) {
        status = -2;
    }
    else {
        const wordexp_t*        wrdexpp = &ep->wrdexp;
        char*                   cp = buf;
        size_t                  i;

        status = 0;

        for (i = 0; i < wrdexpp->we_wordc; i++) {
            int n = snprintf(cp, size, i == 0 ? "%s" : " %s",
                wrdexpp->we_wordv[i]);

            if (n >= 0) {
                cp += n;
                size -= n;
            }
            else {
                log_start("%s", strerror(errno));
                log_add("Couldn't write command-line word %lu", (unsigned)i);

                status = -1;
                break;
            }
        }

        if (status != -1)
            status = cp - buf;
    }
    
    return status;
}

/****/

/*
 * (weak) First line of defense:
 * return nonzero if we are willing to
 * do business with this host, zero otherwise.
 *
 * Of course, a serious threat would spoof the IP address or
 * name service.
 */
int
host_ok(const peer_info *rmtip)
{
        /* TODO: Does this need to be more efficient ? */
        aclt*           acceptEntry;
        AllowEntry*     allowEntry;

        for (allowEntry = allowEntryHead;
            allowEntry != NULL;
            allowEntry = allowEntry->next)
        {
                if(host_set_match(rmtip, allowEntry->hsp))
                        return 1;
        }
        for (acceptEntry = accept_acl;
            acceptEntry != NULL;
            acceptEntry = acceptEntry->next)
        {
                if(host_set_match(rmtip, acceptEntry->hsp))
                        return 1;
        }
        return 0;
}
