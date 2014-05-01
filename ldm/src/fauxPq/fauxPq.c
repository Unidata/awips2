/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: fauxPq.c,v 1.7.18.1 2008/04/15 16:34:07 steve Exp $ */


/*
 * A "faux" product queue.  This module presents the public Product Queue API,
 * but it returns faux products and accepts everything.  The products returned
 * by this implementation are configurable via the "product queue" file, which
 * is a configuration file that describes the behavior of this module.
 */


#include <ldmconfig.h>

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ldm_xlen.h"
#include "pq.h"
#include "ulog.h"

#include "fauxPqConfigFile.h"


/*
 * A value which is an invalid off_t
 */
#define OFF_NONE  ((off_t)(-1))


/*
 * The product queue structure (private to this module).
 */
struct pqueue {
    timestampt cursor;  /* current position in queue */
    int         isOpen;
};


const pqe_index _pqenone = {
    OFF_NONE,
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
};


/* 
 * Creates a product queue.
 *
 * This implementation just checks the arguments and returns EACCES.
 *
 * path              The pathname of the product queue. (in)
 * mode              The mode in which to create the product queue. (in)
 * pflags            Unknown.  (in)
 * align             The alignment of products in bytes or zero for the 
 *                   default. (in)
 * initialsz         The initial size of the product queue. (in)
 * nproducts         The anticipated number of products for the queue. (in)
 * pqp               The product queue. (out)
 *
 * Returns:
 *   errno.h:EACCES  if permission is denied.
 *   errno.h:EINVAL  if the pathname or queue argument is NULL.
 */
/*ARGSUSED1*/
int pq_create(
    const char* path,
    mode_t      mode,
    int         pflags,
    size_t      align,
    off_t       initialsz,
    size_t      nproducts,
    pqueue**    pqp)
{
    int         status;

    if (path == NULL || pqp == NULL) {
        status = EINVAL;
    }
    else {
        status = EACCES;
    }

    return status;
}


/* 
 * Opens an existing product queue.  The pathname references a configuration
 * file that describes the behavior of this module regarding the virtual
 * product queue.  The memory allocated to the returned queue stucture should
 * be freed via pq_close(pqueue*).
 *
 * path              The pathname of the configuration file. (in)
 * pflags            Unknown.  (in)
 * pqp               The product queue. (out)
 *
 * Returns:
 *   pq.h:ENOERR     if success.
 *   errno.h:ENOMEM  if out of memory.
 *   errno.h:EINVAL  if the pathname or queue argument is NULL.
 *   errno.h:EBADF   if the configuration file couldn't be opened.
 */
/*ARGSUSED1*/
int pq_open(
    const char* path,
    int         pflags,
    pqueue**    pqp)
{
    int         status;

    if (path == NULL || pqp == NULL) {
        status = EINVAL;
    }
    else {
        pqueue*   pq = (pqueue*)malloc(sizeof(pqueue));

        if (pq == NULL) {
            status = ENOMEM;
        }
        else {
            status = cfOpen(path);

            if (status == ENOERR) {
                pq->isOpen = 1;
                *pqp = pq;
            }
        }
    }

    return status;
}


/*
 * Closes an open product queue and frees any allocated resources.  Do not use
 * the product queue structure after this method.
 *
 * pq               The product queue to be closed. (in/out)
 *
 * Returns:
 *   pq.h:ENOERR    if success.
 *   errno.h:EINVAL if the queue argument is NULL or the queue has already been
 *                  closed.
 */
int pq_close(pqueue *pq)
{
    int       status;

    if (pq == NULL || !pq->isOpen) {
        status = EINVAL;
    }
    else {
        pq->isOpen = 0;
        free((void*)pq);
        cfClose();
        status = ENOERR;
    }

    return status;
}


/*
 * Returns the page size of the product queue.
 *
 * This implementation always returns 4096.
 *
 * pq               The product queue whose page size is to be returned.
 *                  (in)
 *
 * Returns          The product queue page size.
 */
/*ARGSUSED0*/
int pq_pagesize(const pqueue *pq)
{
    return 4096;
}


/*
 * Ensures that a produce queue is open.
 *
 * pq                The product queue.
 *
 * Returns:
 *   pq.h:ENOERR     if the queue is open.
 *   errno.h:EINVAL  if the queue is NULL or not open.
 */
static int checkOpen(pqueue* pq)
{
    return
        (pq == NULL)
            ? EINVAL
            : ((pq->isOpen)
                ? ENOERR
                : EINVAL);
}


/*
 * Reserves space in the product queue for a product.  This function is used
 * when a product is not yet assem- bled but we wish to allocate storage in
 * the queue for its assembly, such as upon the receipt of a COMINGSOON remote
 * procedure call in the server.  It returns storage in *ptrp suitable for
 * placing the data of the product described by infop and product_size.  The
 * value of *indexp should be retained for use in committing the product using
 * pqe_insert() or abandoning it using pqe_discard().  Calls to this function
 * for products whose signature is already in the queue fail with an error
 * indication of PQUEUE_DUP.
 *
 * This implementation just checks the arguments and returns allocated space 
 * for the product.
 *
 * pq                The product queue. (in)
 * infop             Information on the product. (in)
 * ptrp              Where to copy the data portion of the product. Managed by
 *                   this module. (out)
 * indexp            The index of the product for use by pqe_insert() or
 *                   pqe_discard(). (out)
 *
 * Returns:
 *   pq.h:ENOERR     if success.
 *   pq.h:PQUEUE_DUP if a product with same signature is already in the queue.
 *   errno.h:ENOMEM  if out of memory.
 *   errno.h:EINVAL  if any argument is NULL or the queue is not open.
 */
int pqe_new(
    pqueue*          pq,
    const prod_info* infop,
    void**           ptrp,
    pqe_index*       indexp)
{
    int              status = checkOpen(pq);

    if (status == ENOERR) {
        if (infop == NULL || ptrp == NULL || indexp == NULL) {
            status = EINVAL;
        }
        else {
            void*            ptr = malloc(infop->sz);

            if (ptr == NULL) {
                status = ENOMEM;
            }
            else {
                *ptrp = ptr;
                indexp->offset = (off_t)ptr;
                (void)memcpy(indexp->signature, infop->signature, 
                    sizeof(infop->signature));
                status = ENOERR;
            }
        }
    }

    return status;
}


/*
 * Abandons the saving of a product which was begun using pqe_new() and
 * releases any allocated resources.
 *
 * pq                The product queue. (in/out)
 * index             The index of the product begun by pqe_new(...). (in)
 *
 * Returns:
 *   pq.h:ENOERR     if success.
 *   errno.h:EINVAL  if the queue is NULL or not open.
 */
int pqe_discard(
    pqueue*   pq,
    pqe_index index)
{
    int status = checkOpen(pq);

    if (status == ENOERR) {
        if(!pqeIsNone(index))
            free((void*)index.offset);
    }

    return status;
}


/*
 * Inserts a completed product which was begun using pqe_new() and releases
 * any allocated resources.
 *
 * pq                The product queue. (in/out)
 * index             The index of the product begun by pqe_new(...). (in)
 *
 * Returns:
 *   pq.h:ENOERR     if success.
 *   errno.h:EINVAL  if the queue is NULL or not open.
 */
int pqe_insert(
    pqueue*   pq,
    pqe_index index)
{
    return pqe_discard(pq, index);
}


/*
 * Inserts a product into the queue.
 *
 * pq                The product queue. (in/out)
 * prod              The product. (in)
 *
 * Returns:
 *   pq.h:ENOERR     if success.
 *   errno.h:EINVAL  if the queue is NULL or not open.
 */
/*ARGSUSED1*/
int pq_insert(
    pqueue*        pq,
    const product* prod)
{
    assert(pq != NULL);

    return checkOpen(pq);
}


/*
 * Returns some useful statistics.
 *
 * pq                The product queue. (in/out)
 * highwaterp        Maximum size of data segment.  Set to 0. (out)
 * maxproductsp      Maximum number of products in queue.  Set to 0. (out)
 *
 * Returns:
 *   pq.h:ENOERR    if success.
 *   errno.h:EINVAL if the queue argument is NULL.
 */
int pq_highwater(
    pqueue* pq,
    off_t*  highwaterp,
    size_t* maxproductsp)
{
    int     status;

    if (pq == NULL || highwaterp == NULL || maxproductsp == NULL) {
        status = EINVAL;
    }
    else {
        *highwaterp = 0;
        *maxproductsp = 0;
    }

    return status;
}



/*
 * Sets the cursor used by pq_sequence() or pq_seqdel().
 *
 * pq               The product queue. (in/out)
 * tvp              The time cursor. (in)
 *
 * Raises:
 *   SIGSEGV        If the queue argument is NULL.
 */
void pq_cset(
    pqueue*           pq,
    const timestampt* tvp)
{
    pq->cursor = *tvp;
}


/*
 * Gets the current cursor value used by pq_sequence() or pq_seqdel().
 *
 * pq               The product queue. (in)
 * tvp              The time cursor. (out)
 *
 * Returns:
 *   pq.h:ENOERR    if success.
 *
 * Raises
 *   SIGSEGV        if either argument is NULL.
 */
void pq_ctimestamp(
    const pqueue* pq,
    timestampt*  tvp)
{
    *tvp = pq->cursor;
}


/*
 * Steps thru the time-sorted inventory of products according to 'mt', and
 * the current cursor value.  If no product in the inventory matches the
 * specification, then PQUEUE_END is returned; otherwise, if a matching
 * product is found, then ifMatch(xprod, len, otherargs) is called and the
 * return value of that function is returned.
 *
 * Whether or not a matching product is found depends upon the configuration
 * file.
 *
 * pq               The product queue.  (in)
 * mt               The time-matching criteria for finding a product. (in)
 * clss             The class of products to find. (in)
 * ifMatch          The function to call if a matching product is found. (in)
 * otherargs        The last argument to (*ifMatch).  (unknown)
 *
 * Returns:
 *   pq.h:PQUEUE_END  if no matching product was found.
 *   errno.h:EINVAL   if the queue is NULL or not open.
 *   (else)           <errno.h> error code.
 */
int pq_sequence(
    pqueue*           pq,
    pq_match          mt,
    const prod_class_t* clss,
    pq_seqfunc*       ifMatch,
    void*             otherargs)
{
    int              status = checkOpen(pq);

    if (status == ENOERR) {
        prod_info   info;   /* the product metadata */
        void*       datap;  /* the XDR-decoded product-data */
                            /* (excludes metadata) */
        void*       xprodp; /* the corresponding, XDR-encoded product */
                            /* (includes metadata) */
        size_t      len;    /* the size of xprodp in bytes */

        status = cfGetProduct(mt, clss, &info, &datap, &xprodp, &len);

        if (status == ENOERR) {
            status = (*ifMatch)(&info, datap, xprodp, len, otherargs);

            cfFreeProduct(&info, &datap, &xprodp);
        }
    }

    return status;
}


/*
 * Like pq_sequence(), but the action is to remove the product from the product
 * queue.  If no matching product is found, then PQUEUE_END is returned and
 * *extentp and *timestampp are unmodified.
 *
 * pq               The product queue.  (in/out)
 * mt               The sequencing behavior for finding a product. (in)
 * clss             The class of products to find. (in)
 * wait             Unknown. (in)
 * extentp          The amount of storage, in bytes, made available by the
 *                  removal. (out)
 * timestampp       The insertion time of the removed product. (out)
 *
 * Returns:
 *   pq.h:PQUEUE_END if no matching product was found.
 *   errno.h:EINVAL  if the queue is NULL or not open.
 */
/*ARGSUSED*/
int pq_seqdel(
    pqueue*           pq,
    pq_match          mt,
    const prod_class_t* clss,
    int               wait,
    size_t*           extentp,
    timestampt*       timestampp) 
{
    return checkOpen(pq);
}


static void hndlr_noop(int sig)
{
#ifndef NDEBUG
        switch(sig) {
        case SIGALRM :
                udebug("SIGALRM") ;
                return ;
        case SIGCONT :
                udebug("SIGCONT") ;
                return;
        }
        udebug("hndlr_noop: unhandled signal: %d", sig) ;
#endif
        /* nothing to do, just wake up */
        return;
}


/*
 * Suspends the current thread until one of the following events occurs:
 *
 *   A signal is received that has a non-default handler.
 *   A SIGCONT is received, indicating that new data is available in the queue.
 *   The specified time elapses.
 *
 * maxsleep          The amount of time to sleep in seconds.  If zero, then
 *                   the sleep is indefinite.
 *
 * Returns:
 *   0               Success.
 */
int
pq_suspend(unsigned int maxsleep)
{
        struct sigaction sigact, csavact, asavact;
        sigset_t mask, savmask;

        /* block CONT and ALRM while we set up */
        (void)sigemptyset(&mask);
        (void)sigaddset(&mask, SIGCONT);
        if(maxsleep)
                (void)sigaddset(&mask, SIGALRM);
        (void) sigprocmask(SIG_BLOCK, &mask, &savmask);

        /*
         * Set up handlers for CONT and ALRM, stashing old
         */
        (void)sigemptyset(&sigact.sa_mask);
        sigact.sa_flags = 0;
        sigact.sa_handler = hndlr_noop;
        (void) sigaction(SIGCONT, &sigact, &csavact);
        if(maxsleep)
        {
                /* set the alarm */
                (void) sigaction(SIGALRM, &sigact, &asavact);
                (void) alarm(maxsleep);
        }
        
        /*
         * Unblock the signals.
         */
        mask = savmask;
        (void)sigdelset(&mask, SIGCONT);
        if(maxsleep)
                (void)sigdelset(&mask, SIGALRM);

        /* Nighty night... */
        (void) sigsuspend(&mask);

        /* Now we are back, restore state */
        if(maxsleep)
        {
                (void)alarm(0);
                (void) sigaction(SIGALRM, &asavact, NULL );
        }
        (void) sigaction(SIGCONT, &csavact, NULL );
        
        (void) sigprocmask(SIG_SETMASK, &savmask, NULL);

        return 0;
}


/*
 * Get some detailed product queue statistics.  These may be useful for
 * monitoring the internal state of the product queue:
 *
 *   nprodsp
 *         holds the current number of products in the queue.
 *   nfreep
 *         holds the current number of free regions.  This should be small
 *         and it's OK if it's zero, since new free regions are created
 *         as needed by deleting oldest products.  If this gets large,
 *         insertion and deletion take longer.
 *   nemptyp
 *         holds the number of product slots left.  This may decrease, but
 *         should eventually stay above some positive value unless too
 *         few product slots were allocated when the queue was
 *         created.  New product slots get created when adjacent free
 *         regions are consolidated, and product slots get consumed
 *         when larger free regions are split into smaller free
 *         regions.
 *   nbytesp
 *         holds the current number of bytes in the queue used for data
 *         products.
 *   maxprodsp
 *         holds the maximum number of products in the queue, so far.
 *   maxfreep
 *         holds the maximum number of free regions, so far.
 *   minemptyp
 *         holds the minimum number of empty product slots, so far.
 *   maxbytesp
 *         holds the maximum number of bytes used for data, so far.
 *   age_oldestp
 *         holds the age in seconds of the oldest product in the queue.
 *   maxextentp
 *         holds extent of largest free region
 *
 * Returns 0
 */
/*ARGSUSED0*/
int pq_stats(
    pqueue *pq,
    size_t *nprodsp,
    size_t *nfreep,
    size_t *nemptyp,
    size_t *nbytesp,
    size_t *maxprodsp,
    size_t *maxfreep,
    size_t *minemptyp,
    size_t *maxbytesp, 
    double *age_oldestp,
    size_t *maxextentp)
{
    if(nprodsp)
        *nprodsp = 0;
    if(nfreep)
        *nfreep = 0;
    if(maxextentp)
        *maxextentp = 0;
    if(nemptyp)
        *nemptyp = 0;
    if(nbytesp)
        *nbytesp = 0;
    if(maxprodsp)
        *maxprodsp = 0;
    if(maxfreep)
        *maxfreep = 0;
    if(minemptyp)
        *minemptyp = 0;
    if(maxbytesp)
        *maxbytesp = 0;
    if(age_oldestp) {
        *age_oldestp = 0;
    }

    return 0;
}


/*
 * For debugging: dump extents of regions on free list, in order by extent.
 */
/*ARGSUSED0*/
int pq_fext_dump(pqueue *const pq)
{
    return ENOERR;
}


/*ARGSUSED0*/
int pq_last(
    pqueue*           pq,
    const prod_class_t* clssp,
    timestampt*       tsp) /* modified upon return */
{
    int status = set_timestamp(tsp);

    if (status  != 0) {
        uerror("%s:%d: Couldn't set time: %s", __FILE__,
            __LINE__, strerror(errno));
    }

    return status;
}


/*
 * Insert at rear of queue
 * (Don't signal process group.)
 */
/*ARGSUSED0*/
int pq_insertNoSig(pqueue *pq, const product *prod)
{
    return ENOERR;
}


/*ARGSUSED0*/
int pq_ctimeck(
    const pqueue*     pq,
    pq_match          mt,
    const prod_class_t* clssp,
    const timestampt* maxlatencyp)
{
    return ENOERR;
}


/*
 * LDM 4 convenience funct.
 * Change signature, Insert at rear of queue, send SIGCONT to process group
 */
/*ARGSUSED2*/
int pqe_xinsert(
    pqueue*          pq,
    pqe_index        index,
    const signaturet realsignature)
{
    return pqe_insert(pq, index);
}


int pq_clss_setfrom(
    pqueue* pq,
    prod_class_t *clssp)        /* modified upon return */
{
    timestampt ts = clssp->from;
    int        status = pq_last(pq, clssp, &ts);

    if(status == ENOERR)
    {
        if(tvEqual(ts, clssp->from))
            status = PQUEUE_END;
        else
            clssp->from = ts;
    }
    return status;
}


/*ARGSUSED0*/
int pq_cClassSet(
    pqueue*           pq,
    pq_match*         mtp,
    const prod_class_t* clssp)
{
    return ENOERR;
}
