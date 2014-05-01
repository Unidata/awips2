/*
 *   Copyright 1994, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pq.h,v 3.20.8.2.2.1.2.7 2009/05/21 20:30:44 steve Exp $ */

#ifndef _PQ_H
#define _PQ_H

#include <sys/types.h>	/* off_t, mode_t */
#include <stddef.h>	/* size_t */

#include "prod_class.h"


/*
 * The functions below return ENOERR upon success.
 * Upon failure, the return something else :-).
 * (Usually, that something else will be the a system
 * error (errno.h), don't count on it making sense.)
 */
#ifndef ENOERR
#define ENOERR 0
#endif /*!ENOERR */

#define PQ_END		-1	/* at end of product-queue */
#define PQ_CORRUPT	-2	/* the product-queue is corrupt */
#define PQ_NOTFOUND	-3	/* no such data-product */

typedef struct pqueue pqueue; /* private, implemented in pq.c */


/*
 * pflags arg to pq_open() and pq_create()
 */
#define PQ_DEFAULT	0x00
#define PQ_NOCLOBBER	0x01	/* Don't destroy existing file on create */
#define PQ_READONLY	0x02	/* Default is read/write */
#define PQ_NOLOCK	0x04	/* Disable locking */
#define PQ_PRIVATE	0x08	/* mmap() the file MAP_PRIVATE, default MAP_SHARED */
#define PQ_NOGROW	0x10	/* If pq_create(), must have intialsz */
#define PQ_NOMAP	0x20	/* Use malloc/read/write/free instead of mmap() */
#define PQ_MAPRGNS	0x40	/* Map region by region, default whole file */
#define PQ_SPARSE       0x80    /* Created as sparse file, zero blocks unallocated */
/* N.B.: bits 0x1000 (and above) in use internally */

extern int
pq_create(const char *path, mode_t mode, int pflags,
	size_t align, off_t initialsz, size_t nproducts,
	pqueue **pqp);

extern int
pq_open(const char *path,
	int pflags,
	pqueue **pqp);

extern int 
pq_close(pqueue *pq);


/*
 * Let the user find out the pagesize.
 */
extern int pq_pagesize(const pqueue *pq);


struct pqe_index {
	off_t offset;
	signaturet signature;
};
typedef struct pqe_index pqe_index;
#define pqeOffset(pqe) ((pqe).offset)
#define pqeEqual(left, rght) (pqeOffset(left) == pqeOffset(rght))

extern const pqe_index _pqenone;
#define PQE_NONE (_pqenone)
#define pqeIsNone(pqe) (pqeEqual(pqe, PQE_NONE))

#define PQUEUE_DUP (-2)	/* return value indicating attempt to insert
				duplicate product */
#define PQUEUE_BIG (-3)	/* return value indicating attempt to insert
				product that's too large */
extern int
pqe_new(pqueue *pq, const prod_info *infop,
	void **ptrp, pqe_index *indexp);

extern int
pqe_discard(pqueue *pq, pqe_index index);

/* Insert at rear of queue, send SIGCONT to process group */
extern int
pqe_insert(pqueue *pq, pqe_index index);

/*
 * LDM 4 convenience funct.
 * Change signature, Insert at rear of queue, send SIGCONT to process group
 */
extern int
pqe_xinsert(pqueue *pq, pqe_index index, const signaturet realsignature);

/*
 * pq_insertNoSig() is a special purpose function that only
 * needs to be in the public interface for 'pqsurf'.
 */
/* Insert at rear of queue  (Don't signal process group.) */
extern int
pq_insertNoSig(pqueue *pq, const product *prod);

/* Insert at rear of queue, send SIGCONT to process group */
extern int
pq_insert(pqueue *pq, const product *prod);


/*
 * Get some useful statistics.
 * 'highwater' is the max number of bytes used for data storage,
 * suitable as the 'initialsz' parameter to pq_create().
 * 'maxproductsp' is the max number of products in the inventory,
 * suitable as the 'nproducts' parameter to pq_create().
 */
extern int
pq_highwater(pqueue *pq, off_t *highwaterp, size_t *maxproductsp);


/* prototype for 4th arg to pq_sequence() */
typedef int pq_seqfunc(const prod_info *infop, const void *datap,
	void *xprod, size_t len,
	void *otherargs);

/*
 * Set cursor used by pq_sequence() or pq_seqdel().
 */
extern void
pq_cset(pqueue *pq, const timestampt *tsp);


/*
 * Get current cursor value used by pq_sequence() or pq_seqdel().
 */
extern void
pq_ctimestamp(const pqueue *pq, timestampt *tsp);


/*
 * Which direction the cursor moves in pq_sequence().
 */
typedef enum {
	TV_LT = -1,
	TV_EQ =  0,
	TV_GT =  1
} pq_match;


/*
 * Step thru the time sorted inventory according to 'mt',
 * and the current cursor value.
 *
 * If(mt == TV_LT), pq_sequence() will get a product
 * whose queue insertion timestamp is strictly less than
 * the current cursor value.
 *
 * If(mt == TV_GT), pq_sequence() will get a product
 * whose queue insertion timestamp is strictly greater than
 * the current cursor value.
 *
 * If(mt == TV_EQ), pq_sequence() will get a product
 * whose queue insertion timestamp is equal to
 * the current cursor value.
 *
 * If no product is in the inventory which which meets the
 * above spec, return PQUEUE_END.
 *
 * Otherwise, if the product info matches class,
 * execute ifMatch(xprod, len, otherargs) and return the
 * return value from ifMatch().
 */
#define PQUEUE_END PQ_END	/* return value indicating end of queue */
extern int
pq_sequence(pqueue *pq, pq_match mt,
	const prod_class *clssp, pq_seqfunc *ifMatch, void *otherargs);

/*
 * Boolean function to
 * check that the cursor timestime is
 * in the time range specified by clssp.
 * Returns non-zero if this is the case, zero if not.
 */
extern int
pq_ctimeck(const pqueue *pq,  pq_match mt, const prod_class *clssp,
	const timestampt *maxlatencyp);

/*
 * Figure out the direction of scan of clssp, and set *mtp to it.
 * Set the cursor to include all of clssp time range in the queue.
 * (N.B.: For "reverse" scans, this range may not include all
 * the arrival times.)
 */
extern int
pq_cClassSet(pqueue *pq,  pq_match *mtp, const prod_class *clssp);

/*
 * Like pq_sequence(), but the ifmatch action is to remove the
 * product from inventory.
 * If wait is nonzero, then wait for locks.
 */
extern int
pq_seqdel(pqueue *pq, pq_match mt,
	const prod_class *clssp, int wait, size_t *nrp, timestampt *tp);

/*
 * Find the most recent match and set the timestamp *tsp
 * to info->arrival (product creation time).
 * clss->from should be less than  clssp->to ("forward scan").
 * Leaves cursor set as side effect, ready for a forward scan.
 */
extern int
pq_last(pqueue *pq,
	const prod_class *clssp,
	timestampt *tsp); /* modified upon return */

/*
 * Find the most recent match and set clssp->from to it.
 * clss->from should be less than  clssp->to ("forward scan").
 */
extern int
pq_clss_setfrom(pqueue *pq, prod_class_t *clssp);


/*
 * Suspend yourself (sleep) until
 * one of the following events occurs:
 *   You recieve a signal that you handle.
 *   You recieve SIGCONT (as send from an insert proc indicating
 *      data is available).
 *   "maxsleep" seconds elapse.
 *   If "maxsleep" is zero, you could sleep forever. 
 */
extern unsigned
pq_suspend(unsigned int maxsleep);

/*
 * Get some detailed product queue statistics.  These may be useful for
 * monitoring the internal state of the product queue:
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
 *   Note: the fixed number of slots allocated for products when the
 *         queue was created is nalloc = (nprods + nfree + nempty).
 */
extern int
pq_stats(pqueue *pq,
	 size_t *nprodsp,
	 size_t *nfreep,
	 size_t *nemptyp,
	 size_t *nbytesp,
	 size_t *maxprodsp,
	 size_t *maxfreep,
	 size_t *minemptyp,
	 size_t *maxbytesp, 
	 double *age_oldestp,
	 size_t *maxextentp);

/*
 * Returns the insertion-timestamp of the oldest data-product in the
 * product-queue.
 *
 * Arguments:
 *	oldestCursor	Pointer to structure to received the insertion-time
 *			of the oldest data-product.
 * Returns:
 *	ENOERR		Success.
 *	else		Failure.
 */
int
pq_getOldestCursor(
    pqueue*		pq,
    timestampt* const	oldestCursor);

extern int		pq_del_oldest(pqueue *pq);
extern int		pq_fext_dump(pqueue *const pq);
extern void		pq_coffset(pqueue *pq, off_t c_offset);
extern void		pq_reset_random(void);
extern int		pq_clear_write_count(const char* path);
extern int		pq_get_write_count(const char* path, unsigned* count);
extern const char*	pq_strerror(const pqueue* pq, int error);


/*
 * Set the cursor based on the insertion-time of the product with the given
 * signature if and only if the associated data-product is found in the 
 * product-queue.
 *
 * Arguments:
 *      pq              Pointer to the product-queue.
 *      signature       The given signature.
 * Returns:
 *      0       Success.  The cursor is set to reference the data-product with
 *              the same signature as the given one.
 *      EACCES  "pq->fd" is not open for read or "pq->fd" is not open for write
 *              and PROT_WRITE was specified for a MAP_SHARED type mapping.
 *      EAGAIN  The mapping could not be locked in memory, if required by
 *              mlockall(), due to a lack of resources.
 *      EBADF   "pq->fd" is not a valid file descriptor open for reading.
 *      EDEADLK The necessary lock is blocked by some lock from another process
 *              and putting the calling process to sleep, waiting for that lock
 *              to become free would cause a deadlock.
 *      EFBIG or EINVAL
 *              The extent of the region is greater than the maximum file size.
 *      EFBIG   The file is a regular file and the extent of the region is
 *              greater than the offset maximum established in the open file
 *              description associated with "pq->fd".
 *      EINTR   A signal was caught during execution.
 *      EINVAL  The region's offset or extent is not valid, or "pq->fd" refers
 *              to a file that does not support locking.
 *      EINVAL  The region's offset is not a multiple of the page size as 
 *              returned by sysconf(), or is considered invalid by the 
 *              implementation.
 *      EIO     An I/O error occurred while reading from the file system.
 *      EIO     The metadata of a data-product in the product-queue could not be
 *              decoded.
 *      EMFILE  The number of mapped regions would exceed an
 *              implementation-dependent limit (per process or per system).
 *      ENODEV  "pq->fd" refers to a file whose type is not supported by mmap().
 *      ENOLCK  Satisfying the request would result in the number of locked
 *              regions in the system exceeding a system-imposed limit.
 *      ENOMEM  There is insufficient room in the address space to effect the
 *              necessary mapping.
 *      ENOMEM  The region's mapping could not be locked in memory, if required
 *              by mlockall(), because it would require more space than the 
 *              system is able to supply.
 *      ENOMEM  Insufficient memory is available.
 *      ENOTSUP The implementation does not support the access requested in
 *              "pq->pflags".
 *      ENXIO   The region's location is invalid for the object specified by
 *              "pq->fd".
 *      EOVERFLOW
 *              The smallest or, if the region's extent is non-zero, the
 *              largest offset of any byte in the requested segment cannot be
 *              represented correctly in an object of type off_t.
 *      EOVERFLOW
 *              The file size in bytes or the number of blocks allocated to the
 *              file or the file serial number cannot be represented correctly.
 *      EOVERFLOW
 *              The file is a regular file and the region's offset plus 
 *              extent exceeds the offset maximum established in the open file
 *              description associated with "fd". 
 *      EROFS   The file resides on a read-only file system.
 *
 *      PQ_CORRUPT
 *              The product-queue is corrupt.
 *      PQ_NOTFOUND
 *              A data-product with the given signature was not found in the
 *              product-queue.
 */
int
pq_setCursorFromSignature(
    pqueue* const       pq,
    const signaturet    signature);

#endif /* !_PQ_H */
