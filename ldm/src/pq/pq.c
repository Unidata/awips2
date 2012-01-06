/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pq.c,v 3.78.8.2.2.9.2.38 2009/05/21 20:30:42 steve Exp $ */

#include "ldmconfig.h"

#include <inttypes.h> /* sysconf */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#ifndef NO_MMAP
#include <sys/mman.h>
#endif /*!NO_MMAP*/
#include <unistd.h> /* sysconf */
#include <limits.h>
#include <assert.h>
#include <stdio.h>  /* DEBUG */
#include <errno.h>
#include <signal.h>
#include <math.h>
#include <time.h>
#include <search.h>

#include "ldm.h"
#include "pq.h"
#include "fbits.h"
#include "globals.h"
#include "lcm.h"
#include "ulog.h"
#include "ldmprint.h"
#include "fsStats.h"
#include "ldm_xlen.h"
#include "prod_info.h"
#include "timestamp.h"

/* #define TRACE_LOCK 1 */

/*
 * The time interval, in seconds, to be subtracted from the creation-time
 * of a "signature" data-product in order to determine the initial
 * start-time for a search of the data-product in the time-queue.
 */
#define SEARCH_BACKOFF  interval

/*
 * A value which is an invalid off_t
 */
#define OFF_NONE  ((off_t)(-1))


/*
 * flags used by the region layer.
 */
#define RGN_NOLOCK      0x1     /* Don't lock region.  Used when
                                 * contention control handled
                                 * elsewhere.  */
#define RGN_NOWAIT      0x2     /* return immediate if can't lock, else wait */

#define RGN_WRITE       0x4     /* we intend to modify, else read only */

#define RGN_MODIFIED    RGN_WRITE       /* we did modify, else, discard */


/* useful for aligning memory */
#define _RNDUP(x, unit)  ((((x) + (unit) - 1) / (unit)) * (unit))
#define M_RND_UNIT      (sizeof(double))
#define M_RNDUP(x) _RNDUP(x, M_RND_UNIT)
#define M_RNDDOWN(x)  ((x) - ((x)%M_RND_UNIT))

#define MIN_RGN_SIZE    M_RND_UNIT

/* 
 * Some of the data structures and algorithms in this implementation
 * of the pq library use Skip Lists, data structures designed to
 * support the operations of search, insert, and delete in logarithmic
 * average time.  For more information on Skip Lists, see
 *
 *  William Pugh: Skip Lists: A Probabilistic Alternative to Balanced
 *  Trees. 668-676, CACM 33(6), June 1990.
 *
 * and
 *
 *  ftp://ftp.cs.umd.edu/pub/skipLists/
 */

/* Begin fb */
/*
 * This layer provides dynamic allocation of a set of "fblks" of small
 * integral sizes, intended to hold offsets.  These are used in leau
 * of pointers in the subsequent skip list algorithm, because these may
 * all be in the shared mmap'd file where pointers would be useless.
 * So where the skip list algorithm specifies a block of pointers of
 * size k, this layer provides an fblk that can hold k offsets.  
 * 
 * The fblks must be allocated dynamically, but we know what
 * distribution of sizes is required by the skip list algorithms, so we
 * just preallocate most of the fblks and keep them on free lists for
 * each size needed.  A few extra fblks are provided for statistical
 * overflow.
 */

#define MAXLEVELS 15            /* enough for 4^15 products in the queue */

typedef size_t fblk_t;          /* index into fblks array, points to fblk */

struct fb {
#define FB_MAGIC        0x54514642
  size_t magic;                 /* TQFB, used to check alignment, endianness */
  int maxsize;                  /* maximum size of fblks */
  size_t arena_sz;              /* number of fblks, in units of fblk_t */
  size_t avail;                 /* fblks still available, of all sizes */
  size_t allocated;             /* number of fblks currently allocated */
  size_t nfree[MAXLEVELS];      /* number of free fblks for each level */
  fblk_t free[MAXLEVELS];       /* Heads of free lists.  When free,
                                   the first fblk_t in each fblk is the
                                   offset of the next free block of
                                   the same size, relative to the
                                   start of the fblks array */
#define FBLKS_NALLOC_INITIAL 2
  fblk_t fblks[FBLKS_NALLOC_INITIAL]; /* actually arena_sz long */
};
typedef struct fb fb;

/* return floor(log4(n)) */
static int 
log4(size_t n) 
{
    assert(n > 0);
    return (int)(log(n + 0.5)/log(4.0));
}


static volatile sig_atomic_t initialized = 0;


void
pq_reset_random()
{
    initialized = 0;
}


/* 
 * Return random level of
 *  0 with probability 3/4*(1/4)^0 = .75
 *  1 with probability 3/4*(1/4)^1 = .1875
 *  2 with probability 3/4*(1/4)^2 = .046875
 *  ...
 *  capped at "fbp->maxsize - 1".
 */
static int 
fb_ranlev(fb *fbp) 
{
#   define      BITSINRANDOM 31
    int         level = 0;
    int         b;
    int         maxlevel = fbp->maxsize - 1;
    static int  randomsLeft;
    static int  randomBits;

    if (initialized == 0) {
        srandom(1);

        randomBits = random();
        randomsLeft = BITSINRANDOM / 2;    
        initialized = 1;
    }

    do {
        b = randomBits&3;
        if (!b) level++;
        randomBits>>=2;
        if (--randomsLeft == 0) {
            randomBits = random();
            randomsLeft = BITSINRANDOM / 2;
        };
    } while (!b);

    return level <= maxlevel ? level : maxlevel;
}


/* returns arena size needed for fblks for nelems products */
static size_t 
fb_arena_sz(size_t nelems) 
{
    int maxsize = log4(nelems) + 1;
    int blksize;                /* size of fblk (number of levels) */
    int numblks;                /* blks to preallocate for each level */
    int level;
    size_t total=0;
    numblks = 0.75*nelems;      /* level 1 blks to preallocate */
    for (level = 0; level < maxsize; level++) {
        blksize = level + 1;
        total += blksize*numblks;
        if(numblks >= 4)
            numblks /= 4;
        else
            numblks = 1;
    }
    /* extra blocks to allow for statistical fluctuations */
    total += 3*sqrt((double)nelems)*log4(nelems)*maxsize;
    return total;
    /*
     * I believe this function underestimates the necessary number of skip-list
     * nodes.  I believe the maximum number of nodes should be "3*nelems + 2",
     * which would correspond to the worst-case scenario in which the
     * time-queue is full and the data-products are separated from each other
     * and from the ends of the queue by the free regions in the region-list.
     * --SRE 2007-06-14
     */
}

/* return size of fb needed for nelems products */
static size_t
fb_sz(size_t nelems) 
{
    static size_t sz;
    static size_t last_nelems = 0;
    if(nelems != last_nelems) {
        last_nelems = nelems;
        sz = sizeof(fb) - sizeof(fblk_t) * FBLKS_NALLOC_INITIAL;
        sz += fb_arena_sz(nelems) * sizeof(fblk_t);
    }
    return sz;
}


/* Dump info about free fblks arena, for debugging.
static void
fb_stats_dump(fb *fbp) 
{
    int level;
    assert(fbp != NULL);
    uerror("maxsize = %d\n", fbp->maxsize);
    uerror("arena_sz = %d\n", fbp->arena_sz);
    uerror("avail = %d\n", fbp->avail);
    uerror("allocated = %d\n", fbp->allocated);
    for(level = 0; level <= fbp->maxsize; level++) {
        uerror("nfree[%d]:\t%d\t%d\n", level, fbp->nfree[level], 
               fbp->free[level]);
    }
}
*/


/* initialize fblks of needed sizes in needed proportions */
static void
fb_init(fb *fbp, size_t nalloc) 
{
    size_t fblk_sz;
    int i, j;
    int maxsize = log4(nalloc) + 1;
    fblk_t offset = 0;
    int level;                  /* level of skip list pointers */
    int blksize;                /* size of fblk (number of levels) */
    int numblks;                /* blks to preallocate for each level */

    assert(maxsize < MAXLEVELS);

    fbp->magic = FB_MAGIC;      /* to later check we mapped it correctly */
    fbp->maxsize = maxsize;
    /* free[i] is the free list for blocks of size i+1; free[maxsize] holds
       3*sqrt(nalloc)*log4(nalloc) extra blocks of max length to allow for
       random variations. */

    /* initialize arena to invalid offsets */
    fblk_sz = fb_arena_sz(nalloc);
    for(i = 0; i < fblk_sz; i++)
        fbp->fblks[i] = (fblk_t)OFF_NONE;

    fbp->allocated = 0;
    fbp->avail = 0;
    fbp->free[0] = offset;

    numblks = 0.75*nalloc;      /* level 1 blks to preallocate */
    for (level = 0; level < maxsize; level++) {
        blksize = level + 1;
        fbp->free[level] = offset;
        for (j = 0; j < numblks - 1; j++) { /* link each block to next one */
            fbp->fblks[offset] = offset + blksize;
            offset += blksize;
            fbp->avail++;
        }
        fbp->fblks[offset] = (fblk_t)OFF_NONE;
        offset +=  blksize;
        fbp->avail++;
        fbp->nfree[level] = numblks;
        if(numblks >= 4)
            numblks /= 4;
        else
            numblks = 1;
    }


    /* create free list of extra blocks of max size */
    numblks = 3*sqrt((double)nalloc)*log4(nalloc);
    blksize = maxsize;
    fbp->free[maxsize] = offset;
    for(j = 0; j < numblks - 1; j++) {
        fbp->fblks[offset] = offset + blksize;  /* link to next */
        offset += blksize;
        fbp->avail++;
    }
    fbp->fblks[offset] = (fblk_t)OFF_NONE; /* end of list */
    offset +=  blksize;
    fbp->avail++;
    fbp->nfree[maxsize] = numblks;
    fbp->arena_sz = offset;
}


/* 
 * Returns a free fblk of specified size to list. 
 */
static void 
fb_rel(fb *fbp, int size, fblk_t fblk)
{
    int level = size - 1;

    assert(fbp != NULL);
    assert(0 < size && size <= fbp->maxsize);
    assert(fblk < fbp->arena_sz);

    fbp->fblks[fblk] = fbp->free[level]; /* stick on front of list */
    fbp->free[level] = fblk;
    fbp->nfree[level]++;
    fbp->avail++;
    fbp->allocated--;
}


/* 
 * Gets a free fblk of specified level (0 <= level < MAX_LEVEL).
 * If no fblks are available, returns OFF_NONE.
 */
static fblk_t 
fb_get(fb *fbp, int level)
{
    fblk_t fblk, fblk2;
    int size = level + 1;
  
    assert(fbp != NULL);
    assert(0 < size && size <= fbp->maxsize);

    if(fbp->nfree[level] > 0) { /* ok, available */
        fblk = fbp->free[level];        /* take it off the front of the list */
        assert(fblk != (fblk_t)OFF_NONE);
        fbp->free[level] = fbp->fblks[fblk];
        fbp->nfree[level]--;
        fbp->avail--;
        fbp->allocated++;
        return fblk;
    }
    /* else:  all out, have to get it from next level */
    level++;
    while (level <= fbp->maxsize) {
        if(fbp->nfree[level] > 0) {
            fblk = fbp->free[level];    /* take it off the front of the list */
            assert(fblk != (fblk_t)OFF_NONE);
            fbp->free[level] = fbp->fblks[fblk];
            fbp->nfree[level]--;
            fbp->avail--;
            fbp->allocated++;

            if(level < fbp->maxsize && size < level + 1) { 
                /* split off remainder and release it */
                fblk2 = fblk + size;
                fb_rel(fbp, (level+1) - size, fblk2);
                fbp->allocated++; /* fix count, since fb_rel decremented */
            }
            return fblk;
        }
        level++;
    }
    /* else: all out of extra blocks too.  This means we tried to keep
       in product queue significantly more than the specified maximum
       number of products. */
    uerror("fb layer ran out of product slots, too many products in queue\n");
    /* fb_stats_dump(fbp);   */
    return (fblk_t)OFF_NONE;
}
/* End fb */
/* Begin tqueue */

/*
 * The product queue is indexed by product insertion time,
 * represented by timestampt (timestamp).
 * The tqueue structure is this index. It refers to the
 * region index pq->rlp via the offset.
 */

typedef off_t tqep_t;           /* index into tqep array, points to tqelem */

#define TQ_NONE ((tqep_t) OFF_NONE)
/* For extra TQ_NIL and TQ_HEAD elements */
#define TQ_OVERHEAD_ELEMS 2

struct tqelem {
  timestampt tv;
  off_t offset;                 /* offset of region associated with
                                   product.  Also used to link up
                                   freelist of tqelems. */
  fblk_t fblk;                  /* forward "pointer" block of length lvl */
};
typedef struct tqelem tqelem;

struct tqueue
{
#define TQ_NALLOC_INITIAL       84
  size_t nalloc;                /* number of allocated product slots */
  size_t nelems;                /* current number of products in queue */
  size_t nfree;                 /* number of free tqep's left */
  tqep_t free;                  /* index of tqep free list */
  int level;                    /* Current level of skip list */
  off_t fbp_off;                /* skip list blocks */
  tqelem tqep[TQ_NALLOC_INITIAL]; /* actually nalloc long */
};
typedef struct tqueue tqueue;

/* 
 * For a tq with the capacity to index nelems, return how much space
 * it will consume
 */
static size_t
tq_sz(size_t nelems) 
{
    static size_t sz;
    static size_t last_nelems = 0;
    if(nelems != last_nelems) {
        last_nelems = nelems;
        sz = sizeof(tqueue) - sizeof(tqelem) * TQ_NALLOC_INITIAL;
        /* TQ_OVERHEAD_ELEMS extra slots for TQ_NIL, TQ_HEADER  */
        sz += (nelems + TQ_OVERHEAD_ELEMS) * sizeof(tqelem);
    }
    return sz;
}

/*
 * Initialize tqueue structures
 */
static void
tq_init(tqueue *const tq, size_t const nalloc0, fb *fbp)
{
    tqelem *tqelemp;
    tqelem *const end = &tq->tqep[nalloc0 + TQ_OVERHEAD_ELEMS];
    fblk_t *fblkp;
    int i;
    int maxlevel;
    size_t nelems;
    size_t nalloc = nalloc0 + TQ_OVERHEAD_ELEMS; /* for TQ_NIL, TQ_HEADER */

    assert(fbp->magic == FB_MAGIC); /* sanity check */

    tq->nalloc = nalloc0;
    /* cache offset to skip list blocks, so we can find them from only tq */
    tq->fbp_off = (char *)fbp - (char *)tq;
    /* build two distinguished tqelems, TQ_NIL and TQ_HEAD */
#define TQ_NIL ((tqep_t)0)
    tqelemp = &tq->tqep[TQ_NIL];
    tqelemp->tv = TS_ENDT; /* the end of time, as we know it */
    tqelemp->offset = OFF_NONE;
    tqelemp->fblk = fb_get(fbp, 0); /* not used */
#define TQ_HEAD ((tqep_t)1)
    tqelemp = &tq->tqep[TQ_HEAD];
    tqelemp->tv = TS_NONE;              /* not used */
    tqelemp->offset = OFF_NONE; /* not used */
    maxlevel = fbp->maxsize - 1;
    tqelemp->fblk = fb_get(fbp, maxlevel);
    fblkp = &fbp->fblks[tqelemp->fblk];
    for(i=0; i < fbp->maxsize; i++) {
        fblkp[i] = TQ_NIL;              /* set all forward "pointers" of TQ_HEAD to nil */
    }

    tq->level = 0;

    tq->nelems = TQ_OVERHEAD_ELEMS;
    tq->nfree = nalloc - TQ_OVERHEAD_ELEMS;
    tq->free = tq->nelems;              /* head of list of free tqelems */
    /* Initialize rest of tqelems */
    nelems = tq->nelems;
    for(tqelemp = &tq->tqep[tq->nelems]; tqelemp < end; tqelemp++) {
        tqelemp->tv = TS_NONE;
        tqelemp->offset = ++nelems;     /* link on to free list */
        tqelemp->fblk = (fblk_t)OFF_NONE;
    }
    /* terminate free list through pointer of last element */
    tqelemp = &tq->tqep[tq->nalloc + 2-1];
    tqelemp->offset = TQ_NONE;
}

/*
 * Affirm that that another element can be added to tq
 */
static int
tq_HasSpace(const tqueue *const tq)
{
        assert(tq->nelems - TQ_OVERHEAD_ELEMS <= tq->nalloc);
        return (tq->nelems - TQ_OVERHEAD_ELEMS < tq->nalloc);
}

/* 
 * Get tqelem from free list.
 * Returns TQ_NONE if no free elements are left.
 */
static tqep_t
tq_get_tqelem(tqueue *const tq) 
{
    tqep_t result;
    tqelem *tpp;
    if(tq->nfree > 0) {
        result = tq->free;
        tpp = &tq->tqep[result];
        tq->free = tpp->offset;
        tq->nfree--;
        tq->nelems++;
        assert(result > TQ_HEAD && result != TQ_NONE);
        return result;
    }
    /* else */
    return TQ_NONE;
}


/*
 * Return tqelem to free list.
 * p is index of tqelem to be freed.
 */
static void
tq_rel_tqelem(tqueue *const tq, int level, tqep_t p) 
{
    tqelem *tpp = &tq->tqep[p];
    fb *fbp = (fb *)((char *)tq + tq->fbp_off);

    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    assert(TQ_HEAD < p && p < tq->nalloc + TQ_OVERHEAD_ELEMS);
    tpp->tv = TS_NONE;
    tpp->offset = tq->free;
    /* free associated fblk */
    fb_rel(fbp, level + 1, tpp->fblk);
    tpp->fblk = (fblk_t)OFF_NONE;
    tq->free = p;
    tq->nfree++;
    tq->nelems--;
}


#define TV_CMP_LT(tv, uv) \
        ((tv).tv_sec < (uv).tv_sec || \
         (tv).tv_sec == (uv).tv_sec && (tv).tv_usec < (uv).tv_usec)

#define TV_CMP_LE(tv, uv) \
        ((tv).tv_sec < (uv).tv_sec || \
         (tv).tv_sec == (uv).tv_sec && (tv).tv_usec <= (uv).tv_usec)

#define TV_CMP_EQ(tv, uv) \
        ((tv).tv_sec == (uv).tv_sec && (tv).tv_usec == (uv).tv_usec)

/*
 * Add element to time-queue.
 *
 * Arguments:
 *      tq      Pointer to time-queue.
 *      offset  Offset to data-portion of element to be added to time-queue.
 * Returns:
 *      0       Success
 *      !0      <errno.h> failure code.
 */
static int
tq_add(
    tqueue* const       tq,
    const off_t         offset)
{
    int         status;
    fb*         fbp = (fb*)((char*)tq + tq->fbp_off);
    tqep_t      tpix;
    tqelem*     tp;

    assert(fbp->magic == FB_MAGIC);     /* check for sanity */
    assert(tq->nalloc != 0);
    assert(tq_HasSpace(tq));

    tpix = tq_get_tqelem(tq);           /* get new tqelem off of free list */
    assert(tpix != TQ_NONE);

    tp = &tq->tqep[tpix];
    status = set_timestamp(&tp->tv);    /* set insertion-time to now */

    if (status == ENOERR) {
        tqelem* tpp;
        tqep_t  update[MAXLEVELS];
        tqep_t  p = TQ_HEAD;
        int     k = tq->level;

        tp->offset = offset;            /* tp is tqelem* to insert */

        do {
            tpp = &tq->tqep[p];

            for (;;) {
                tqep_t  q;
                tqelem* tqp;

                /*
                 * q = p->forward[k]; same as *(fbp->fblks + tpp->fblk + k).
                 */
                q = fbp->fblks[tpp->fblk + k];
                tqp = &tq->tqep[q];

                /*
                 * while(q->key < key) {...}
                 */
                while (TV_CMP_LT(tqp->tv, tp->tv)) {
                    p = q;
                    tpp = tqp;
                    /*
                     * q = p->forward[k]
                     */
                    q = fbp->fblks[tpp->fblk + k];
                    tqp = &tq->tqep[q];
                }

                if (!TV_CMP_EQ(tqp->tv, tp->tv)) {
                    /*
                     * The insertion-time of the new data-product is unique.
                     */
                    update[k] = p;

                    if (--k < 0)
                        break;          /* done */
                }
                else {
                    /*
                     * A data-product with the same insertion-time as the
                     * target-time already exists in the time-queue.  Because
                     * keys in the time-queue must be unique, the target-time
                     * is incremented by one microsecond and the search is
                     * restarted from the last highest-level position.  This
                     * should be safe as long as the mean interval between
                     * data-product insertions is much greater than one
                     * microsecond (ASSUMPTION).
                     */
                    timestamp_incr(&tp->tv);

                    if (k < tq->level) {
                        k = tq->level;
                        p = update[k];
                    }

                    break;              /* re-search necessary */
                }                       /* found entry with same time */
            }                           /* loop until done or re-search */
        } while (k >= 0);               /* loop until done */

        /*
         * Found where to put the new element.  Obtain a skip-list node to
         * contain it.
         *
         * The following hack limits increments in level to 1.  This messes up
         * the theoretical distribution of random levels slightly and could be
         * left out for a "purist" implementation.
         */
        k = fb_ranlev(fbp);
        if (k > tq->level) {
            tq->level++;
            k = tq->level;
            update[k] = TQ_HEAD;
        }

        tp->fblk = fb_get(fbp, k);      /* Get new fblk of level k */

        /*
         * Insert the new element by having it reference the following element
         * and having the immediately previous level-k element reference the new
         * element for all level k.
         */
        do {
            p = update[k];
            tpp = &tq->tqep[p];
            /*
             * q->forward[k] = p->forward[k]
             */
            fbp->fblks[tp->fblk + k] = fbp->fblks[tpp->fblk + k];
            /*
             * p->forward[k] = q;  forward pointer to new tqelem.
             */
            fbp->fblks[tpp->fblk + k] = tpix;
        } while(--k >= 0);
    }                                   /* insertion-time set */

    return status;
}


/*
 * Search the tqueue 'tq' for a tqelem whose time is greatest less
 * than, equal to, or least greater than 'key', according whether 'mt'
 * is TV_LT, TV_EQ, or TV_GT.  ASSUMPTION: All keys in the time-queue are 
 * unique.
 *
 * Returns the tqelem or NULL if no match.
 */
static tqelem *
tqe_find(const tqueue *const tq, const timestampt *const key, const pq_match mt)
{
    int k;
    tqep_t p;
    tqep_t q;
    const tqelem *tpp;
    const tqelem *tqp;
    fb *fbp = (fb *)((char *)tq + tq->fbp_off);

    if(tq->nelems - TQ_OVERHEAD_ELEMS == 0) {
        return NULL;
    }
    assert(fbp->magic == FB_MAGIC);
    p = TQ_HEAD;                /* header of skip list */
    tpp = &tq->tqep[p];
    k = tq->level;              /* level of skip list and header */
    do {
        /* q = p->forward[k]; */
        /* same as *(fbp->fblks + tpp->fblk + k) */
        q = fbp->fblks[tpp->fblk + k];
        tqp = &tq->tqep[q];
        /* while(q->key < key) {...} */
        while(TV_CMP_LT(tqp->tv, *key)) {
            p = q;
            tpp = &tq->tqep[p];
            /* q = p->forward[k]; */
            q = fbp->fblks[tpp->fblk + k];
            tqp = &tq->tqep[q];
        }
    } while(--k >= 0);
  
    /* q is the next entry >= key.  p is < than it.  */
    switch (mt) {
    case TV_LT:
        if(p == TQ_HEAD) {
            return NULL;
        } /*else */
        return (tqelem *) tpp;
    case TV_EQ:
        if(TV_CMP_EQ(tqp->tv, *key)) {
            return (tqelem *) tqp;
        } /* else */
        return NULL;
    case TV_GT:
        if(q == TQ_NIL) {
            return NULL;
        } /* else */
        if(TV_CMP_EQ(tqp->tv, *key)) {
            q = fbp->fblks[tqp->fblk];
            tqp = &tq->tqep[q];
            if(q == TQ_NIL) {
                return NULL;
            } /* else */
            return (tqelem *) tqp;
        } /* else */
        return (tqelem *) tqp;
    }
    uerror("tqe_find: bad value for mt: %d", mt);
    return NULL;
}


/*
 * Return the oldest (first) element in the tqueue 'tq'
 *
 * Returns the tqelem or NULL if no match. 
 */
static tqelem *
tqe_first(const tqueue *const tq) 
{
    tqep_t p;
    tqep_t q;
    const tqelem *tpp;
    const tqelem *tqp;
    fb *fbp = (fb *)((char *)tq + tq->fbp_off);

    assert(fbp->magic == FB_MAGIC);
    p = TQ_HEAD;                /* header of skip list */
    tpp = &tq->tqep[p];
    /* q = p->forward[0]; */
    /* same as *(fbp->fblks + tpp->fblk) */
    q = fbp->fblks[tpp->fblk];
    if(q == TQ_NIL) {
        return NULL;
    }
    tqp = &tq->tqep[q];
    return (tqelem *) tqp;
}


/*
 * Delete elem from queue; if not found, don't do anything.
 */
static void
tq_delete(tqueue *const tq, tqelem *tqep)
{
    int k, m;
    tqep_t p;
    tqep_t q;
    const tqelem *tpp;
    const tqelem *tqp;
    tqep_t update[MAXLEVELS];
    fb *fbp = (fb *)((char *)tq + tq->fbp_off);

    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    /* p = l->header; */
    p = TQ_HEAD;
    tpp = &tq->tqep[p];
    /* k = l->level; */
    m = tq->level;
    k = m;
    do {
        /* q = p->forward[k]; */
        /* same as *(fbp->fblks + tpp->fblk + k) */
        q = fbp->fblks[tpp->fblk + k];
        tqp = &tq->tqep[q];
        assert((q == TQ_NIL) || (TQ_HEAD < q && q < tq->nalloc + TQ_OVERHEAD_ELEMS));
        /* while(q->key < key) { */
        /* on fast machines distinct products may have equal timestamps */
        while(TV_CMP_LT(tqp->tv, tqep->tv) ||
              (TV_CMP_EQ(tqp->tv, tqep->tv) && tqp->offset < tqep->offset)) {
            p = q;
            tpp = tqp;
            /* q = p->forward[k]; */
            q = fbp->fblks[tpp->fblk + k];
            tqp = &tq->tqep[q];
            assert((q == TQ_NIL) || (TQ_HEAD < q && q < tq->nalloc + TQ_OVERHEAD_ELEMS));
        }
        update[k] = p;
    } while(--k >= 0);
    /* q may have key equal or greater than the specified key.  */
    assert((q == TQ_NIL) || (TQ_HEAD < q && q < tq->nalloc + TQ_OVERHEAD_ELEMS));
    /* if (q->key == key) { */
    /*
     * Given the way this function is used, the equality-test of the offsets in
     * the following line is unnecessary.  It's here as a reminder and because
     * the behavior of this function should be independent of how it's used.
     */
    if (TV_CMP_EQ(tqp->tv, tqep->tv) && tqp->offset == tqep->offset) {
        for(k = 0; k <= m; k++) {
            p = update[k];
            tpp = &tq->tqep[p];
            /* if (p->forward[k] != q) { */
            if (fbp->fblks[tpp->fblk + k] != q) {
                break;
            }
            /* p->forward[k] = q->forward[k]; */
            fbp->fblks[tpp->fblk + k] = fbp->fblks[tqp->fblk + k];
        }
        /* free(q); */
        tq_rel_tqelem(tq, k - 1, q);
    
        /* update level of list, in case we just deleted highest level */
        /* while( l->header->forward[m] == NIL && m > 0 ) { */
        p = TQ_HEAD;
        tpp = &tq->tqep[p];
        while(fbp->fblks[tpp->fblk + m] == TQ_NIL && m > 0) {
            m--;
        }
        tq->level = m;
    }
    return;
}


/*
 * Return the next element by insertion time in the time queue tq, 
 * after the one pointed to by tqep.
 */
static tqelem *
tq_next(const tqueue *const tq, const tqelem *const tqep) 
{
    /* get the skip list array of offsets */
    fb *fbp = (fb *)((char *)tq + tq->fbp_off);
    assert(fbp->magic == FB_MAGIC);

    /* use the 0-level offset block to get the next element by time */
    return (tqelem *) &tq->tqep[fbp->fblks[tqep->fblk]];
}



/* End tqueue */
/* Begin region */

/*
 * This is the basic structure used for keeping track of
 * storage allocations in the queue.  It appears on the shared region
 * list, regionl * pq->rlp.
 *
 * There are three kinds of regions: in-use, free, and empty.  In-Use
 * regions have an offset and extent and are in use for containing a
 * data product (or an internal data structure).  Free regions have an
 * offset and extent but are available for use.  Empty regions are
 * just slots for regions that don't have any meaningful offset and
 * extent currently.  There are a fixed maximum number of regions in
 * any product queue, allocated initially by the "-S nproducts" option
 * to pqcreate (or by default, assuming an average product size).
 *
 * Initially, a few overhead regions are allocated for indices, one
 * large free region is allocated for products, and all the other
 * regions are empty.
 *
 * In the steady state, there are a large number of in-use regions,
 * a small number of free regions, and however many empty slots are
 * left.  When a new product comes in, it gets a free region, if there
 * is one that's big enough.  Any extra space in the region is carved
 * off to form a new free region.  When a region is added to the list
 * of free regions, a check is made to see if it can be consolidated
 * with adjacent regions to form a larger region.  This consolidation
 * can free up empty slots that go back on the list of empty regions.
 *
 * If no free region of adequate size is available, the oldest product
 * in the queue is deleted repeatedly until a large enough region
 * becomes available, either through freeing a single large enough
 * region, or through consolidation of smaller adjacent regions.
 *
 * For empty regions, the 'next' member of the region structure forms
 * a linked list of empty regions; the 'prev' member is not used.  The
 * head of the empty list is referred to by the list of empty regions,
 * pq->rlp->empty.
 *
 * For free regions, the 'next' and 'prev' members are used instead as
 * offsets into an index to quickly access the list of free regions by
 * offset and by extent, respectively.
 *
 * Two principles are important to understanding this implementation:
 * regions are manipulated through a region table without accessing
 * the actual regions at all, and offsets rather than pointers are
 * used since the region table is shared among mulitple processes.
 * Not accessing region data except to read and write products avoids
 * paging in product space just to do region management, but also
 * means we cannot use "border tag" consolidation algorithms, for
 * example.
 *
 * Since we can't store pointers in shared data space, regions are
 * accessed by an analogous "region list index" rlix for the region
 * pq->rlp->rp[rlix].
 *
 * When in use and locked, (extent, offset) are put onto a process
 * private list of regions in use, riul* pq->riulp.
 */

struct region {
    off_t offset;
    size_t extent;
    size_t next;                /* For a free region, the skip list
                                   pointer block for freelist offset
                                   index */
    size_t prev;                /* For a free region, skip list
                                   pointer block for freelist extent
                                   index */
};
typedef struct region region;

/* sentinel value for prev and next members of regions */ 
#define RL_NONE ((size_t)(-1))

#define ISALLOC  ((unsigned)0x1)        /* extent field is or'd with ISALLOC 
                                         * when allocated */
#define set_IsAlloc(rp)         (fSet((rp)->extent, ISALLOC))
#define clear_IsAlloc(rp)       (fClr((rp)->extent, ISALLOC))
#define IsAlloc(rp)     (fIsSet((rp)->extent, ISALLOC))
#define IsFree(rp)      (!IsAlloc(rp))
#define Extent(rp)      (fMask((rp)->extent, ISALLOC))

/* End region */
/* Begin regionl */

#define RL_MAGIC        0x524c4841 /* "RLHA" to check alignment, endianness */
#define RL_NALLOC_INITIAL       5

/*
 * The list of allocations in the file.
 */
struct regionl
{
    size_t nalloc;              /* total no. of in-use + free + empty regions */
    size_t nchains;             /* number of chain slots for hashing */
    size_t empty;               /* rp-index of head of empty regions */
    /* statistics */
    size_t nelems;              /* number of in-use regions */
    size_t maxelems;            /* maximum nelems so far */
    size_t nfree;               /* number of free regions */
    size_t maxfree;             /* maximum nfree so far */
    size_t maxfextent;          /* max extent of regions currently on freelist */
    size_t nempty;              /* number of empty regions */
    size_t minempty;            /* minimum nempty so far */
    size_t nbytes;              /* number of bytes in use for data products */
    size_t maxbytes;            /* max nbytes so far */
    off_t  fbp_off;             /* skip list blocks */
                                /* skip list index for free list by offset */
    size_t level_foff;          /* current level of skip list by offset */
    size_t foff;                /* head of skip list by offset */
                                /* skip list index for free list by extent */
    size_t level_fext;          /* current level of skip list by extent */
    size_t fext;                /* head of skip list by extent */

    /* the region table, containing in-use, free, and empty regions */
    region rp[RL_NALLOC_INITIAL]; /* actually nalloc long */
};
typedef struct regionl regionl;

/* Overhead for region slots used as head and tail of free skip lists */
#define RL_FREE_OVERHEAD 4
#define RL_EMPTY_HD RL_FREE_OVERHEAD

/* 
 * Create a list of nalloc empty region slots for region list rl.
 */
static void
rp_init(regionl *const rl) 
{
    region *rep;
    region *rlrp = rl->rp;
    region *const end = rlrp + (rl->nalloc + RL_FREE_OVERHEAD);
    off_t irl = RL_EMPTY_HD + 1;

    /* Note: don't need or use prev member for empty regions. */
    for(rep = rlrp + RL_EMPTY_HD; rep < end; rep++, irl++) {
        rep->offset = OFF_NONE;
        rep->extent = 0;
        rep->next = irl;        /* link up empty region list */
    }
    rep = rlrp + (irl-2);
    rep->next = RL_NONE;        /* reset last pointer to indicate end of empty list */
}


/* 
 * Get the rp-index of an empty region slot. 
 * Returns RL_NONE if no empty slots are available.
 */
static size_t 
rp_get(regionl *rl) 
{
    size_t ix;
    region *rep;
    size_t iy;
    region *rlrp = rl->rp;

    /* Just get first empty slot on list */
    if(rl->empty == RL_NONE) {
        assert(rl->nempty == 0);
        return RL_NONE;
    }
    ix = rl->empty;
    rep = rlrp + ix;
    iy = rep->next;
    rl->empty = iy;
    rl->nempty--;
    if(rl->nempty < rl->minempty)
        rl->minempty = rl->nempty;
    return ix;
}


/* 
 * Return the region slot at index rlix to region list rl's empty
 * region slot list.
 */
static void 
rp_rel(regionl *rl, size_t rlix) 
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    size_t rnix = rl->empty;

    assert(0 < rlix && rlix < rl->nalloc + RL_FREE_OVERHEAD);

    /* Just put on front of list of empties. */
    rep->next = rnix;
    rep->offset = OFF_NONE;
    rep->extent = 0;
    rl->empty = rlix;
    rl->nempty++;
}

/* Heads of hash chain lists.  The size of this struct depends on the
 * number of products (pq->nalloc).  It is placed directly after the
 * regionl struct. */
struct rlhash {
  size_t magic;
  size_t chains[RL_NALLOC_INITIAL]; /* heads of lists of regions */
};
typedef struct rlhash rlhash;


/*
 * Returns 1 if prime, 0 if composite.
 * Used to get prime for hashing, on the order of the number of product slots.
 */
static int
isprime(unsigned long n) {
  unsigned long d;

  assert(n <= 4294967290UL);   /* if larger: infinite loop with 32 bit longs */
  if (n <= 1)
    return 0;
  if (n <= 19)
    if (n==2 || n==3 || n==5 || n==7 || n==11 || n==13 || n==17 || n==19)
      return 1;
  if ( n%2==0||n%3==0||n%5==0||n%7==0||n%11==0||n%13==0||n%17==0||n%19==0)
    return 0;
  for(d = 23; d*d <= n; d += 2) {
    if (n % d == 0)
      return 0;
  }
  return 1;
}

static unsigned long
prevprime(unsigned long n) {/* find largest prime <= n */
  assert (n > 1);
  if(n == 2)
    return n;
  if(n%2 == 0)
    n--;
  while(n > 0) {
    if(isprime(n))
      return n;
    n -= 2;
  }
  return 0;                     /* NOT REACHED */
}

/* Tuning parameter, expected length of hash chain lists, hence the
 * expected number of list elements to be examined in an unsuccessful
 * search.  Making this smaller will decrease region insertion,
 * deletion, and find times at the expense of more space in the queue
 * to hold a larger number of hash chain lists.  */
#define RL_EXP_CHAIN_LEN  4

/*
 * Returns number of chains required for the specified number of elements.
 */
static size_t
rlhash_nchains(size_t const nelems) 
{
  return prevprime(nelems / RL_EXP_CHAIN_LEN);
}

/*
 * For an rlhash which is nelems long, return how much space it will
 * consume.
 */
static size_t
rlhash_sz(size_t nelems)
{
        size_t sz = sizeof(rlhash) - sizeof(size_t) * RL_NALLOC_INITIAL;
        sz += nelems * sizeof(size_t);
        return sz;
}

/*
 * For a region list which is nelems long, return how much space it will
 * consume, *without* the auxilliary rlhash structure.
 */
static size_t
rlwo_sz(size_t nelems) 
{
        size_t sz = sizeof(regionl) - sizeof(region) * RL_NALLOC_INITIAL;
        /* extra slots for RL_FREE_HD, RL_FREE_TL, ... */
        sz += (nelems + RL_FREE_OVERHEAD) * sizeof(region);
        return sz;
}


/*
 * For a region list which is nelems long,
 * return how much space the rl will consume.
 */
static size_t
rl_sz(size_t nelems)
{
    static size_t sz;
    static size_t last_nelems = 0;
    /* cache value, since number of products is fixed by pqcreate */
    if(nelems != last_nelems) {
        sz = rlwo_sz(nelems) + rlhash_sz(rlhash_nchains(nelems));
        last_nelems = nelems;
    }
    return sz;
}

/* 
 * Hash function for offset.
 */
static size_t 
rl_hash(size_t nchains, const off_t offset) 
{
    unsigned int n = offset;
    return n % nchains;
}

/*
 * Initialize an rlhash, with all chains empty.
 */
static void
rlhash_init(rlhash *const rlhp, size_t const nchains)
{
    size_t i;
        
    rlhp->magic = RL_MAGIC;     /* used to check we have mapped it right */
    for(i = 0; i < nchains; i++) {
        rlhp->chains[i] = RL_NONE;
    }
    return;
}

/*
 * Initialize freelist skip list by offset.
 */
static void
rl_foff_init(regionl *const rl)
{
    region *rlrp = rl->rp;
    static off_t huge_off_t =
         ((off_t)1 << (sizeof(off_t)*CHAR_BIT - 2)) +
        (((off_t)1 << (sizeof(off_t)*CHAR_BIT - 2)) - 1);
    region *foff_hd;
    region *foff_tl;
    int maxlevel;
    fblk_t *fblkp;
    int i;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    /* create psuedo-regions to use for head and tail of freelist
       skip list by offset, makes list maintenance cleaner */
#define RL_FOFF_HD 0
#define RL_FOFF_TL 1
    foff_hd = rlrp + RL_FOFF_HD; /* head of skip list by offset */
    foff_hd->offset = 0;
    foff_hd->extent = 0;
    maxlevel = fbp->maxsize - 1; /* maximum level of fblks */
    foff_hd->next = fb_get(fbp, maxlevel);
    foff_hd->prev = 0;  /* not used */

    foff_tl = rlrp + RL_FOFF_TL; /* tail of skip list by offset */
    foff_tl->offset = huge_off_t;
    foff_tl->extent = 0;
    foff_tl->next = fb_get(fbp, 0); /* not used */
    foff_tl->prev = 0; /* not used */

    /* set all forward "pointers" of RL_FOFF_HD to RL_FOFF_TL */
    fblkp = &fbp->fblks[foff_hd->next];
    for(i=0; i < fbp->maxsize; i++) {
        fblkp[i] = RL_FOFF_TL;
    }

    rl->level_foff = 0;
    rl->foff = RL_FOFF_HD;
}

/*
 * Initialize freelist skip list by extent.
 */
static void
rl_fext_init(regionl *const rl)
{
    region *rlrp = rl->rp;
    static size_t huge_size_t = 0;
    region *fext_hd;
    region *fext_tl;
    int maxlevel;
    fblk_t *fblkp;
    int i;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    if(huge_size_t == 0) {      /* to mark end of skip list sorted by extent */
        /*
         * ULLONG_MAX is not used to set "huge_size_t" because some compilers
         * can't handle it (e.g. SunOS 5.8's /opt/SUNWspro/bin/c89).  SRE
         * 2002-10-18.
         */
        huge_size_t = ~huge_size_t;

        assert(huge_size_t > 0);
    }
    
    /* create psuedo-regions to use for head and tail of freelist
       skip list by extent, makes list maintenance cleaner */
#define RL_FEXT_HD 2
#define RL_FEXT_TL 3
    fext_hd = rlrp + RL_FEXT_HD; /* head of skip list by extent */
    fext_hd->offset = 0;
    fext_hd->extent = 0;
    maxlevel = fbp->maxsize - 1; /* maximum level of fblks */
    fext_hd->next = 0;  /* not used */
    fext_hd->prev = fb_get(fbp, maxlevel);

    fext_tl = rlrp + RL_FEXT_TL; /* tail of skip list by extent */
    fext_tl->offset = 0;
    fext_tl->extent = huge_size_t;
    clear_IsAlloc(fext_tl);
    fext_tl->next = 0;  /* not used */
    fext_tl->prev = fb_get(fbp, 0); /* not used */

    /* set all forward "pointers" of RL_FEXT_HD to RL_FEXT_TL */
    fblkp = &fbp->fblks[fext_hd->prev];
    for(i=0; i < fbp->maxsize; i++) {
        fblkp[i] = RL_FEXT_TL;
    }

    rl->level_fext = 0;
    rl->fext = RL_FEXT_HD;
}

/*
 * Initialize a regionl and its associated rlhash for looking up by offset.
 */
static void
rl_init(regionl *const rl, size_t const nalloc, fb *fbp)
{
    region *rlrp = rl->rp;
    region *const end = rlrp + (nalloc + RL_FREE_OVERHEAD);
    rlhash *rlhp;

    assert(fbp->magic == FB_MAGIC); /* sanity check */

    rlhp = (rlhash *)end;       /* associated chains */
    
    rl->nalloc = nalloc;
    rl->nchains = rlhash_nchains(nalloc);
    rlhash_init(rlhp, rl->nchains);
    
    assert(rlhp->magic == RL_MAGIC); /* sanity check */
    
    rp_init(rl);                /* create list of empty region slots */
    rl->empty = RL_EMPTY_HD;    /* rp array starts out as all empty list */
    rl->nelems = 0;
    rl->maxelems = rl->nelems;
    rl->nempty = nalloc;
    rl->minempty = rl->nempty;
    rl->nbytes = 0;
    rl->maxbytes = rl->nbytes;
    /* cache offset to skip list blocks, so we can find them from only rl */
    rl->fbp_off = (char *)fbp - (char *)rl;

    rl->nfree = 0;
    rl->maxfree = rl->nfree;
    rl->maxfextent = 0;
    
    rl_foff_init(rl);
    rl_fext_init(rl);

    /* This assertion should always be preserved by conversions among
       used, free and empty regions */
    assert(rl->nelems + rl->nfree + rl->nempty == rl->nalloc);
    return;
}

/*
 * Affirm that that another element can be added to rl.
 */
static int
rl_HasSpace(const regionl *const rl)
{
        return (rl->nempty > 0);
}


/*
 * Find previous region by extent on freelist using extent skip list, in 
 * O(log nfree) time, where nfree is number of regions on freelist.
 * rl is pointer to region list.
 * rlix is index in associated region array (rl->rp) of the free region.
 * Returns index in associated array of the previous free region by extent, or
 * RL_FEXT_HD if this is the region on the freelist with the smallest extent.
 */
static size_t
rl_fext_prev(regionl *const rl, size_t rlix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    size_t spix;
    size_t sqix;
    region *spp;
    region *sqp;
    int k;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    assert(IsFree(rep));
    assert(fbp->magic == FB_MAGIC); /* check for sanity */

    spix = rl->fext;            /* head of skip list by extent, p */
    spp = rlrp + spix;
    k = rl->level_fext;
    do {
        /* q = p->forward[k]; */
        sqix = fbp->fblks[spp->prev + k];
        sqp = rlrp + sqix;
        /*      while(q->key < key) { */
        while(sqp->extent < rep->extent 
              || (sqp->extent == rep->extent && sqp->offset < rep->offset)) {
            spix = sqix;
            spp = sqp;
            /* q = p->forward[k]; */
            sqix = fbp->fblks[spp->prev + k];
            sqp = rlrp + sqix;
        }
    } while(--k >= 0);
    /* now p is spix is previous by extent */

#if !defined(NDEBUG)
    {
        region *left = rlrp + spix;

        assert(IsFree(left));
        assert(left->extent <= rep->extent);
    }
#endif

    return spix;
}


/*
 * Recompute the maximum extent of all the regions on the freelist,
 * rl->maxfextent, in O(log(nfree)) time.  Used after taking the free
 * region with maximum extent off of the freelist.  
*/
static size_t
rl_maxfextent(regionl *const rl) {
    region *rlrp = rl->rp;
    size_t rmix;                /* index of region with maximum extent */
    region *rmp;                /* region with maximum extent */

    rmix = rl_fext_prev(rl, RL_FEXT_TL);
    rmp = rlrp + rmix;

    return rmp->extent;
}


/*
 * Delete free region rlix from skip list by offset in O(log(nfree)) time.
 */
static void
rl_foff_del(regionl *rl, size_t rlix)
{
    int k, m;
    size_t spix;
    size_t sqix;
    const region *spp;
    const region *sqp;
    size_t update[MAXLEVELS];
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    /* p = l->header; */
    spix = rl->foff;    /* head of skip list by offset */
    spp = rlrp + spix;
    /* k = l->level; */
    m = rl->level_foff;
    k = m;
    do {
        /* q = p->forward[k]; */
        sqix = fbp->fblks[spp->next + k];
        sqp = rlrp + sqix;
        assert((sqix == RL_FOFF_TL) || (rl->foff < sqix && sqix < rl->nalloc + RL_FREE_OVERHEAD));
        /* while(q->key < key) { */
        while(sqp->offset < rep->offset) {
            spix = sqix;
            spp = sqp;
            /* q = p->forward[k]; */
            sqix = fbp->fblks[spp->next + k];
            sqp = rlrp + sqix;
        }
        update[k] = spix;
    } while(--k >= 0);
    /* q may have key equal or greater than the specified key.  */
    assert((sqix == RL_FOFF_TL) || (rl->foff < sqix && sqix < rl->nalloc + RL_FREE_OVERHEAD));
    /* if (q->key == key) { */
    if (sqp->offset == rep->offset) {
        for(k = 0; k <= m; k++) {
            spix = update[k];
            spp = rlrp + spix;
            /* if (p->forward[k] != q) { */
            if (fbp->fblks[spp->next + k] != sqix) {
                break;
            }
            /* p->forward[k] = q->forward[k]; */
            fbp->fblks[spp->next + k] = fbp->fblks[sqp->next + k];
        }
        /* free associated fblk */
        fb_rel(fbp, k, sqp->next);
        
        /* update level of list, in case we just deleted highest level */
        /* while( l->header->forward[m] == NIL && m > 0 ) { */
        spix = rl->foff;
        spp = rlrp + spix;
        while(fbp->fblks[spp->next + m] == RL_FOFF_TL && m > 0) {
            m--;
        }
        rl->level_foff = m;
    }
}

/*
 * Delete free region rlix from skip list by extent in O(log(nfree)) time.
 */
static void
rl_fext_del(regionl *rl, size_t rlix)
{
    int k, m;
    size_t spix;
    size_t sqix;
    const region *spp;
    const region *sqp;
    size_t update[MAXLEVELS];
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    /* p = l->header; */
    spix = rl->fext;    /* head of skip list by extent */
    spp = rlrp + spix;
    /* k = l->level; */
    m = rl->level_fext;
    k = m;
    do {
        /* q = p->forward[k]; */
        sqix = fbp->fblks[spp->prev + k];
        sqp = rlrp + sqix;
        assert((sqix == RL_FEXT_TL) || (rl->fext < sqix && sqix < rl->nalloc + RL_FREE_OVERHEAD));
        /* while(q->key < key) { */
        assert(sqp->extent > 0);
        /* regions with equal extents may be on freelist, so need to find right one to delete */
        while(sqp->extent < rep->extent
              || (sqp->extent == rep->extent && sqp->offset < rep->offset)) {
            spix = sqix;
            spp = sqp;
            /* q = p->forward[k]; */
            sqix = fbp->fblks[spp->prev + k];
            sqp = rlrp + sqix;
        }
        update[k] = spix;
    } while(--k >= 0);
    /* q may have key equal or greater than the specified key.  */
    /* if (q->key == key) { */
    if (sqp->extent == rep->extent) {
        for(k = 0; k <= m; k++) {
            spix = update[k];
            spp = rlrp + spix;
            /* if (p->forward[k] != q) { */
            if (fbp->fblks[spp->prev + k] != sqix) {
                break;
            }
            /* p->forward[k] = q->forward[k]; */
            fbp->fblks[spp->prev + k] = fbp->fblks[sqp->prev + k];
        }
        /* free associated fblk */
        sqp = rlrp + sqix;
        fb_rel(fbp, k, sqp->prev);
        
        /* update level of list, in case we just deleted highest level */
        /* while( l->header->forward[m] == NIL && m > 0 ) { */
        spix = rl->fext;
        spp = rlrp + spix;
        while(fbp->fblks[spp->prev + m] == RL_FEXT_TL && m > 0) {
            m--;
        }
        rl->level_fext = m;
    }
}


/*
 * Find best-fit free region from skip list by extent in O(log(nfree)) time.
 */
static size_t
rl_fext_find(regionl *rl, size_t extent)
{
    int k;
    size_t spix;
    size_t sqix;
    const region *spp;
    const region *sqp;
    region *rlrp = rl->rp;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    /* p = l->header; */
    spix = rl->fext;    /* head of skip list by extent */
    spp = rlrp + spix;
    /* k = l->level; */
    k = rl->level_fext;
    do {
        /* q = p->forward[k]; */
        sqix = fbp->fblks[spp->prev + k];
        sqp = rlrp + sqix;
        /* while(q->key < key) { */
        /* regions with equal extents may be on freelist, so need to find right one to delete */
        while(sqp->extent < extent) {
            spix = sqix;
            spp = sqp;
            /* q = p->forward[k]; */
            sqix = fbp->fblks[spp->prev + k];
            sqp = rlrp + sqix;
        }
    } while(--k >= 0);
    /* q may have key equal or greater than the specified key.  */
    return sqix;
}


/*
 * Get index of an available region for a specified extent off the
 * list of free regions, using a best fit algorithm.  Returns
 * RL_NONE if none available.  
 */
static size_t
rl_get(regionl *const rl, size_t extent) 
{
    region *rlrp = rl->rp;
    region *rep;
    size_t sqbest;              /* index of best fit */

    if(extent > rl->maxfextent)
        return RL_NONE;

    sqbest = RL_NONE;

    sqbest = rl_fext_find(rl, extent);
    if(sqbest == RL_FEXT_TL) {
        return RL_NONE;
    }
    rep = rlrp + sqbest;

    /* Remove free region from offset and extent skip lists */
    rl_foff_del(rl, sqbest);
    rl_fext_del(rl, sqbest);

    rl->nfree--;
    if(rep->extent == rl->maxfextent) { /* recompute maxfextent from remaining 
                                           freelist regions */
        rl->maxfextent = rl_maxfextent(rl);
    }
    rl->nelems++;
    if (rl->nelems > rl->maxelems)
        rl->maxelems = rl->nelems;
    return sqbest;
}

/* rlhash must be aligned to start right after pq->rlp */
#define RLHASHP(rl) ((rlhash *)(&(rl)->rp[(rl)->nalloc + RL_FREE_OVERHEAD]))

/*
 * Delete elem from region hashtable by offset.
 */
static void
rlhash_del(regionl *const rl, size_t rlix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    size_t rpix = rep->prev;    /* previous element */
    region *rpp;
    size_t rnix = rep->next;    /* next element */

    if(rpix != RL_NONE) {
        rpp = rlrp + rpix;
        rpp->next = rnix;
    } else {                    /* deleting head of chain */
        size_t try;
        rlhash *rlhp;

        try = rl_hash(rl->nchains, rep->offset);
        rlhp = RLHASHP(rl);
        rlhp->chains[try] = rnix;
    }
    if(rnix != RL_NONE) {
        region *rnp = rlrp + rnix;
        assert(IsAlloc(rnp));
        rnp->prev = rpix;
    }
    return;
}

/*
 * Add recycled region to freelist skip list by offset in O(log(nfree))
 * time.
 */
static void
rl_foff_add(regionl *const rl, size_t rlix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    int k;
    size_t update[MAXLEVELS];
    size_t spix;
    size_t sqix;
    region *spp;
    region *sqp;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);
    
    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    spix = rl->foff;            /* head of skip list by offset, p */
    spp = rlrp + spix;
    k = rl->level_foff;
    do {
        /* q = p->forward[k]; */
        sqix = fbp->fblks[spp->next + k];
        sqp = rlrp + sqix;
        /*      while(q->key < key) { */
        while(sqp->offset < rep->offset) {
            spix = sqix;
            spp = sqp;
            /* q = p->forward[k]; */
            sqix = fbp->fblks[spp->next + k];
            sqp = rlrp + sqix;
        }
        update[k] = spix;
    } while(--k >= 0);
                                /* found where to put new node, after spix & before sqix */
    k = fb_ranlev(fbp);
    /* Note, following hack limits increment in level to 1, messes up
       distribution of random levels slightly.  This could be left out
       for "purist" implementation. */
    if (k > rl->level_foff) {
        rl->level_foff++;
        k = rl->level_foff;
        update[k] = rl->foff;
    }
    /* get new fblk of level k */
    rep->next = fb_get(fbp, k);
    do {
        spix = update[k];
        spp = rlrp + spix;
        /* q->forward[k] = p->forward[k]; */
        fbp->fblks[rep->next + k] = fbp->fblks[spp->next + k];
        /* p->forward[k] = q; */
        fbp->fblks[spp->next + k] = rlix; /* forward pointer to new region */
    } while(--k >= 0);
}

/*
 * Add recycled region to freelist skip list by extent in O(log(nfree))
 * time.
 */
static void
rl_fext_add(regionl *const rl, size_t rlix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    int k;
    size_t update[MAXLEVELS];
    size_t spix;
    size_t sqix;
    region *spp;
    region *sqp;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);
    
    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    spix = rl->fext;            /* head of skip list by extent, p */
    spp = rlrp + spix;
    k = rl->level_fext;
    /* Can have multiple identical extents in list */
    do {
        /* q = p->forward[k]; */
        sqix = fbp->fblks[spp->prev + k];
        sqp = rlrp + sqix;
        /*      while(q->key < key) { */
        while(sqp->extent < rep->extent
              || (sqp->extent == rep->extent && sqp->offset < rep->offset)) {
            spix = sqix;
            spp = sqp;
            /* q = p->forward[k]; */
            sqix = fbp->fblks[spp->prev + k];
            sqp = rlrp + sqix;
        }
        update[k] = spix;
    } while(--k >= 0);
                                    /* found where to put new node, after spix & before sqix */
    k = fb_ranlev(fbp);
    /* Note, following hack limits increment in level to 1, messes up
       distribution of random levels slightly.  This could be left out
       for "purist" implementation. */
    if (k > rl->level_fext) {
        rl->level_fext++;
        k = rl->level_fext;
        update[k] = rl->fext;
    }
    /* get new fblk of level k */
    rep->prev = fb_get(fbp, k);
    do {
        spix = update[k];
        spp = rlrp + spix;
        /* q->forward[k] = p->forward[k]; */
        fbp->fblks[rep->prev + k] = fbp->fblks[spp->prev + k];
        /* p->forward[k] = q; */
        fbp->fblks[spp->prev + k] = rlix; /* forward pointer to new region */
    } while(--k >= 0);
}


#if 0
static void
rl_foff_dump(regionl *const rl)
{
    region *rlrp = rl->rp;
    region *rep;
    size_t spix;
    size_t sqix;
    const region *spp;
    const region *sqp;
    fb *fbp = rl->fbp;
    off_t prev_offset = 0;

    /* p = l->header; */
    spix = rl->foff;    /* head of skip list by offset */
    spp = rlrp + spix;
    /* q = p->forward[0]; */
    sqix = fbp->fblks[spp->next];
    sqp = rlrp + sqix;
    udebug("** Offsets:\t");                    /* debugging */
    while(sqix != RL_FOFF_TL) {
        /* p = q */
        spix = sqix;
        spp = rlrp + spix;
        udebug("%u ", spp->offset);             /* debugging */
        assert(spp->offset > prev_offset);
        prev_offset = spp->offset;
        /* q = p->forward[0]; */
        sqix = fbp->fblks[spp->next];
        sqp = rlrp + sqix;
    }
}
#endif


/*
 * Return region with index rlix to the free list.
 */
static void
rl_rel(regionl *const rl, size_t rlix) 
{
    rl_foff_add(rl, rlix);      /* add to freelist skip list by offset */
    rl_fext_add(rl, rlix);      /* add to freelist skip list by extent */

    rl->nfree++;
    return;
}

/*
 * Find next region by offset on freelist using offset skip list, in O(1) time.
 * rl is pointer to region list.
 * rlix is index in associated region array (rl->rp) of the free region.
 * Returns index in associated array of the next free region by offset, or
 * RL_FOFF_TL if this is the region on the freelist with the largest offset.
 */
static size_t
rl_foff_next(regionl *const rl, size_t rlix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    size_t rnix;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    assert(IsFree(rep));
    assert(fbp->magic == FB_MAGIC); /* check for sanity */
    rnix = fbp->fblks[rep->next];

#if !defined(NDEBUG)
    {
        region *rght = rlrp + rnix;

        assert(IsFree(rght));
        assert(rght->offset > rep->offset);
    }
#endif

    return rnix;
}

/*
 * Find previous region by offset on freelist using offset skip list, in 
 * O(log nfree) time, where nfree is number of regions on freelist.
 * rl is pointer to region list.
 * rlix is index in associated region array (rl->rp) of the free region.
 * Returns index in associated array of the previous free region by offset, or
 * RL_FOFF_HD if this is the region on the freelist with the smallest offset.
 */
static size_t
rl_foff_prev(regionl *const rl, size_t rlix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rlix;
    size_t spix;
    size_t sqix;
    region *spp;
    region *sqp;
    int k;
    fb *fbp = (fb *)((char *)rl + rl->fbp_off);

    assert(IsFree(rep));
    assert(fbp->magic == FB_MAGIC); /* check for sanity */

    spix = rl->foff;            /* head of skip list by offset, p */
    spp = rlrp + spix;
    k = rl->level_foff;
    do {
        /* q = p->forward[k]; */
        sqix = fbp->fblks[spp->next + k];
        sqp = rlrp + sqix;
        /*      while(q->key < key) { */
        while(sqp->offset < rep->offset) {
            spix = sqix;
            spp = sqp;
            /* q = p->forward[k]; */
            sqix = fbp->fblks[spp->next + k];
            sqp = rlrp + sqix;
        }
    } while(--k >= 0);
    /* now p is spix is previous by offset */

#if !defined(NDEBUG)
    {
        region *left = rlrp + spix;

        assert(IsFree(left));
        assert(left->offset < rep->offset);
    }
#endif

    return spix;
}


/*
 * Given a newly free'd region at rpix, merge it with any free neighbors.
 * We are using "consolidate on free" strategy.
 */
static void
rl_consolidate(regionl *const rl, size_t rpix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rpix;
    size_t rghtix;
    size_t leftix;
    int nmerges = 0;

    rghtix = rl_foff_next(rl, rpix); /* if last, returns RL_FOFF_TL */
    leftix = rl_foff_prev(rl, rpix); /* if first, returns RL_FOFF_HD */
    
    if(rghtix != RL_FOFF_TL) { /* not last free region */
        region *rght = rlrp + rghtix;
        if(rep->offset + rep->extent == rght->offset) { /* mergeable */
            rl_fext_del(rl, rpix); /* since extent will change, delete from extent skip list first */
            rep->extent += rght->extent;
            rl_fext_add(rl, rpix); /* reinsert to keep extent skip list sorted by extent */
            rl->nfree--;
            rl_foff_del(rl, rghtix);
            rl_fext_del(rl, rghtix);
            rp_rel(rl, rghtix); /* now put right back in empty region slots */
            nmerges++;
        }
    }
    if(leftix != RL_FOFF_HD) { /* not first region */
        region *left = rlrp + leftix;
        if(left->offset + left->extent == rep->offset) /* mergeable */ {
            rl_fext_del(rl, leftix); /* since extent will change, delete from extent skip list first */
            left->extent += rep->extent;
            rl_fext_add(rl, leftix); /* reinsert to keep extent skip list sorted by extent */
            rl->nfree--;
            rl_foff_del(rl, rpix);
            rl_fext_del(rl, rpix);
            rp_rel(rl, rpix); /* put back in empty region slots */
            nmerges++;
            rep = left;
        }
    }
    if(rep->extent > rl->maxfextent)
        rl->maxfextent = rep->extent;
}


/*
 * Search the region list 'rl' for an in-use region whose offset is
 * 'offset'.  Returns the index in rl->rp[] of the region if found.
 * Otherwise, returns RL_NONE.  
 */
static size_t
rl_find(const regionl *const rl, off_t const offset)
{
    const region* rep;
    size_t try;
    size_t next;
    rlhash *rlhp;
    const region *rlrp = rl->rp;
    size_t ret;

    rlhp = RLHASHP(rl);
    assert(rlhp->magic == RL_MAGIC);
    
    ret = RL_NONE;
    try = rl_hash(rl->nchains, offset);
    next = rlhp->chains[try];
    while (next != RL_NONE) {
        rep = rlrp + next;
        if(offset == rep->offset) { /* found */
            ret = next;
            assert(IsAlloc(rep)); /* verify region is in use */
            break;
        }
        next = rep->next;
    }
    return ret;
}

/*
 * Search the regionl 'rl' for an in-use region whose offset is
 * 'offset'.  Returns 1 and sets *rpp to match if found.  Otherwise,
 * returns 0 and sets *rpp to NULL.  (For backward compatibility with
 * a previous interface) 
*/
static int
rl_r_find(regionl *const rl, off_t const offset, region **rpp)
{
        size_t rlix;
        region *rlrp = rl->rp;

        rlix = rl_find(rl, offset);
        if(rlix == RL_NONE) {
            *rpp = NULL;
            return 0;
        }
        *rpp = rlrp + rlix;
        return 1;
}


/*
 * Add in-use region to region hashtable by offset.
 */
static void
rlhash_add(regionl *const rl, size_t rpix)
{
    region *rlrp = rl->rp;
    region *rep;
    size_t try;
    size_t next;
    rlhash *rlhp;
    rlhp = RLHASHP(rl);
    assert(rlhp->magic == RL_MAGIC);

    rep = rlrp + rpix;
    assert(IsAlloc(rep));
    try = rl_hash(rl->nchains, rep->offset);
    /* link new element on front of chain */
    next = rlhp->chains[try];
    if (next != RL_NONE) {
        region *rnp = rlrp + next;
        assert(IsAlloc(rnp));
        rnp->prev = rpix;
    }
    rep->next = next;
    rep->prev = RL_NONE;
    rlhp->chains[try] = rpix;

    return;
}


/*
 * Allocate a new region and add it to free region list.
 * Return NULL if no more region slots left.
 */
static region *
rl_add(regionl *const rl, off_t const offset, size_t const extent)
{
    region *rlrp = rl->rp;
    region *rep = NULL;
    size_t rpix;

    rpix = rp_get(rl);          /* get an empty region */
    if (rpix == RL_NONE) {
        /* This shouldn't happen if enough product slots are allocated
           by pqcreate ... */
        uerror("Need more product slots, allocate more when creating queue");
        /* Can't call pq_del_oldest(), because that's who might have called us */
        return NULL;
    }
    assert(rl->nelems < rl->nalloc);
    
    rep = rlrp + rpix;
    rep->offset = offset;
    rep->extent = extent;

    rl_rel(rl, rpix);   /* Insert into free list.  No need to consolidate. */
    if(rl->nfree > rl->maxfree)
        rl->maxfree = rl->nfree;
    assert(rl->nelems + rl->nfree + rl->nempty == rl->nalloc);
    return rep;
}

/*
 * Split the soon to be in-use region indexed by rlix, into two
 * extents, putting the remainder back in free table.  If there are no
 * more empty slots from which to make a new region, returns ENOMEM,
 * otherwise ENOERR.  
*/
static int
rl_split(regionl *const rl, size_t rlix, size_t const extent)
{
        region *rlrp = rl->rp;
        region *new;
        off_t newoff;
        size_t rem;
        int status = ENOERR;

        region *low = rlrp + rlix;
        assert(low != NULL);
        assert(IsFree(low));
        assert(extent <= low->extent);

        rem = low->extent - extent;
        newoff = low->offset + (off_t)extent;
        new = rl_add(rl, newoff, rem);
        if(new) {
            assert(IsFree(new));
            low->extent = extent; /* can change extent, because this is not on freelist */
            if(rem > rl->maxfextent)
                rl->maxfextent = rem;
        } else {                /* out of empty slots, not enough allocated */
            status = ENOMEM;
        }
        return status;
}


/*
 * Low level region free.
 * Mark region indexed by rpix as free, consolidate, and add to the free list.
 */
static void
rl_free(regionl *const rl, size_t rpix)
{
    region *rlrp = rl->rp;
    region *rep = rlrp + rpix;

    clear_IsAlloc(rep);
    rl->nbytes -= rep->extent;
    rlhash_del(rl, rpix);
    rl->nelems--;
    rl_rel(rl, rpix);           /* add to skip list indices by offset and extent */
    rl_consolidate(rl, rpix);

    /* update statistics */
    if(rl->nfree > rl->maxfree)
        rl->maxfree = rl->nfree;
    assert(rl->nelems + rl->nfree + rl->nempty == rl->nalloc);
}
/* End regionl */
/* Begin sx */

/*
 * The last index is by "signature".
 * This is a 128 bit md5 checksum on the
 * _data_ portion of a product.
 * This index is used for duplicate detection and
 * suppression.
 *
 * The implementation uses hashing with chaining.  (Open chaining
 * using double hashing won't work, because deletions are as common
 * as searching and insertion; every signature is eventually deleted.)  
 */

#define SX_NONE ((size_t)(-1))

/* Tuning parameter, expected length of hash chain lists, hence the
 * expected number of list elements to be examined in an unsuccessful
 * search.  Making this smaller will decrease signature insertion,
 * deletion, and find times at the expense of more space in the queue
 * to hold a larger number of hash chain lists.  */
#define SX_EXP_CHAIN_LEN  4

struct sxelem {
  signaturet sxi;             /* the signature of a product (128-bit MD5) */
  off_t offset;               /* of product associated with this signature */
  size_t next;                /* for linking sxelems on lists */
};
typedef struct sxelem sxelem;

/* The array of sxelems is both a threaded list of free sxelems and
 * the chains of sxelems that hash to the same bin
 * (sxhash->chains[i]).  */
struct sx {
#define SX_NALLOC_INITIAL       9
  size_t nalloc;                  /* including free list elements */
  size_t nelems;                  /* current number of signatures  */
  size_t nchains;                 /* actual number of chain slots */
  size_t free;                    /* index of free list for signatures */
  size_t nfree;                   /* number of free slots left */
  sxelem sxep[SX_NALLOC_INITIAL]; /* actually nalloc long */
};
typedef struct sx sx;

/* Heads of hash chain lists.  The size of this struct depends on the
 * number of products (pq->nalloc).  It is placed directly after the
 * sx struct.  */
struct sxhash {
#define SX_MAGIC        0x53584841
  size_t magic;                 /* "SXHA" to check alignment, endianness */
#define SXHASH_NALLOC_INITIAL   2
  size_t chains[SXHASH_NALLOC_INITIAL]; /* heads of lists of sxelems */
};
typedef struct sxhash sxhash;

/*
 * Returns number of chains required for the specified number of elements.
 */
static size_t
nchains(size_t const nelems) 
{
  return prevprime(nelems / SX_EXP_CHAIN_LEN);
}

/*
 * For an sxhash which is nelems long, return how much space it will
 * consume.
 */
static size_t
sxhash_sz(size_t nelems)
{
        size_t sz = sizeof(sxhash) - sizeof(off_t) * SXHASH_NALLOC_INITIAL;
        sz += nelems * sizeof(off_t);
        return sz;
}

/*
 * For a sx which is nelems long, return how much space it will
 * consume, *without* the auxilliary sxhash structure.
 */
static size_t
sxwo_sz(size_t nelems) 
{
        size_t sz = sizeof(sx) - sizeof(sxelem) * SX_NALLOC_INITIAL;
        sz += nelems * sizeof(sxelem);
        return sz;
}

/*
 * For a sx which is nelems long, return how much space it will
 * consume, including the auxilliary sxhash structure.
 */
static size_t
sx_sz(size_t nelems) 
{
    static size_t sz;
    static size_t last_nelems = 0;
    if(nelems != last_nelems) {
        last_nelems = nelems;
        sz = sxwo_sz(nelems) + sxhash_sz(nchains(nelems));
    }
    return sz;
}

/* 
 * Hash function for signature.
 */
static size_t 
sx_hash(size_t nchains, const signaturet sig) 
{
  size_t h;
  int i;
  unsigned int n;

  n = 0;
  for(i=0; i<4; i++)
    n = 256*n + sig[i];
  h = n % nchains;
  return h;
}

/*
 * Initialize an sxhash, with all chains empty.
 */
static void
sxhash_init(sxhash *const sxhp, size_t const nchains)
{
  size_t i;
        
  sxhp->magic = SX_MAGIC;       /* used to check we have mapped it right */
  for(i = 0; i < nchains; i++) {
    sxhp->chains[i] = SX_NONE;
  }
  return;
}


/*
 * Initialize an sx (and its associated sxhash).
 * We define number of chains so that expected length of each chain will be
 * SX_EXP_CHAIN_LEN.
 */
static void
sx_init(sx *const sx, size_t const nalloc)
{
        sxelem *sxep;
        sxelem *const end = &sx->sxep[nalloc];
        sxhash *sxhp;
        off_t isx = 1;

        sxhp = (sxhash *)end;   /* associated chains */

        sx->nalloc = nalloc;
        sx->nelems = 0;
        sx->nchains = nchains(nalloc);
        sxhash_init(sxhp, sx->nchains);

        assert(sxhp->magic == SX_MAGIC); /* sanity check */

        for(sxep = &sx->sxep[0]; sxep < end; sxep++, isx++)
        {
                memset(sxep->sxi, 0, sizeof(signaturet));
                sxep->offset = OFF_NONE;
                sxep->next = isx; /* link up free list */
        }
        sxep = &sx->sxep[isx-2];
        sxep->next = SX_NONE;     /* reset last pointer to end of free list */
        sx->free = 0;             /* sxep array starts out as all free list */
        sx->nfree = nalloc;
        return;
}

/*
 * Comparison function used in sx_find() below.  
 * Returns 1 if sig1 equals sig2, 0 otherwise.
 */
static int
sx_compare(const signaturet sig1, const signaturet sig2)
{
  return 0 == memcmp(sig1, sig2, sizeof(signaturet));
}

/*
 * Get index of an available sxelem off the free list.
 * Returns SX_NONE if none available.
 */
static size_t
sxelem_new(sx *const sx) 
{
    size_t avail;
    sxelem *sxep;

    if (sx->nfree == 0) {
        return SX_NONE;
    }
    avail = sx->free;
    sxep = &sx->sxep[avail];
    sx->free = sxep->next;
    sx->nfree--;
    return avail;
}

/*
 * Return sxelem[sxix] to the free list.
 */
static void
sxelem_free(sx *const sx, size_t sxix) 
{
    sxelem *sxep = &sx->sxep[sxix];
    sxep->offset = OFF_NONE;
    sxep->next = sx->free;
    sx->free = sxix;
    sx->nfree++;
}

/*
 * Search the index 'sx' for signature.
 * Returns 1 and sets *sxepp to match if found.
 * Otherwise, returns 0.
 */
static int
sx_find(sx *const sx, const signaturet sig, sxelem **sxepp)
{
    sxelem* sxep;
    size_t try;
    size_t next;
    sxhash *sxhp;
    int status = 0;
        /* sxhp = (sxhash *)((char *)(sx) + sxwo_sz(sx->nalloc)); */
    sxhp = (sxhash *)(&sx->sxep[sx->nalloc]);
    assert(sxhp->magic == SX_MAGIC);
    
    *sxepp = (sxelem *) 0;
    
    try = sx_hash(sx->nchains, sig);
    next = sxhp->chains[try];
    while (next != SX_NONE) {
        sxep = &sx->sxep[next];
        if(sx_compare(sig, sxep->sxi)) { /* found */
            *sxepp = sxep;
            status = 1;
            break;
        }
        next = sxep->next;
    }
    return status;
}

/*
 * Add elem to (signature, offset) hashtable.
 * Returns added elem, or NULL if no space left to add
 */
static sxelem *
sx_add(sx *const sx, const signaturet sig, off_t const offset)
{
    sxelem* sxep;
    size_t sxix;
    size_t try;
    size_t next;                /* head of a list of signatures */
    sxhash *sxhp;
    /* sxhp = (sxhash *)((char *)(sx) + sxwo_sz(sx->nalloc)); */
    sxhp = (sxhash *)(&sx->sxep[sx->nalloc]);
    assert(sxhp->magic == SX_MAGIC);
    
    assert(sx->nalloc != 0);
    assert(sx->nfree + sx->nelems == sx->nalloc);
    
    /* get a new sxelem from the front of free list */
    sxix = sxelem_new(sx);
    if (sxix == SX_NONE) {
        uerror("sx_add: no slots for signatures, too many products?\n");
        return 0;
    }
    sxep = &sx->sxep[sxix];
    memcpy((void *)sxep->sxi, (void *)sig, sizeof(signaturet));
    sxep->offset = offset;
    
    try = sx_hash(sx->nchains, sig);
    /* link new element on front of chain */
    next = sxhp->chains[try];
    sxep->next = next;
    sxhp->chains[try] = sxix;
    
    sx->nelems++;
    
    return sxep;
}

/*
 * Find and then delete from index.
 * Returns 1 if found and deleted, returns 0 if not found.
 */
static int
sx_find_delete(sx *const sx, const signaturet sig) 
{
    sxelem* sxep;
    sxelem* osxep;
    size_t try;
    size_t next;
    sxhash *sxhp;
    int status = 0;
    /* sxhp = (sxhash *)((char *)(sx) + sxwo_sz(sx->nalloc)); */
    sxhp = (sxhash *)(&sx->sxep[sx->nalloc]);
    assert(sxhp->magic == SX_MAGIC);
    assert(sx->nfree + sx->nelems == sx->nalloc);

    /* find chain */
    try = sx_hash(sx->nchains, sig);
    next = sxhp->chains[try];
    sxep = &sx->sxep[next];
    if(sx_compare(sig, sxep->sxi)) { /* found */
        sxhp->chains[try] = sxep->next;
        sxelem_free(sx, next);
        sx->nelems--;
        status = 1;
        return status;
    }
    next = sxep->next;
    while (next != SX_NONE) {
        osxep = sxep;
        sxep = &sx->sxep[next];
        if(sx_compare(sig, sxep->sxi)) { /* found */
            osxep->next = sxep->next;
            sxelem_free(sx, next);
            sx->nelems--;
            status = 1;
            return status;
        }
        next = sxep->next;
    }
    return status;              /* not found */
}

/* End sx */
/* Begin ix */

/*
 * We use 'ix' to refer to the collection of shared
 * indexes at the end of the file.
 * (pq->rlp & pq->tqp)
 */

/*
 * Return the amount of space required to store a
 * collection of indices, each of 'nelems'.
 */
static size_t
ix_sz(size_t nelems, size_t align)
{
    /* cache value, since it only depends on nelems */
    static size_t sz;
    static size_t last_nelems = 0;
    if(nelems != last_nelems) {
        last_nelems = nelems;
        sz = _RNDUP(rl_sz(nelems), align) + _RNDUP(tq_sz(nelems), align) 
           + _RNDUP(fb_sz(nelems), align) + _RNDUP(sx_sz(nelems), align);
    }
    return sz;
}


/*
 * Convert the raw index area 'ix', 'ixsz'
 * into the useful handles.
 */
static int
ix_ptrs(void *ix, size_t ixsz, size_t nelems,
        size_t align, regionl **rlpp, tqueue **tqpp, fb **fbpp, sx **sxpp)
{
        *rlpp = (regionl *)ix;
        *tqpp = (tqueue *) _RNDUP((size_t)((char *)(*rlpp) + rl_sz(nelems)), align);
        *fbpp = (fb *) _RNDUP((size_t)((char *)(*tqpp) + tq_sz(nelems)), align);
        *sxpp = (sx *) _RNDUP((size_t)((char *)(*fbpp) + fb_sz(nelems)), align);
        /* can't set cached tq->fbp and rl->fbp here, because those
           are in mmap'd file, which might be open read-only */
        /*
        ((struct regionl *)*rlpp)->fbp = *fbpp;
        ((struct tqueue *)*tqpp)->fbp = *fbpp;
        */
#ifndef NDEBUG
        assert(((char *)(*sxpp) + sx_sz(nelems)) <= ((char *)ix + ixsz));
#else
        if (!(((char *)(*sxpp) + sx_sz(nelems)) <= ((char *)ix + ixsz))) {
            uerror("ix_ptrs: *sxpp=%p, sx_sz(%lu)=%lu, ix=%p, ixsz=%lu",
                (void*)*sxpp, (unsigned long)nelems,
                (unsigned long)sx_sz(nelems), (void*)ix,
                (unsigned long)ixsz);
            return 0;
        }
#endif
        return 1;
}

/* End ix */
/* Begin bsrch */
/*
 * Code is derived from berkeley bsearch(),
 * But we want to know where we were on failure, so
 * we can do sorted insertions.
 */
/*
 * Perform a binary search.
 *
 * The code below is a bit sneaky.  After a comparison fails, we
 * divide the work in half by moving either left or right. If lim
 * is odd, moving left simply involves halving lim: e.g., when lim
 * is 5 we look at item 2, so we change lim to 2 so that we will
 * look at items 0 & 1.  If lim is even, the same applies.  If lim
 * is odd, moving right again involes halving lim, this time moving
 * the base up one item past p: e.g., when lim is 5 we change base
 * to item 3 and make lim 2 so that we will look at items 3 and 4.
 * If lim is even, however, we have to shrink it by one before
 * halving: e.g., when lim is 4, we still looked at item 2, so we
 * have to make lim 3, then halve, obtaining 1, so that we will only
 * look at item 3.
 */
static int
bsrch(const void *key,
        const void *base0,
        size_t nmemb,
        size_t size,
        int (*compar)(const void *, const void *),
        const void **resultp
        )
{
        const char *base = base0;
        size_t lim;
        int cmp = 0;
        const void *p = base0;

        for (lim = nmemb; lim != 0; lim /= 2) {
                p = base + (lim/2) * size;
                cmp = (*compar)(key, p);
                if (cmp == 0)
                {
                        /* found it */
                        *resultp = p;
                        return 1;
                }
                if (cmp > 0) {  /* key > p: move right */
                        base = (const char *)p + size;
                        lim--;
                }               /* else move left */
        }
        /* didn't find it */
        if(cmp > 0)
                *resultp = (const char *)p + size;
        else    
                *resultp = p;
        return 0;
}
/* End bsrch */
/* Begin riu */

/*
 * This structure is used to keep track of
 * a region which this process has 'in use'.
 * It appears on the process private list (riul *) pq->riulp.
 * 'offset' is the seek offset of the region in the queue file,
 * 'extent' is it's size,
 * 'vp' is the memory handle being used to access the region,
 * and 'rflags' stashes the RGN_* flags with which the region was gotten.
 */
struct riu {
        off_t offset;
        size_t extent;
        void *vp;
        int rflags;
};
typedef struct riu riu;

/* End riu */
/* Begin riul */

/*
 * The process private list of regions in use.
 * Sorted by offset.
 *
 * TODO: This table typically contains 3 to 5 entries (verify).
 * Maintaining sort and using binary search is overkill.
 */
struct riul
{
#define RIU_NALLOC_INITIAL      255
        size_t sz;
        size_t nalloc;
        size_t nelems;
        size_t maxelems;        /* max nelems so far */
        riu rp[RIU_NALLOC_INITIAL]; /* actually nalloc long */
};
typedef struct riul riul;
#define MIN_RIUL_SZ (3 * sizeof(size_t))


/*
 * Return the nalloc (array size) 
 * of a riul which will fit into 'sz'.
 */
static size_t
riul_nalloc(size_t sz)
{
        assert(sz >  MIN_RIUL_SZ);
        sz -= sizeof(size_t); /* sz */
        sz -= sizeof(size_t); /* nalloc */
        sz -= sizeof(size_t); /* nelems */
        sz -= sizeof(size_t); /* maxelems */
        sz /= sizeof(riu);
        return sz;
}


/*
 * Initialize (re Initialize) a riul
 */
static void
riul_init(riul *const rl, size_t const nelems, size_t sz)
{
        riu *rp;
        size_t nalloc = riul_nalloc(sz);
        riu *const end = &rl->rp[nalloc];

        assert(rl != NULL);
        assert(sz != 0);
        assert(nelems == 0 || rl->nelems == nelems);
        assert(nelems == 0 || rl->nalloc < nalloc);

        rl->sz = sz;
        rl->nalloc = nalloc;
        rl->nelems = nelems;
        if(nelems == 0)
            rl->maxelems = 0;
        for(rp = &rl->rp[nelems];
                        rp < end; rp++)
        {
                rp->offset = OFF_NONE;
                rp->extent = 0;
                rp->vp = NULL;
                rp->rflags = 0;
        }
}


/*
 * Affirm that that another element can be added to rl
 */
static int
riul_HasSpace(const riul *const rl)
{
        assert(rl->nelems <= rl->nalloc);
        return (rl->nelems < rl->nalloc);
}


/*
 * Comparison function used by bsrch()
 * in riul_r_find() below.
 * The riul is sorted by region offset.
 */
static int
riul_r_compare(const void *vp1, const void *vp2)
{
        const off_t diff = (((const riu *)vp1)->offset -
                 ((const riu *)vp2)->offset);
        if(diff == 0)
                return 0;
        /* else */
        if(diff < 0)
                return -1;
        /* else */
        return 1;
}


/*
 * Search the riul 'rl' for a riu whose offset is 'offset'.
 * Returns 1 and sets *rpp to match if found.
 * Otherwise, returns 0 and sets *rpp to where it should be found.
 */
static int
riul_r_find(const riul *const rl, off_t const offset, riu *const *rpp)
{
        int status;
        riu rgn;
        rgn.offset = offset;
        status = bsrch(&rgn,
                rl->rp, rl->nelems, sizeof(riu), riul_r_compare,
                        (const void **)rpp);
        assert(status == 0 || (*rpp)->vp != NULL);
        return status;
}


/*
 * add elem to list, maintain sort by offset
 *
 * Returns:
 *      0       Success.
 *      ENOMEM  Insufficient memory is available.
 */
static int
riul_add(riul **riulpp, size_t growby,
        off_t const offset, size_t const extent,
        void *const vp, const int rflags)
{
        riu *rp;
        riul *rl = *riulpp;
        riu *end = &rl->rp[rl->nelems];

        if(!riul_HasSpace(rl))
        {
                /* get space */
                size_t newsz = rl->sz + growby;
                riul *nriulp = (riul *)realloc(rl, newsz);
                if(nriulp == NULL)
                        return errno;
                riul_init(nriulp, nriulp->nelems, newsz);
                *riulpp = rl = nriulp;
        }

#if !defined(NDEBUG)
        {
            int found =
#endif
            riul_r_find(rl, offset, &rp);

#if !defined(NDEBUG)
            assert(found == 0);
        }
#endif

        if(rp < end)
        {
                /* shuffle right */
                /* udebug("riul_add memmove: %ld\n", (char* )end - (char *)rp); */
                memmove(rp +1, rp, (char *)end - (char *)rp);
        }
        
        rp->offset = offset;
        rp->extent = extent;
        rp->vp = vp;
        rp->rflags = rflags;

        rl->nelems++;
        if(rl->nelems > rl->maxelems) {
            rl->maxelems = rl->nelems;
        }
        /* DEBUG */ /* assert(riul_r_find(rl, offset, &rp) != 0); */

        return ENOERR;
}

/*
 * Delete elem rp from the list rl
 */
static void
riul_delete(riul *rl, riu *const rp)
{
        riu *end = &rl->rp[rl->nelems];
        riu *rght = rp +1;

        assert(&rl->rp[0] <= rp && rp < end);

        if(rght < end)
        {
                /* shuffle left */
                /* udebug("riul_delete memmove: %ld\n", (char* )end - (char *)rght); */
                memmove(rp, rght, (char *)end - (char *)rght);
        }
        end--;

        end->offset = OFF_NONE;
        end->extent = 0;
        end->vp = NULL;
        end->rflags = 0;
        rl->nelems--;
}

/* End riul */
/* Begin pqctl */

/*
 * Shared, on disk, pq control structure.
 * Fixed size, never grows.
 * At beginning of file.
 */
struct pqctl {
#define PQ_MAGIC        0x50515545      /* PQUE */
        size_t magic;
#define PQ_VERSION      0x07
        size_t version;
        off_t datao;            /* beginning of data segment */
        off_t ixo;              /* beginning of index segment */
        size_t ixsz;            /* size of index segement */
        size_t nalloc;          /* slots allocated for products */
        size_t align;
        /* stats */
        off_t highwater;
        size_t maxproducts;
#define WRITE_COUNT_MAGIC       PQ_MAGIC
        unsigned write_count_magic;
#define MAX_WRITE_COUNT         ~0u
        unsigned write_count;
};
typedef struct pqctl pqctl;

/* End pqctl */
/* Begin pq */

/* function for putting memory to disk and releasing the memory; does a
 * possible unmap() or write() followed by an unlock */
typedef int mtofFunc(pqueue *const pq,
        off_t const offset,
        int const rflags);

/* function for getting stuff on disk into memory; does a lock possibly followed
 * by an mmap() or read() */
typedef int ftomFunc(pqueue *const pq,
        const off_t offset,
        const size_t extent,
        const int rflags,
        void **const ptrp);

/*
 * The process private pq info. (Internal structure)
 */
struct pqueue {
#define PQ_SIGSBLOCKED  0x1000  /* sav_set is valid */
        int pflags;
        size_t pagesz;
        ftomFunc *ftom;
        mtofFunc *mtof;
        size_t riusz;
        riul *riulp;

        int fd;

        pqctl *ctlp;

        off_t datao;
        void *base;             /* start of memory-mapped file */

        off_t ixo;              /* where are the indexes */
        size_t ixsz;
        void *ixp;
        size_t nalloc;          /* slots allocated for products */

        regionl *rlp;           /* region list index */
        tqueue *tqp;            /* timestamp index */
        fb *fbp;                /* skip list blocks, needed in both region list and
                                   timestamp layers */
        sx *sxp;                /* signature index */
        timestampt cursor;      /* private, current position in queue */
        off_t cursor_offset;    /* private, current offset in queue */
        sigset_t sav_set;
};

/* Begin OS */

/*
 * What is the system pagesize?
 */
static long
pagesize(void)
{
/* Hmm, aren't standards great? */
#ifndef CRAY
#if defined(_SC_PAGE_SIZE) && !defined(_SC_PAGESIZE)
#define _SC_PAGESIZE _SC_PAGE_SIZE
#endif
#ifdef _SC_PAGESIZE
        return sysconf(_SC_PAGESIZE);
#else
        return (long)getpagesize();
#endif
#else
        return 4096L;
#endif
}

/*
 * Sortof like ftruncate, except won't make the
 * file shorter.
 * May have side effect of leaving the
 * current position hosed.
 * If sparse == 0, fill in all the zero blocks (slow); 
 * else extend sparsely, without allocating zero blocks.
 *
 * Returns:
 *      EBADF   The "fd" argument is not a file descriptor open for writing.
 *      EIO     An I/O error occurred while reading from the file system.
 *      EOVERFLOW
 *              The file size in bytes or the number of blocks allocated to the
 *              file or the file serial number cannot be represented correctly.
 *      EINTR   A signal was caught during execution.
 *      EINVAL  The "len" argument was less than 0.
 *      EFBIG or EINVAL
 *              The "len" argument was greater than the maximum file size.
 *      EFBIG   The file is a regular file and length is greater than the offset
 *              maximum established in the open file description associated with
 *              "fd".
 *      EROFS   The named file resides on a read-only file system.
 */
static int
fgrow(const int fd, const off_t len, int sparse)
{
        struct stat sb;
        if (fstat(fd, &sb) < 0)
                return errno;
        if (len < sb.st_size)
                return ENOERR;
        if (sparse != 0) {
#ifdef HAVE_FTRUNCATE
            if (ftruncate(fd, len) < 0)
                return errno;
#else
            {
                int dumb = 0;
                /* beware position moved as side effect */
                if (lseek(fd, len-sizeof(dumb), SEEK_SET) < 0)
                        return errno;
                if(write(fd, &dumb, sizeof(dumb)) < 0)
                        return errno;
            }
#endif /* HAVE_FTRUNCATE */
        } else {                /* else, fill in all the zeros */
#define N_ZEROS_GROW 8192
            static int zeros[N_ZEROS_GROW];
            size_t zsize = N_ZEROS_GROW * sizeof(int);
            size_t clen = sb.st_size;
            off_t ii;
            /* beware position moved as side effect */
            if (lseek(fd, clen, SEEK_SET) < 0)
                return errno;
            if(write(fd, zeros, (len-clen) % zsize) < 0)
                return errno;
            for(ii = clen + (len-clen) % zsize; ii < len; ii += zsize) {
                if(write(fd, zeros, zsize) < 0)
                    return errno;
            }
        }
        return ENOERR;
}


/* 
 * Decode flock l_type member to string.
 * DEBUG
 */
static char *
s_ltype(const short l_type)
{
        switch (l_type) {
        case F_WRLCK: return "F_WRLCK";
        case F_RDLCK: return "F_RDLCK";
        case F_UNLCK: return "F_UNLCK";
        }
        return "Unknown type";
}

/* 
 * Decode flock whence member to string.
 * DEBUG
 */
static char *
s_whence(const short whence)
{
        switch (whence) {
        case SEEK_SET: return "SEEK_SET";
        case SEEK_CUR: return "SEEK_CUR";
        case SEEK_END: return "SEEK_END";
        }
        return "Unknown whence";
}


/*
 * If this process would be unable to obtain a lock,
 * return the pid of the process holding the conflicting lock.
 * Else return zero or -1 on error.
 */
static pid_t
fd_isLocked(const int fd, const short l_type,
        const off_t offset, const short l_whence, const size_t extent)
{
        int cmd = F_GETLK;
        struct flock lock;
        lock.l_type = l_type;
        lock.l_start = offset;
        lock.l_whence = l_whence;
        lock.l_len = (off_t)extent;
        if(fcntl(fd, cmd, &lock) < 0)
                return ((pid_t)-1);
        return (lock.l_type == F_UNLCK ? 0 : lock.l_pid);
}


/*
 * convenience wrapper around fcntl locking
 *
 * Returns:
 *      0       Success
 *      EACCESS or EAGAIN
 *              The "cmd" argument is F_SETLK: the type of lock (l_type) is a
 *              shared (F_RDLCK) or exclusive (F_WRLCK) lock and the segment
 *              of a file to be locked is already exclusive-locked by another
 *              process, or the type is an exclusive lock and some portion of
 *              the segment of a file to be locked is already shared-locked or
 *              exclusive-locked by another process.
 *      EBADF   The "fd" argument is not a valid open file descriptor, or the
 *              argument "cmd" is F_SETLK or F_SETLKW, the type of lock, l_type,
 *              is a shared lock (F_RDLCK), and "fd" is not a valid file
 *              descriptor open for reading, or the type of lock l_type, is an
 *              exclusive lock (F_WRLCK), and "fd" is not a valid file
 *              descriptor open for writing.
 *      EINVAL  The "cmd" argument is invalid, or the "cmd" argument is F_GETLK,
 *              F_SETLK or F_SETLKW and "l_type", "offset", "l_whence", or
 *              "extent" is not valid, or "fd" refers to a file that does not 
 *              support locking.
 *      ENOLCK  The argument "cmd" is F_SETLK or F_SETLKW and satisfying the
 *              lock or unlock request would result in the number of locked
 *              regions in the system exceeding a system-imposed limit.
 *      EOVERFLOW
 *              The "cmd" argument is F_GETLK, F_SETLK or F_SETLKW and the
 *              smallest or, if "extent" is non-zero, the largest offset of any
 *              byte in the requested segment cannot be represented correctly
 *              in an object of type off_t.
 *      EDEADLK The "cmd" argument is F_SETLKW, the lock is blocked by some lock
 *              from another process and putting the calling process to sleep,
 *              waiting for that lock to become free would cause a deadlock.
 */
static int
fd_lock(const int fd, const int cmd, const short l_type,
        const off_t offset, const short l_whence, const size_t extent)
{
        struct flock lock;

        lock.l_type = l_type;
        lock.l_start = offset;
        lock.l_whence = l_whence;
        lock.l_len = (off_t)extent;
        /* return (fcntl(fd, cmd, &lock) < 0 ? errno : 0); */
        if (fcntl(fd, cmd, &lock) < 0)
        {
                int errnum = errno;

                if(errnum == EDEADLK || errnum == EAGAIN || errnum == EACCES)
                {
                        if(errnum == EDEADLK || cmd != F_SETLK)
                        {
                        pid_t conflict = fd_isLocked(fd, l_type, offset,
                                        l_whence, extent);
                        uerror("fcntl %s failed for rgn (%ld %s, %lu): %s",
                                s_ltype(l_type), 
                                (long)offset, s_whence(l_whence), (long)extent, 
                                strerror(errnum));
                        uerror("conflicting pid %d", (int)conflict);
                        }
                }
                else
                {
                        serror("fcntl %s failed for rgn (%ld %s, %lu) %d",
                                s_ltype(l_type), 
                                (long)offset, s_whence(l_whence), (long)extent, 
                                errnum);
                }
                
                return errnum;
        }
        /* else */
        return 0;
}


#ifndef NO_MMAP
/*
 * Wrapper around mmap()
 *
 * Returns:
 *      EACCES  The "fd" argument is not open for read, regardless of the
 *              protection specified, or "fd" is not open for write and
 *              PROT_WRITE was specified for a MAP_SHARED type mapping.
 *      EAGAIN  The mapping could not be locked in memory, if required by
 *              mlockall(), due to a lack of resources.
 *      EBADF   The "fd" argument is not a valid open file descriptor.
 *      EINVAL  The "*ptrp" argument (if MAP_FIXED was specified) or "offset" is
 *              not a multiple of the page size as returned by sysconf(), or
 *              are considered invalid by the implementation.
 *      EINVAL  The value of "mflags" is invalid (neither MAP_PRIVATE nor 
 *              MAP_SHARED is set).
 *      EMFILE  The number of mapped regions would exceed an
 *              implementation-dependent limit (per process or per system).
 *      ENODEV  The "fd" argument refers to a file whose type is not supported
 *              by mmap().
 *      ENOMEM  MAP_FIXED was specified, and the range [*ptrp, *ptrp + extent)
 *              exceeds that allowed for the address space of a process; or if
 *              MAP_FIXED was not specified and there is insufficient room in
 *              the address space to effect the mapping.
 *      ENOMEM  The mapping could not be locked in memory, if required by
 *              mlockall(), because it would require more space than the system
 *              is able to supply.
 *      ENOTSUP The implementation does not support the combination of accesses
 *              requested in the "prot" argument.
 *      ENXIO   Addresses in the range [offset, offset + extent) are invalid for
 *              the object specified by "fd".
 *      ENXIO   MAP_FIXED was specified in "mflags" and the combination of
 *              "*ptrp", "extent" and "offset" is invalid for the object
 *              specified by "fd".
 *      EOVERFLOW
 *              The file is a regular file and the value of "offset" plus 
 *              "extent" exceeds the offset maximum established in the open file
 *              description associated with "fd". 
 */
static int
mapwrap(const int fd,
        const off_t offset,
        const size_t extent,
        const int prot,
        int mflags,
        void **ptrp)
{
        int status = ENOERR;
        void *mm;

#ifdef MAP_FILE /* HP-UX */
        fSet(mflags, MAP_FILE);
#endif
#ifdef MAP_VARIABLE /* HP-UX */
        if(!fIsSet(mflags, MAP_FIXED))
                fSet(mflags, MAP_VARIABLE);
#endif
        
        mm = (void *) mmap(*ptrp, extent, prot, mflags, fd, offset);
        if(mm == (void *)((ptrdiff_t)-1))
        {
                status = errno;
                serror("mmap: %p %ld %lu", *ptrp,
                        (long)offset, (unsigned long)extent);
                return status;
        }
#if TRACE_MMAP
        udebug("%p = mmap: %p %ld %lu", mm, *ptrp,
                (long)offset, (unsigned long)extent );
#endif

        *ptrp = mm;
        return status;
}


/*
 * Wrapper around munmap
 */
/*ARGSUSED*/
static int
unmapwrap(void *const ptr,
        const off_t offset,
        const size_t extent,
        const int mflags)
{
        int status = ENOERR;

#ifdef USE_MSYNC
        /* if(!fIsSet(mflags, M_DISCARD)) */
        if(1)
        {
#ifdef MS_ASYNC
                if(msync(ptr, extent, MS_ASYNC) == -1)
                        serror("msync: %ld %lu MS_ASYNC",
                                (long)offset, (unsigned long)extent);
#else
                if(msync(ptr, extent) == -1)
                        serror("msync: %ld %lu",
                                (long)offset, (unsigned long)extent);
#endif
        }
#endif /* USE_MSYNC */

#if TRACE_MMAP
        udebug("unmap: %p %ld %lu", ptr, (long)offset, (unsigned long)extent);
#endif
        if(munmap(ptr, extent) == -1)
        {
                status = errno;
                serror("munmap: %ld %lu", (long)offset, (unsigned long)extent);
                return status;
        }

        return status;
}
#endif /*!NO_MMAP*/

/*
 * Get a lock on (offset, extent) according to the
 * RGN_* flags rflags.
 *
 * Returns:
 *      0       Success
 *      EACCESS or EAGAIN
 *              The "cmd" argument is F_SETLK: the type of lock (l_type) is a
 *              shared (F_RDLCK) or exclusive (F_WRLCK) lock and the segment
 *              of a file to be locked is already exclusive-locked by another
 *              process, or the type is an exclusive lock and some portion of
 *              the segment of a file to be locked is already shared-locked or
 *              exclusive-locked by another process.
 *      EBADF   The "fd" argument is not a valid open file descriptor, or the
 *              argument "cmd" is F_SETLK or F_SETLKW, the type of lock, l_type,
 *              is a shared lock (F_RDLCK), and "fd" is not a valid file
 *              descriptor open for reading, or the type of lock l_type, is an
 *              exclusive lock (F_WRLCK), and "fd" is not a valid file
 *              descriptor open for writing.
 *      EINVAL  The "cmd" argument is invalid, or the "cmd" argument is F_GETLK,
 *              F_SETLK or F_SETLKW and "l_type", "offset", "l_whence", or
 *              "extent" is not valid, or "fd" refers to a file that does not 
 *              support locking.
 *      ENOLCK  The argument "cmd" is F_SETLK or F_SETLKW and satisfying the
 *              lock or unlock request would result in the number of locked
 *              regions in the system exceeding a system-imposed limit.
 *      EOVERFLOW
 *              The "cmd" argument is F_GETLK, F_SETLK or F_SETLKW and the
 *              smallest or, if "extent" is non-zero, the largest offset of any
 *              byte in the requested segment cannot be represented correctly
 *              in an object of type off_t.
 *      EDEADLK The "cmd" argument is F_SETLKW, the lock is blocked by some lock
 *              from another process and putting the calling process to sleep,
 *              waiting for that lock to become free would cause a deadlock.
 */
static int
rgn_lock(pqueue *const pq,
        const off_t offset,
        const size_t extent,
        const int rflags)
{
#ifndef NDEBUG
        if(offset == pq->ixo && extent == pq->ixsz)
                assert(fIsSet(rflags, RGN_NOLOCK));
        else
                assert(!fIsSet(rflags, RGN_NOLOCK));
#endif

        if(fIsSet(rflags, RGN_NOLOCK) || fIsSet(pq->pflags, PQ_NOLOCK))
                return ENOERR;
        
        /* else */
        {
                int cmd = fIsSet(rflags, RGN_NOWAIT) ?  F_SETLK : F_SETLKW;
                short l_type = fIsSet(rflags, RGN_WRITE) ? F_WRLCK : F_RDLCK;

                int status =  fd_lock(pq->fd, cmd, l_type,
                                offset, SEEK_SET, extent);
#if TRACE_LOCK
        udebug("%s (%ld, %lu)",
                s_ltype(l_type),
                (long)offset, (unsigned long)extent);
#endif
                return status;
        }
}


/*
 * Release lock on (offset, extent) according to the
 * RGN_* flags rflags.
 */
static int
rgn_unlock(pqueue *const pq,
        const off_t offset,
        const size_t extent,
        const int rflags)
{
#ifndef NDEBUG
        if(offset == pq->ixo && extent == pq->ixsz)
                assert(fIsSet(rflags, RGN_NOLOCK));
#endif

        if(fIsSet(rflags, RGN_NOLOCK) || fIsSet(pq->pflags, PQ_NOLOCK))
                return ENOERR;
        /* else */
#if TRACE_LOCK
        udebug("F_UNLCK (%ld, %lu)",
                (long)offset, (unsigned long)extent);
#endif
        return fd_lock(pq->fd, F_SETLK, F_UNLCK,
                        offset, SEEK_SET, extent);
}


/*
 * file to memory lseek/malloc/read
 */
/*ARGSUSED*/
static int
f_ftom(pqueue *const pq,
        const off_t offset,
        const size_t extent,
        const int rflags,
        void **const ptrp)
{
        int status = ENOERR;
        void *vp = NULL;
        ssize_t nread = 0;

        assert(pq != NULL);
        assert(pq->datao > 0);
        assert(pq->datao % pq->pagesz == 0);
        assert(pq->ixo >= pq->datao);
        assert(pq->ixo % pq->pagesz == 0);
        assert(pq->ixsz >= pq->pagesz);
        assert(pq->ixsz % pq->pagesz == 0);

        assert(0 <= offset && offset <= pq->ixo);
        assert(0 != extent && extent < pq->ixo + pq->ixsz);

        assert(pIf(fIsSet(rflags, RGN_WRITE),
                        !fIsSet(pq->pflags, PQ_READONLY)));
        
        vp = malloc(extent);
        if(vp == NULL)
        {
                status = errno;
                serror("f_ftom malloc %lu", (unsigned long)extent);
                return status;
        }
        /* DEBUG */
        memset(vp, 0, extent);

        status = rgn_lock(pq, offset, extent, rflags);
        if(status != ENOERR)
                goto unwind_alloc;

        if(lseek(pq->fd, offset, SEEK_SET) != offset)
        {
                status = errno;
                serror("f_ftom lseek %ld", (long)offset);
                goto unwind_lock;
        }

        errno = 0;
        nread = read(pq->fd, vp, extent);
        if(nread != extent)
        {
                /* N.B. No EINTR retry */
                status = errno;
                if(nread == -1)
                {
                        serror("f_ftom read %lu", (unsigned long)extent);
                        goto unwind_lock;
                }
                /* else */
                if(status != ENOERR)
                {
                        serror("f_ftom at %ld incomplete read %d != %lu",
                                (long)offset, nread, (unsigned long)extent);
                        goto unwind_lock;
                }
                /* else, its okay we read zero. pq_create */
        }
        
        status = riul_add(&pq->riulp, pq->pagesz,
                        offset, extent, vp, rflags);
        if(status != ENOERR)
                goto unwind_lock;

        *ptrp = vp;
        return ENOERR;
        
unwind_lock:
        (void) rgn_unlock(pq, offset, extent,
                        fMask(rflags, RGN_MODIFIED|RGN_NOWAIT));
        /*FALLTHROUGH*/
unwind_alloc:
        free(vp);
        return status;
}

/*
 * memory release, write()/free() version;
 */
static int
f_mtof(pqueue *const pq,
        off_t const offset,
        int const rflags)
{
        int status = ENOERR;
        riu *rp = NULL;
        size_t extent;
        void *vp;
        ssize_t nwrote;

        assert(pq != NULL); /* would have core dumped already initializing */
        assert(pq->datao > 0);
        assert(pq->datao % pq->pagesz == 0);
        assert(pq->ixo >= pq->datao);
        assert(pq->ixo % pq->pagesz == 0);
        assert(pq->ixsz >= pq->pagesz);
        assert(pq->ixsz % pq->pagesz == 0);

        assert(pIf(fIsSet(rflags, RGN_MODIFIED),
                        !fIsSet(pq->pflags, PQ_READONLY)));

        if(riul_r_find(pq->riulp, offset, &rp) == 0)
        {
                uerror("f_mtof: Couldn't riul_r_find %ld", (long)offset);
                return EINVAL;
        }

        assert(rp->offset == offset);
        assert(0 < rp->extent && rp->extent < pq->ixo + pq->ixsz);
        extent = rp->extent;
        assert(rp->vp != NULL);
        vp = rp->vp;
        assert(pIf(fIsSet(rflags, RGN_MODIFIED),
                        fIsSet(rp->rflags, RGN_WRITE)));
        assert(fIsSet(rflags, RGN_NOLOCK) ==
                        fIsSet(rp->rflags, RGN_NOLOCK));

        riul_delete(pq->riulp, rp);
        
        if(fIsSet(rflags, RGN_MODIFIED))
        {
                assert(!fIsSet(pq->pflags, PQ_READONLY));
                if(lseek(pq->fd, offset, SEEK_SET) != offset)
                {
                        status = errno;
                        serror("f_mtof lseek %ld", (long)offset);
                        goto unwind_vp;
                }
                nwrote = write(pq->fd, vp, extent);
                if(nwrote != extent)
                {
                        /* N.B. No EINTR retry */
                        status = errno;
                        if(nwrote == -1)
                        {
                                serror("f_mtof write %lu",
                                        (unsigned long)extent);
                        }
                        else
                        {
                        serror("f_mtof at %ld incomplete write %d != %lu",
                                        (long)offset, nwrote,
                                        (unsigned long)extent);
                        }
                }
        }
        (void) rgn_unlock(pq, offset, extent, rflags);
unwind_vp:
        (void)free(vp);
        return status;
}


#ifndef NO_MMAP
/*
 * file to memory using mmap
 *
 * Returns:
 *      0       Success
 *      EACCESS or EAGAIN
 *              "rflags" contains RGN_NOWAIT and the segment of a file to
 *              be locked is already exclusive-locked by another process,
 *              or "rflags" contains RGN_WRITE and some portion of the
 *              segment of the file to be locked is already shared-locked or
 *              exclusive-locked by another process.
 *      EACCES  "pq->fd" is not open for read, regardless of the protection
 *              specified, or "fd" is not open for write and PROT_WRITE was
 *              specified for a MAP_SHARED type mapping.
 *      EAGAIN  The mapping could not be locked in memory, if required by
 *              mlockall(), due to a lack of resources.
 *      EBADF   "pq->fd" is not a valid open file descriptor, or "rflags" 
 *              doesn't contain RGN_WRITE and "pq->fd" is not a valid file
 *              descriptor open for reading, or "rflags" contains RGN_WRITE
 *              and "pq->fd" is not a valid file descriptor open for writing.
 *      EDEADLK "rflags" doesn't contain RGN_NOWAIT, the lock is blocked by some
 *              lock from another process and putting the calling process to
 *              sleep, waiting for that lock to become free would cause a
 *              deadlock.
 *      EFBIG or EINVAL
 *              The "extent" argument was greater than the maximum file size.
 *      EFBIG   The file is a regular file and length is greater than the offset
 *              maximum established in the open file description associated with
 *              "pq->fd".
 *      EINTR   A signal was caught during execution.
 *      EINVAL  "offset", or "extent" is not valid, or "pq->fd" refers to a file
 *              that does not support locking.
 *      EINVAL  "offset" is not a multiple of the page size as returned by 
 *              sysconf(), or is considered invalid by the implementation.
 *      EIO     An I/O error occurred while reading from the file system.
 *      EMFILE  The number of mapped regions would exceed an
 *              implementation-dependent limit (per process or per system).
 *      ENODEV  "pq->fd" refers to a file whose type is not supported by mmap().
 *      ENOLCK  Satisfying the request would result in the number of locked
 *              regions in the system exceeding a system-imposed limit.
 *      ENOMEM  There is insufficient room in the address space to effect the
 *              necessary mapping.
 *      ENOMEM  The mapping could not be locked in memory, if required by
 *              mlockall(), because it would require more space than the system
 *              is able to supply.
 *      ENOMEM  Insufficient memory is available.
 *      ENOTSUP The implementation does not support the combination of accesses
 *              requested in "pq->pflags" and "rflags".
 *      ENXIO   Addresses in the range [offset, offset + extent) are invalid for
 *              the object specified by "pq->fd".
 *      EOVERFLOW
 *              The smallest or, if "extent" is non-zero, the largest offset of
 *              any byte in the requested segment cannot be represented
 *              correctly in an object of type off_t.
 *      EOVERFLOW
 *              The file size in bytes or the number of blocks allocated to the
 *              file or the file serial number cannot be represented correctly.
 *      EOVERFLOW
 *              The file is a regular file and the value of "offset" plus 
 *              "extent" exceeds the offset maximum established in the open file
 *              description associated with "fd". 
 *      EROFS   The file resides on a read-only file system.
 */
/*ARGSUSED*/
static int
mm_ftom(pqueue *const pq,
        const off_t offset,
        const size_t extent,
        const int rflags,
        void **const ptrp)
{
        int status = ENOERR;
        int mflags = fIsSet(pq->pflags, PQ_PRIVATE) ?
                        MAP_PRIVATE : MAP_SHARED;
        int prot = fIsSet(pq->pflags, PQ_READONLY)
                         && !fIsSet(rflags, RGN_WRITE) ?
                        PROT_READ : (PROT_READ|PROT_WRITE);
        size_t rem = offset % pq->pagesz;
        off_t pageo = offset;
        size_t pagext = _RNDUP(rem + extent, pq->pagesz);
        void *vp = NULL;

        assert(pq != NULL); /* would have core dumped already initializing */
        assert(pq->datao > 0);
        assert(pq->datao % pq->pagesz == 0);
        assert(pq->ixo >= pq->datao);
        assert(pq->ixo % pq->pagesz == 0);
        assert(pq->ixsz >= pq->pagesz);
        assert(pq->ixsz % pq->pagesz == 0);

        assert(0 <= offset && offset <= pq->ixo);
        assert(0 != extent && extent < pq->ixo + pq->ixsz);

        assert(pIf(fIsSet(rflags, RGN_WRITE),
                        !fIsSet(pq->pflags, PQ_READONLY)));

        if(rem != 0)
                pageo -= rem;

        status = rgn_lock(pq, offset, extent, rflags);
        if(status != ENOERR)
                return status;

        if(fIsSet(prot, PROT_WRITE))
        {
                status = fgrow(pq->fd, offset + extent, 
                               fIsSet(pq->pflags, PQ_SPARSE));
                if(status != ENOERR)
                        goto unwind_lock;
        }

        assert(pageo % pq->pagesz == 0);
        assert(pagext % pq->pagesz == 0);
        status = mapwrap(pq->fd, pageo, pagext, prot, mflags, &vp);
        if(status != ENOERR)
                goto unwind_lock;
        /* else */

        if(rem != 0)
                vp = (char *)vp + rem;

        status = riul_add(&pq->riulp, pq->pagesz,
                        offset, extent, vp, rflags);
        if(status != ENOERR)
                goto unwind_lock;

        *ptrp = vp;
        return status;
        
unwind_lock:
        (void) rgn_unlock(pq, offset, extent,
                        fMask(rflags, RGN_MODIFIED|RGN_NOWAIT));
        return status;
}

/*
 * memory release, mmap version
 */
static int
mm_mtof(pqueue *const pq,
        off_t const offset,
        int const rflags)
{
        int status = ENOERR;
        int mflags = 0; /* TODO: translate rflags to mflags */
        off_t rem = offset % (off_t)pq->pagesz;
        riu *rp = NULL;
        size_t extent;
        void *vp;

        assert(pq != NULL); /* would have core dumped already initializing */
        assert(pq->datao > 0);
        assert(pq->datao % pq->pagesz == 0);
        assert(pq->ixo >= pq->datao);
        assert(pq->ixo % pq->pagesz == 0);
        assert(pq->ixsz >= pq->pagesz);
        assert(pq->ixsz % pq->pagesz == 0);

        assert(pIf(fIsSet(rflags, RGN_MODIFIED),
                        !fIsSet(pq->pflags, PQ_READONLY)));

        if(riul_r_find(pq->riulp, offset, &rp) == 0)
        {
                uerror("mm_mtof: Couldn't riul_r_find %ld", (long)offset);
                return EINVAL;
        }

        assert(rp->offset == offset);
        assert(0 < rp->extent && rp->extent < pq->ixo + pq->ixsz);
        extent = rp->extent;
        assert(rp->vp != NULL);
        vp = rp->vp;
        assert(pIf(fIsSet(rflags, RGN_MODIFIED),
                        fIsSet(rp->rflags, RGN_WRITE)));
        assert(fIsSet(rflags, RGN_NOLOCK) ==
                        fIsSet(rp->rflags, RGN_NOLOCK));

        riul_delete(pq->riulp, rp);
        
        if(rem == 0)
        {
                status =  unmapwrap(vp, offset, extent, mflags);
        }
        else
        {
                off_t pageno = offset / (off_t)pq->pagesz;
                size_t pagext = _RNDUP(rem + extent, (off_t)pq->pagesz);
                vp = (char *)vp - rem;
                status = unmapwrap(vp, pageno * (off_t)pq->pagesz,
                        pagext, mflags);
        }
        (void) rgn_unlock(pq, offset, extent, rflags);
        return status;
}

static int
mm0_map(pqueue *const pq)
{
        int status = ENOERR;
        void *vp = pq->base;
        struct stat sb;
        off_t st_size = pq->ixo + (off_t)pq->ixsz;
        int mflags = fIsSet(pq->pflags, PQ_PRIVATE) ?
                        MAP_PRIVATE : MAP_SHARED;
        int prot = fIsSet(pq->pflags, PQ_READONLY) ?
                        PROT_READ : (PROT_READ|PROT_WRITE);

        if(fstat(pq->fd, &sb) < 0)
                return errno;
        if(st_size < sb.st_size)
        {
                st_size = sb.st_size;
        }
        else if(!fIsSet(pq->pflags, PQ_READONLY))
        {
                status = fgrow(pq->fd, st_size,
                               fIsSet(pq->pflags, PQ_SPARSE));
                if(status != ENOERR)
                        return status;
        }
        if(vp != NULL)
                fSet(mflags, MAP_FIXED);
        udebug("Mapping %ld", (long)st_size);
        if (~(size_t)0 < st_size) {
            uerror("mm0_map(): File is too big to memory-map");
            pq->base = NULL;
            status = EFBIG;
            return status;
        }
        status = mapwrap(pq->fd, 0, st_size, prot, mflags, &vp);
        if(status != ENOERR)
        {
                pq->base = NULL;
                return status;
        }
        assert(vp != NULL);
        assert(pIf(pq->base != NULL, pq->base == vp));
        pq->base = vp;
        return status;
}

/*
 * file to memory using mmap, map whole file 
 */
/*ARGSUSED*/
static int
mm0_ftom(pqueue *const pq,
        const off_t offset,
        const size_t extent,
        const int rflags,
        void **const ptrp)
{
        int status = ENOERR;
        void *vp = NULL;

        assert(pq != NULL);
        assert(pq->datao > 0);
        assert(pq->datao % pq->pagesz == 0);
        assert(pq->ixo >= pq->datao);
        assert(pq->ixo % pq->pagesz == 0);
        assert(pq->ixsz >= pq->pagesz);
        assert(pq->ixsz % pq->pagesz == 0);

        assert(0 <= offset && offset <= pq->ixo);
        assert(0 != extent && extent < pq->ixo + pq->ixsz);

        assert(pIf(fIsSet(rflags, RGN_WRITE),
                        !fIsSet(pq->pflags, PQ_READONLY)));

        status = rgn_lock(pq, offset, extent, rflags);
        if(status != ENOERR)
                return status;

        if(pq->base == NULL)
        {
                /* first time */
                status = mm0_map(pq);
                if(status != ENOERR)
                        goto unwind_lock;
        }

        vp = (char *)pq->base + offset;

        status = riul_add(&pq->riulp, pq->pagesz,
                        offset, extent, vp, rflags);
        if(status != ENOERR)
                goto unwind_lock;

        *ptrp = vp;

        return status;

unwind_lock:
        (void) rgn_unlock(pq, offset, extent,
                        fMask(rflags, RGN_MODIFIED|RGN_NOWAIT));
        return status;
}

/*
 * memory release, mmap whole file version
 */
/*ARGSUSED*/
static int
mm0_mtof(pqueue *const pq,
        off_t const offset,
        int const rflags)
{
        size_t extent;
        riu *rp = NULL;
        
        assert(pq != NULL); /* would have core dumped already initializing */
        assert(pq->datao > 0);
        assert(pq->datao % pq->pagesz == 0);
        assert(pq->ixo >= pq->datao);
        assert(pq->ixo % pq->pagesz == 0);
        assert(pq->ixsz >= pq->pagesz);
        assert(pq->ixsz % pq->pagesz == 0);

        assert(pIf(fIsSet(rflags, RGN_MODIFIED),
                        !fIsSet(pq->pflags, PQ_READONLY)));

        if(riul_r_find(pq->riulp, offset, &rp) == 0)
        {
                uerror("mm0_mtof: Couldn't riul_r_find %ld", (long)offset);
                return EINVAL;
        }

        assert(rp->offset == offset);
        assert(0 < rp->extent && rp->extent < pq->ixo + pq->ixsz);
        extent = rp->extent;
        assert(pq->base == NULL || (rp->vp != NULL
                 && pq->base <= rp->vp
                && (char *)rp->vp <= (char *)pq->base + pq->ixo));
        assert(pIf(fIsSet(rflags, RGN_MODIFIED),
                        fIsSet(rp->rflags, RGN_WRITE)));
        assert(fIsSet(rflags, RGN_NOLOCK) ==
                        fIsSet(rp->rflags, RGN_NOLOCK));

        riul_delete(pq->riulp, rp);

        return rgn_unlock(pq, offset, extent, rflags);
}
/* End OS */
#endif /*!NO_MMAP*/


/*
 * Free a pqueue
 */
static void
pq_delete(pqueue *const pq)
{
        if(pq == NULL)
                return;
        if(pq->riulp != NULL)
        {
                free(pq->riulp);
                pq->riulp = NULL;
        }
        free(pq);
}


/*
 * Sets the functions to be used to access the product-queue.
 *
 * Arguments:
 *      pq      Pointer to product-queue structure.  The following members
 *              must be set to their final values: pflags, ixo, ixsz.
 *      pflags  The flags governing the product-queue.
 *      pqSize  The maximum possible size of the product-queue in bytes.
 */
static void
pq_setAccessFunctions(
    pqueue* const       pq,
    const int           pflags,
    const off_t         pqSize)
{
    assert(NULL != pq);

    if (fIsSet(pflags, PQ_NOMAP))
    {
        /*
         * The product-queue will be accessed via read() and write().
         */
        pq->ftom = f_ftom;
        pq->mtof = f_mtof;
    }
    else {
        static size_t   maxSizeT = ~(size_t)0;

        if (fIsSet(pflags, PQ_MAPRGNS) || pqSize <= 0 || pqSize > maxSizeT
#if __FreeBSD__ == 4
            || (sizeof(size_t) == 4 && pqSize > 2000000000)
            /*
             * The operating-system is 32-bit Free BSD 4, which has a limit of
             * 2 gigabytes on the extent of an mmap() call.
             */
#endif
            )
        {
            /*
             * The product-queue will be accessed by being memory-mapped
             * on a region-by-region basis.
             */
            pq->ftom = mm_ftom;
            pq->mtof = mm_mtof;
        }
        else
        {
            /*
             * The product-queue will be accessed by being memory-mapped
             * once, in its entirety.
             */
            pq->ftom = mm0_ftom;
            pq->mtof = mm0_mtof;
        }
    }
}


/*
 * Allocate and initialize a pqueue.  The product-queue access-functions
 * (pq->ftom & pq->mtof) are set assuming a product-queue of maximum-size.
 */
static pqueue *
pq_new( int pflags,
        size_t align,
        off_t initialsz, /* initial allocation available */
        size_t nregions) /* initial rl->nalloc, ... */
{
        pqueue *const pq = (pqueue *)malloc(sizeof(pqueue));

        if(pq == NULL)
                return NULL; 

        (void)memset(pq, 0, sizeof(pqueue));
        (void)sigemptyset(&pq->sav_set);

/*
 * This is a convenient place to overide things at compile time.
 */

        fSet(pflags, PQ_NOGROW); /* always set for this version of pq! */
#if _NOMAP || NO_MMAP
        fSet(pflags, PQ_NOMAP);
#endif
#if _MAPRGNS
        fSet(pflags, PQ_MAPRGNS);
#endif
        pq->pflags = pflags;

        pq->pagesz = (size_t)pagesize();

#if __hpux
        /* doesn't allow overlapping maps */
        if(fIsSet(pflags, PQ_MAPRGNS) && align < pq->pagesz)
        {
                /* unotice("forcing alignment %u", pq->pagesz); */
                align =  pq->pagesz;
        }
#endif

        pq->riulp = (riul *)malloc(pq->pagesz);
        if(pq->riulp == NULL)
        {
                free(pq);
                return NULL;    
        }
        riul_init(pq->riulp, 0, pq->pagesz);

        pq->fd = -1;

        pq->datao = (off_t)lcm(pq->pagesz, align);
        assert(pq->datao >= sizeof(pqctl));

        pq->ixo = pq->datao + (off_t)_RNDUP(initialsz, pq->pagesz);
        if(nregions != 0)
        {
                pq->ixsz = ix_sz(nregions, align);
                pq->ixsz =  _RNDUP(pq->ixsz, pq->pagesz);
                pq->nalloc = nregions;
        }
        else
        {
                pq->ixsz = pq->pagesz;
        }

        /*
         * Se the product-queue access-functions.  As a failsafe, assume a
         * maximum size for the product-queue.
         */
        pq_setAccessFunctions(pq, pq->pflags, 
            ((off_t)1 << ((sizeof(off_t)*CHAR_BIT - 2))) + 
            (((off_t)1 << (sizeof(off_t)*CHAR_BIT - 2)) - 1));

        pq->cursor = TS_NONE;
        pq->cursor_offset = OFF_NONE;

        return pq;
}



/* End pq */
/* Begin ctl */

/*
 * The 'ctl_xxx' functions deal with pqctl pq->ctlp AND
 * the indexes pq->rlp, pq->tqp, pq->sxp, pq->fbp via ixXXX.
 * Accesses to these areas operate on a single lock, the lock
 * on (0, pgsz), ctlp.
 */

/*
 * Release the ctl lock and write back any changes.
 */
static int
ctl_rel(pqueue *const pq, const int rflags)
{
        int status = ENOERR;

        assert(pq->ctlp != NULL);
        assert(pq->ixp != NULL);

        if(pq->ixp != NULL)
        {
                status = (pq->mtof)(pq, pq->ixo, rflags|RGN_NOLOCK);
                if(status != ENOERR)
                        uerror("mtof ix: %s", strerror(status));
                pq->ixp = NULL;
                pq->rlp = NULL;
                pq->tqp = NULL;
                pq->sxp = NULL;
                pq->fbp = NULL;
        }
        
        if(pq->ctlp != NULL)
        {
                status = (pq->mtof)(pq, 0, rflags);
                if(status != ENOERR)
                        uerror("mtof ctl: %s", strerror(status));
                pq->ctlp = NULL;
        }

        if(fIsSet(pq->pflags, PQ_SIGSBLOCKED))
        {
                /* something was set, end critical section */
                sigset_t sav_set = pq->sav_set;

                (void)sigemptyset(&pq->sav_set);
                fClr(pq->pflags, PQ_SIGSBLOCKED);

                (void) sigprocmask(SIG_SETMASK, &sav_set, NULL);
        }

        return status;
}


/*
 * Initialize the on disk state (ctl and indexes) of a
 * new queue file. Called by pq_create().
 */
static int
ctl_init(pqueue *const pq, size_t const align)
{
        int status = ENOERR;
        void *vp = NULL;
        size_t nalloc;

        assert(pq != NULL);
        assert(pq->pagesz != 0);
        assert(pq->datao > 0);
        assert(pq->datao % pq->pagesz == 0);
        assert(pq->ixo > pq->datao);
        assert(pq->ixo % pq->pagesz == 0);
        assert(pq->ixsz >= pq->pagesz);
        assert(pq->ixsz % pq->pagesz == 0);
        assert(align != 0);

        /*
         * You might well ask why this is not a
         * sigprocmask(SIG_BLOCK,...) critical section.
         * The answer is that that EINTR (and any other
         * error) unlinks the file we create.
         */

        /*
         * bring in the pqctl
         * N.B. No wait. Another lock implies create collision, error
         */
        status = (pq->ftom)(pq,
                         0, (size_t)pq->datao, RGN_WRITE|RGN_NOWAIT, &vp);
#ifndef NO_MMAP
        if(status == EIO && (pq->ftom == mm_ftom || pq->ftom == mm0_ftom))
        {
                unotice("EIO => remote file system\n");
                /* try again */
                pq->ftom = f_ftom;
                pq->mtof = f_mtof;
                status = (pq->ftom)(pq,
                        0, (size_t)pq->datao, RGN_WRITE|RGN_NOWAIT, &vp);
        }
#endif /*!NO_MMAP*/
        if(status != ENOERR)
                return status;

        pq->ctlp = (pqctl *)vp;
        pq->ctlp->magic = PQ_MAGIC;
        pq->ctlp->version = PQ_VERSION;
        pq->ctlp->write_count_magic = WRITE_COUNT_MAGIC;
        pq->ctlp->write_count = 1;              /* this process is writer */
        pq->ctlp->datao = pq->datao;
        pq->ctlp->ixo = pq->ixo;
        pq->ctlp->ixsz = pq->ixsz;
        pq->ctlp->nalloc = pq->nalloc;
        pq->ctlp->highwater = 0;
        pq->ctlp->maxproducts = 0;
        pq->ctlp->align = align;

        /* bring in the indexes */
        status = (pq->ftom)(pq,
                         pq->ixo, pq->ixsz, RGN_WRITE|RGN_NOLOCK, &pq->ixp);
        if(status != ENOERR)
        {
                (void)(pq->mtof)(pq, 0, 0);
                return status;
        }

        (void)ix_ptrs(pq->ixp, pq->ixsz, pq->nalloc, align, &pq->rlp, &pq->tqp, &pq->fbp, &pq->sxp);
        nalloc = pq->nalloc;    /* ix_ptrs computed this in version 3 */

        /* initialize fb for skip list blocks */
        fb_init(pq->fbp, nalloc);

        /* initialize tqueue */
        tq_init(pq->tqp, nalloc, pq->fbp);

        /* initialize regionl, adding one huge region for data */
        rl_init(pq->rlp, nalloc, pq->fbp);
        {
                off_t  datasz = pq->ixo - pq->datao;

                if (~(size_t)0 < datasz) {
                    uerror("ctl_init(): Data portion of file is too big for "
                        "one region");
                    return EFBIG;
                }
                {
                    size_t extent0 = (size_t)datasz;
                    region *rp = rl_add(pq->rlp, pq->datao, extent0);

                    pq->rlp->maxfextent = extent0;
                    assert(rp != NULL
                            && rp->offset == pq->datao
                            && rp->extent == extent0);
                }
        }

        sx_init(pq->sxp, nalloc);
        
        return status;
}


/*
 * Initialize the in memory state of pq from
 * an existing file. Called by pq_open().
 * On return, the control region (pq->ctlp) will be mapped and/or locked.
 *
 * Arguments:
 *      pq      Pointer to product-queue structure to be set.
 *      path    Pathname of product-queue.
 *
 * Returns:
 *      0           Success
 *      PQ_CORRUPT  The  product-queue is internally inconsistent.
 *      else        <errno.h> error-code.
 */
static int
ctl_gopen(pqueue *const pq, const char *path)
{
        int status = ENOERR;
        pqctl *ctlp;
        size_t ctlsz; 
        void *vp = NULL;

        assert(pq != NULL);
        assert(pq->pagesz > 0);
        assert(pq->ixp == NULL && pq->rlp == NULL && pq->tqp == NULL);

        ctlsz = pq->pagesz;

        /*FALLTHROUGH*/
remap:
        status = (pq->ftom)(pq, 0, ctlsz, 0, &vp);
#ifndef NO_MMAP
        if(status == EIO && (pq->ftom == mm_ftom || pq->ftom == mm0_ftom))
        {
                uwarn("Product-queue can't be memory-mapped!  "
                    "Continuing with slower read/write I/O.");
                /* try again */
                pq->ftom = f_ftom;
                pq->mtof = f_mtof;
                status = (pq->ftom)(pq, 0, ctlsz, 0, &vp);
        }
#endif /*!NO_MMAP*/
        if(status != ENOERR)
                return status;

        ctlp = (pqctl *)vp;
        if(ctlp->magic != PQ_MAGIC)
        {
                /* Not a product queue */
                uerror("%s: Not a product queue\n", path);
                status = EINVAL;
                goto unwind_map;
        }
        if(ctlp->version != PQ_VERSION)
        {
                uerror("%s: Product queue is version %d instead of expected version %d\n",
                       path, ctlp->version, PQ_VERSION);
                status = EINVAL;
                goto unwind_map;
        }
        if(ctlp->datao % pq->pagesz != 0)
        {
                /* Can't align */
                /* TODO: If we use read()/write() not fatal ??? */
                uerror("%s: Can't align\n", path);
                status = EINVAL;
                goto unwind_map;
        }
        if((size_t)ctlp->datao != ctlsz)
        {
                /* we guessed wrong, try again */
                if(ctlsz != pq->pagesz)
                {
                        /* don't try more than once */
                        status = EINVAL;
                        goto unwind_map;
                }
                ctlsz = (size_t)ctlp->datao;
                (void)(pq->mtof)(pq, 0, 0);
                goto remap;
        }

        pq->datao = ctlp->datao;
        pq->ixo = ctlp->ixo;
        pq->ixsz = ctlp->ixsz;
        pq->nalloc = ctlp->nalloc;
        pq->ctlp = ctlp;

        if (!(pq->datao > 0) ||
            !(pq->datao % pq->pagesz == 0) ||
            !(pq->ixo > pq->datao) ||
            !(pq->ixo % pq->pagesz == 0) ||
            !(pq->ixsz >= pq->pagesz) ||
            !(pq->ixsz % pq->pagesz == 0)) {

            uerror("ctl_gopen: pq->datao=%lu, pq->pagesz=%lu, pq->ixo=%lu, "
                "pq->ixsz=%lu",
                (unsigned long)pq->datao, (unsigned long)pq->pagesz, 
                (unsigned long)pq->ixo, (unsigned long)pq->ixsz);
            status = PQ_CORRUPT;
            goto unwind_map;
        }

        /*
         * Reset the product-queue access-functions based on the product-queue's
         * actual size.
         */
        pq_setAccessFunctions(pq, pq->pflags, (off_t)(pq->ixo + pq->ixsz));

        /* bring in the indexes */
        status = (pq->ftom)(pq, pq->ixo, pq->ixsz, RGN_NOLOCK, &pq->ixp);
        if(status != ENOERR)
                goto unwind_map;

        if (!ix_ptrs(pq->ixp, pq->ixsz, pq->nalloc, pq->ctlp->align,
                &pq->rlp, &pq->tqp, &pq->fbp, &pq->sxp)) {
            status = PQ_CORRUPT;
            goto unwind_map;
        }

        if (!(pq->rlp->nalloc == pq->nalloc && pq->tqp->nalloc == pq->nalloc
                        && pq->sxp->nalloc == pq->nalloc)) { 
                uerror("ctl_gopen: pq->rlp->nalloc=%lu, pq->nalloc=%lu, "
                    "pq->tqp->nalloc=%lu, pq->sxp->nalloc=%lu",
                    (unsigned long)pq->rlp->nalloc,
                    (unsigned long)pq->nalloc, 
                    (unsigned long)pq->tqp->nalloc, 
                    (unsigned long)pq->sxp->nalloc);
                status = PQ_CORRUPT;
                goto unwind_map;
        }

        return ENOERR;

unwind_map:
        (void)(pq->mtof)(pq, 0, 0);
        return status;
}


/*
 * Get/lock the ctl for access by this process
 *
 * Returns:
 *      ENOERR      Success.
 *      else        <errno.h> error-code.
 */
static int
ctl_get(pqueue *const pq, int const rflags)
{
        int status = ENOERR;

#if _NOMAP || NO_MMAP
        assert(pq->mtof == f_mtof);
        assert(pq->ctlp == NULL);
        assert(pq->ixp == NULL);
#endif

        if(fIsSet(rflags, RGN_WRITE) && !fIsSet(pq->pflags, PQ_SIGSBLOCKED))
        {
                /* We are beginning a critical section */
                sigset_t set;

                (void) sigfillset(&set);
                (void) sigdelset(&set, SIGABRT);
                (void) sigdelset(&set, SIGFPE);
                (void) sigdelset(&set, SIGILL);
                (void) sigdelset(&set, SIGSEGV);
                (void) sigdelset(&set, SIGBUS);
                if(sigprocmask(SIG_BLOCK, &set, &pq->sav_set) < 0)
                {
                        status = errno;
                        return status;
                }
                fSet(pq->pflags, PQ_SIGSBLOCKED);
        }
        if(pq->ctlp == NULL)
        {
                /* bring in the pqctl */
                status = (pq->ftom)(pq,
                                 0, (size_t)pq->datao, rflags,
                                (void **)&pq->ctlp);
                if(status != ENOERR)
                        goto unwind_mask;
        }
        assert(pq->ctlp->magic == PQ_MAGIC);
        assert(pq->ctlp->version == PQ_VERSION);
        assert(pq->ctlp->datao == pq->datao);
        assert(pq->ctlp->ixo == pq->ixo);
        assert(pq->ctlp->ixsz == pq->ixsz);

        if(pq->ixp == NULL)
        {
                /* bring in the indexes */
                status = (pq->ftom)(pq,
                                 pq->ixo, pq->ctlp->ixsz, rflags|RGN_NOLOCK,
                                &pq->ixp);
                if(status != ENOERR)
                        goto unwind_ctl;
        }

        ix_ptrs(pq->ixp, pq->ixsz, pq->nalloc, pq->ctlp->align, &pq->rlp,
            &pq->tqp, &pq->fbp, &pq->sxp);
        assert(pq->rlp->nalloc == pq->nalloc && pq->tqp->nalloc == pq->nalloc
                        && pq->sxp->nalloc == pq->nalloc);

        return ENOERR;
unwind_ctl:
        (void) (pq->mtof)(pq, 0, 0);
        pq->ctlp = NULL;
unwind_mask:
        {
            sigset_t set = pq->sav_set;

            (void) sigemptyset(&pq->sav_set);
            fClr(pq->pflags, PQ_SIGSBLOCKED);
            (void) sigprocmask(SIG_SETMASK, &set, NULL);
        }
        return status;
}

/* End ctl */
/* Begin rp */

/*
 * Release/unlock a data region.
 */
static int
rgn_rel(pqueue *const pq, off_t const offset, int const rflags)
{
        assert(offset >= pq->datao && offset < pq->ixo);
        return (pq->mtof)(pq, offset, rflags);
}

/*
 * Get/lock a data region.
 *
 * Returns:
 *      0       Success
 *      EACCESS or EAGAIN
 *              "rflags" contains RGN_NOWAIT and the segment of a file to
 *              be locked is already exclusive-locked by another process,
 *              or "rflags" contains RGN_WRITE and some portion of the
 *              segment of the file to be locked is already shared-locked or
 *              exclusive-locked by another process.
 *      EACCES  "pq->fd" is not open for read, regardless of the protection
 *              specified, or "pq->fd" is not open for write and PROT_WRITE was
 *              specified for a MAP_SHARED type mapping.
 *      EAGAIN  The mapping could not be locked in memory, if required by
 *              mlockall(), due to a lack of resources.
 *      EBADF   "pq->fd" is not a valid open file descriptor, or "rflags" 
 *              doesn't contain RGN_WRITE and "pq->fd" is not a valid file
 *              descriptor open for reading, or "rflags" contains RGN_WRITE
 *              and "pq->fd" is not a valid file descriptor open for writing.
 *      EDEADLK "rflags" doesn't contain RGN_NOWAIT, the lock is blocked by some
 *              lock from another process and putting the calling process to
 *              sleep, waiting for that lock to become free would cause a
 *              deadlock.
 *      EFBIG or EINVAL
 *              The "extent" argument was greater than the maximum file size.
 *      EFBIG   The file is a regular file and length is greater than the offset
 *              maximum established in the open file description associated with
 *              "pq->fd".
 *      EINTR   A signal was caught during execution.
 *      EINVAL  "offset", or "extent" is not valid, or "pq->fd" refers to a file
 *              that does not support locking.
 *      EINVAL  "offset" is not a multiple of the page size as returned by 
 *              sysconf(), or is considered invalid by the implementation.
 *      EIO     An I/O error occurred while reading from the file system.
 *      EMFILE  The number of mapped regions would exceed an
 *              implementation-dependent limit (per process or per system).
 *      ENODEV  "pq->fd" refers to a file whose type is not supported by mmap().
 *      ENOLCK  Satisfying the request would result in the number of locked
 *              regions in the system exceeding a system-imposed limit.
 *      ENOMEM  There is insufficient room in the address space to effect the
 *              necessary mapping.
 *      ENOMEM  The mapping could not be locked in memory, if required by
 *              mlockall(), because it would require more space than the system
 *              is able to supply.
 *      ENOMEM  Insufficient memory is available.
 *      ENOTSUP The implementation does not support the combination of accesses
 *              requested in "pq->pflags" and "rflags".
 *      ENXIO   Addresses in the range [offset, offset + extent) are invalid for
 *              the object specified by "pq->fd".
 *      EOVERFLOW
 *              The smallest or, if "extent" is non-zero, the largest offset of
 *              any byte in the requested segment cannot be represented
 *              correctly in an object of type off_t.
 *      EOVERFLOW
 *              The file size in bytes or the number of blocks allocated to the
 *              file or the file serial number cannot be represented correctly.
 *      EOVERFLOW
 *              The file is a regular file and the value of "offset" plus 
 *              "extent" exceeds the offset maximum established in the open file
 *              description associated with "pq->fd". 
 *      EROFS   The file resides on a read-only file system.
 */
static int
rgn_get(pqueue *const pq, off_t const offset, size_t const extent,
         int const rflags, void **const vpp)
{
        assert(offset >= pq->datao && offset < pq->ixo);
        assert(extent >= MIN_RGN_SIZE  && extent <= pq->ixo - pq->datao);

        assert(pq->riulp->nelems <= pq->rlp->nelems +1);

        return (pq->ftom)(pq, offset, extent, rflags, vpp);
}

/* End rp */


/*
 * Free a region, by offset
 */
static int
rpqe_free(pqueue *pq, off_t offset, signaturet signature)
{
        region *rp = NULL;
        size_t rlix;

        rlix = rl_find(pq->rlp, offset);
        if(rlix == RL_NONE)
        {
                uerror("rpqe_free: offset 0x%08lx: Not Found\n",
                        (long)offset);
                return EINVAL;
        }
        rp = pq->rlp->rp + rlix;
        if(IsFree(rp))
        {
                uerror("rpqe_free: 0x%08lx: Already free\n",
                        (long)offset);
                return EINVAL;
        }

        if(sx_find_delete(pq->sxp, signature) == 0)
        {
                uerror("rpqe_free: signature %s: Not Found\n",
                        s_signaturet(NULL, 0, signature));
                return EINVAL;
        }
        rl_free(pq->rlp, rlix);

        return ENOERR;
}


/*
 * Delete the oldest product in the product queue pq that is not
 * locked.  In the unlikely event that all the products in the queue
 * are locked or a deadlock is detected, returns an error status
 * other than ENOERR.  
*/
int
pq_del_oldest(pqueue *pq)
{
    tqelem*             tqep;
    int                 status = ENOERR;
    size_t              rlix;
    region*             rep = NULL;
    void*               vp = NULL;

    assert(pq != NULL);
    assert(pq->ctlp != NULL && pq->tqp != NULL);

    /*
     * Get the data region corresponding to the oldest, unlocked data-product.
     */
    tqep = tqe_first(pq->tqp);
    rlix = rl_find(pq->rlp, tqep->offset);
    assert(rlix != RL_NONE);
    rep = pq->rlp->rp + rlix;
    status = rgn_get(pq, rep->offset, Extent(rep), RGN_WRITE|RGN_NOWAIT, &vp);

    while (status != ENOERR) {
        /*
         * The current data-product is locked.  Try the next one.
         */
        tqep = tq_next(pq->tqp, tqep);
        rlix = rl_find(pq->rlp, tqep->offset);

        if (rlix == RL_NONE) {
            uerror("pq_del_oldest: no unlocked products left to delete!");
            break;
        }

        rep = pq->rlp->rp + rlix;
        status =
            rgn_get(pq, rep->offset, Extent(rep), RGN_WRITE|RGN_NOWAIT, &vp);
    }

    if (status == ENOERR) {
        /*
         * Got the data region.  Start releasing associated entries.
         */
        off_t           offset = rep->offset;
        unsigned char*  signature;

        /*
         * Remove the corresponding entry from the time-list.
         */
        tq_delete(pq->tqp, tqep);

        /*
         * Remove the corresponding entry from the signature-list.
         */
#ifndef XLEN_TIMESTAMPT
#  define XLEN_TIMESTAMPT 8
#endif
        signature = (unsigned char*)vp + XLEN_TIMESTAMPT;

        if (sx_find_delete(pq->sxp, signature) == 0) {
            uerror("pq_del_oldest: signature %s: Not Found\n",
                    s_signaturet(NULL, 0, signature));
            status = EINVAL;
        }

        /*
         * Remove the corresponding entry from the region-list.
         */
        rl_free(pq->rlp, rlix);

        /*
         * Release the data region.
         */
        (void) rgn_rel(pq, offset, 0);
    }                                   /* got data region */

    return status;
}


/*
 * Delete oldest elements until you have space for 'extent'
 * Returns in *rixp the region list index for a suitable region.
 */
static int
rpqe_mkspace(pqueue *const pq, size_t const extent, size_t *rixp)
{
        int status = ENOERR;
        size_t rlix;

        udebug("Deleting oldest to make space %ld bytes", (long)extent);

        do {
                if(pq->rlp->nelems == 0)
                        return ENOMEM;

                status = pq_del_oldest(pq);
                if(status != ENOERR)
                        return status;

                rlix = rl_get(pq->rlp, extent);
        } while (rlix == RL_NONE) ;

        *rixp = rlix;

        return ENOERR;
}

/*
 * Delete oldest elements until a consolidation has occurred, making
 * an rl element available.  If this gets called much, you didn't
 * allocate enough product slots or a big enough queue.
 */
static int
rpqe_mkslot(pqueue *const pq)
{
        int status = ENOERR;

        /* unotice("Deleting oldest to get a queue slot"); */

        do {
                if(pq->rlp->nelems == 0)
                        return ENOMEM;

                status = pq_del_oldest(pq);
                if(status != ENOERR)
                        return status;

        } while (!rl_HasSpace(pq->rlp)) ;

        return ENOERR;
}


/*
 * Allocate a new region from the data section,
 * (which may eventually get handed to the user).
 */
static int
rpqe_new(pqueue *pq, size_t extent, const signaturet sxi,
        void **vpp, sxelem **sxepp)
{
        int status = ENOERR;
        size_t rlix;            /* region list index */
        region *hit = NULL;
        off_t highwater = 0;
        static size_t smallest_extent_seen = UINT_MAX;

        /*
         * Check for duplicate
         */
        if(sx_find(pq->sxp, sxi, sxepp) != 0) {
                udebug("PQUEUE_DUP");
                return PQUEUE_DUP;
        }

        /* We may need to split what we find */
        if(!rl_HasSpace(pq->rlp)) {
          /* get one slot */
          status = rpqe_mkslot(pq);
          if(status != ENOERR)
            return status;
        }

        extent = _RNDUP(extent, pq->ctlp->align);
        if (extent < smallest_extent_seen) {
            smallest_extent_seen = extent;
        }

        rlix = rl_get(pq->rlp, extent);
        if (rlix == RL_NONE) {
            status = rpqe_mkspace(pq, extent, &rlix);
            if(status != ENOERR)
                return status;
        }
        hit = pq->rlp->rp + rlix;
        assert(IsFree(hit));
#define PQ_FRAGMENT_HEURISTIC 64
        /* Don't bother to split off tiny fragments too small for any
           product we've seen */
        if(extent + smallest_extent_seen + PQ_FRAGMENT_HEURISTIC < hit->extent) {
            status = rl_split(pq->rlp, rlix, extent);
            if(status != ENOERR)
                return status;
            hit = pq->rlp->rp + rlix;
        }
                
        assert((hit->offset % pq->ctlp->align) == 0);
        set_IsAlloc(hit);
        rlhash_add(pq->rlp, rlix);

        /* update stats */
        highwater = hit->offset + (off_t)Extent(hit) - pq->ctlp->datao;
        if(highwater > pq->ctlp->highwater)
                pq->ctlp->highwater = highwater;
        
        if(pq->rlp->nelems >  pq->ctlp->maxproducts)
                pq->ctlp->maxproducts = pq->rlp->nelems;
        pq->rlp->nbytes += (off_t)Extent(hit);
        if(pq->rlp->nbytes > pq->rlp->maxbytes)
            pq->rlp->maxbytes = pq->rlp->nbytes;

        status = rgn_get(pq, hit->offset, Extent(hit), RGN_WRITE, vpp);
        if(status != ENOERR)
                return status;

        *sxepp = sx_add(pq->sxp, sxi, hit->offset); 

        return status;
}

/* Begin XDR */

static void *
xinfo_i(void *buf, size_t size, enum xdr_op op,
        prod_info *infop)
{
        XDR xdrs[1] ;

        xdrmem_create(xdrs, buf, (u_int)size, op) ;
        
        if(!xdr_prod_info(xdrs, infop))
        {
                uerror("xinfo:%s xdr_prod_info() failed\n",
                        infop->ident) ;
                return NULL;
        }
        /* return data ptr. Unwarranted intimacy with xdr_mem implementation */
        return xdrs->x_private;
}


/*
 * XDR Encode or Decode "prod" to or from "buf" of size "size".
 */
static ptrdiff_t
xproduct(void *buf, size_t size, enum xdr_op op, product *prod)
{
        XDR xdrs[1] ;
        xdrmem_create(xdrs, buf, (u_int)size, op);

        if (!xdr_product(xdrs, prod))
        {
                uerror("xproduct: %s xdr_product() failed\n",
                        prod->info.ident);
                return 0;
        }

        /*      return xlen_product(prod) ; */
        return (xdrs->x_private - xdrs->x_base);
}


/* End XDR */


/*****  Begin public interface */


const pqe_index _pqenone = {
        OFF_NONE,
        {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
};


/*
 * On success, the writer-counter of the created product-queue will be one.
 */
int
pq_create(const char *path, mode_t mode,
        int pflags,
        size_t align,
        off_t initialsz, /* initial allocation available */
        size_t nproducts, /* initial rl->nalloc, ... */
        pqueue **pqp)
{
        int oflags = (O_RDWR|O_CREAT|O_TRUNC);
        int fd;
        pqueue *pq = NULL;
        int status = ENOERR;

        /* enforce minimum alignment */
        align = (align == 0 ? M_RND_UNIT : _RNDUP(align, M_RND_UNIT));
        /* TODO: check for absurd align values ? */

        if(initialsz != 0)
                initialsz = (off_t) _RNDUP(initialsz, align);
        else
                initialsz = (off_t) align;

        pq = pq_new(pflags, align, initialsz, nproducts);
        if(pq == NULL)
                return errno;

        pq_setAccessFunctions(pq, pq->pflags, initialsz);

        if(fIsSet(pflags, PQ_NOCLOBBER))
                fSet(oflags, O_EXCL);
        fd = open(path, oflags, mode);
        if(fd < 0)
        {
                status = errno;
                goto unwind_new;
        }
        pq->fd = fd;
        
        status = ctl_init(pq, align);
        if(status != ENOERR)
                goto unwind_open;

        *pqp = pq;

        (void) ctl_rel(pq, RGN_MODIFIED);

        return ENOERR;

unwind_open:
        (void)close(fd);
        (void)unlink(path);
        /*FALLTHROUGH*/
unwind_new:
        pq_delete(pq);
        return status;
}


/*
 * Arguments:
 *      path    Pathname of product-queue.
 *      pflags  File-open bit-flags.
 *      pqp     Memory location to receive pointer to product-queue structure.
 * Returns:
 *      0           Success. *pqp set.
 *      EACCESS     Permission denied. pflags doesn't contain PQ_READONLY and 
 *                  the product-queue is already open by the maximum number of
 *                  writers.
 *      PQ_CORRUPT  The  product-queue is internally inconsistent.
 *      else        Other <errno.h> error-code.
 */
int
pq_open(
    const char* const path,
    int               pflags,
    pqueue** const    pqp)
{
    int               status;
    pqueue*           pq = pq_new(pflags, M_RND_UNIT, 0, 0);

    if (NULL == pq) {
        status = errno;
    }
    else {
        pq->fd = open(path, fIsSet(pflags, PQ_READONLY) ? O_RDONLY : O_RDWR, 0);

        if (0 > pq->fd) {
            status = errno;
        }
        else {
            status = ctl_gopen(pq, path);

            if (!status) {
                (void)ctl_rel(pq, 0);           /* release control-block */

                if (!fIsSet(pflags, PQ_READONLY)) {
                    status = ctl_get(pq, RGN_WRITE);

                    if (!status) {
                        int      rflags = 0;    /* control-block unmodified */
                        pqctl*   ctlp = pq->ctlp;
                        unsigned count;

                        if (WRITE_COUNT_MAGIC != ctlp->write_count_magic) {
                            /*
                             * This process is the first one of this version of
                             * the LDM to open the product-queue for writing.
                             * Initialize the "write count" mechanism.
                             */
                            ctlp->write_count_magic = WRITE_COUNT_MAGIC;
                            ctlp->write_count = 0;
                            rflags = RGN_MODIFIED;
                        }

                        count = ctlp->write_count;

                        if (MAX_WRITE_COUNT > count) {
                            ctlp->write_count++;

                            rflags = RGN_MODIFIED;
                        }
                        else {
                            uerror("Too many writers (%u) to product-queue "
                                "(%s)", count, path);

                            status = EACCES;    /* too many writers */
                        }
                        (void)ctl_rel(pq, rflags);
                    }                           /* ctl_get() success */
                }                               /* open for writing */
            }                                   /* ctl_gopen() success */

            if (status) {
                (void)close(pq->fd);
                pq->fd = -1;
            }
        }                                       /* pq->fd >= 0 */

        if (status) {
            pq_delete(pq);
        }
        else {
            *pqp = pq;
        }
    }                                           /* pq != NULL */

    return status;
}


/*
 * On success, if the product-queue was open for writing, then its 
 * writer-counter will be decremented.
 *
 * Returns:
 *      0               Success.
 *      EOVERFLOW       Write-count of product queue was prematurely zero.
 *      !0              Other <errno.h> code.
 */
int
pq_close(pqueue *pq)
{
        int status = ENOERR;                    /* success */
        int fd = -1;

        if (pq == NULL)
                return 0;

        fd = pq->fd;

        if(pq->riulp != NULL)
        {
                while(pq->riulp->nelems > 2)
                {
                        off_t offset =
                                pq->riulp->rp[pq->riulp->nelems -1].offset;
                        if(offset == pq->ixo || offset == 0)
                                continue;
                        (void) (pq->mtof)(pq, offset, 0);
                }
        }

        if(pq->rlp != NULL)
        {
                (void)(pq->mtof)(pq, pq->ixo, RGN_NOLOCK);
                pq->rlp = NULL;
        }

        if (fIsSet(pq->pflags, PQ_READONLY)) {
            if (NULL != pq->ctlp)
                (void)ctl_rel(pq, 0);
        }
        else {
            int rflags;

            status = ctl_get(pq, RGN_WRITE);

            if (!status) {
                if (0 < pq->ctlp->write_count) {
                    pq->ctlp->write_count--;
                    rflags = RGN_MODIFIED;
                }
                else {
                    uerror("Write-count of product-queue prematurely 0");

                    rflags = 0;                 /* unmodified */
                    status = EOVERFLOW;
                }

                (void)ctl_rel(pq, rflags);
            }
        }                                       /* was opened for writing */

#ifndef NO_MMAP
        if(pq->base != NULL && pq->ftom == mm0_ftom)
        {
                /* special case, time to unmap the whole thing */
                int mflags = 0; /* TODO: translate rflags to mflags */
                (void) unmapwrap(pq->base, 0, pq->ixo + pq->ixsz, mflags);
                pq->base = NULL;
        }
#endif /*!NO_MMAP*/

        pq_delete(pq);
        
        if(fd > -1 && close(fd) < 0 && !status)
                status = errno;

        return status;
}


/*
 * Let the user find out the pagesize.
 */
int
pq_pagesize(const pqueue *pq)
{
        /*
         * Allow 'em to figure out what the default
         *      would be, prior to pq_create().
         */
        if(pq == NULL)
                return (int)pagesize();
        /* else, tell'em what it is */
        return (int) pq->pagesz;
}


int
pqe_new(pqueue *pq,
        const prod_info *infop,
        void **ptrp, pqe_index *indexp)
{
        int status = ENOERR;
        size_t extent;
        void *vp = NULL;
        sxelem *sxep;

        assert(pq != NULL);
        assert(infop != NULL);
        assert(ptrp != NULL);
        assert(indexp != NULL);
        
        if(infop->sz == 0) {
                uerror("pqe_new(): zero product size");
                return EINVAL;  
        }

        if (infop->sz > (pq->ixo - pq->datao)) {
                uerror("pqe_new(): product too big");
                return PQUEUE_BIG;  
        }

        if(fIsSet(pq->pflags, PQ_READONLY))
                return EACCES;

        /*
         * Write lock pq->xctl.
         */
        status = ctl_get(pq, RGN_WRITE);
        if(status != ENOERR) {
                udebug("pqe_new(): ctl_get() failure");
                return status;
        }

/* */
        extent = xlen_prod_i(infop);
        status = rpqe_new(pq, extent, infop->signature, &vp, &sxep);
        if(status != ENOERR) {
                udebug("pqe_new(): rpqe_new() failure");
                goto unwind_ctl;
        }

                                                /* cast away const'ness */
        *ptrp = xinfo_i(vp, extent, XDR_ENCODE, (prod_info *)infop);
        if(*ptrp == NULL)
        {
                udebug("pqe_new(): xinfo_i() failure");
                status = EIO;
                goto unwind_ctl;
        }

        assert(((char *)(*ptrp) + infop->sz) <= ((char *)vp + extent));

        indexp->offset = sxep->offset;
        memcpy(indexp->signature, sxep->sxi, sizeof(signaturet));
/* */
        /*FALLTHROUGH*/
unwind_ctl:
        (void) ctl_rel(pq, RGN_MODIFIED);
        return status;
}


int
pqe_discard(pqueue *pq, pqe_index index)
{
        int status = ENOERR;
        off_t offset = pqeOffset(index);

        status = (pq->mtof)(pq, offset, 0);
        if(status != ENOERR)
                return status;

        /*
         * Write lock pq->xctl.
         */
        status = ctl_get(pq, RGN_WRITE);
        if(status != ENOERR)
                return status;

        status = rpqe_free(pq, offset, index.signature);
        
        (void) ctl_rel(pq, RGN_MODIFIED);
        return status;
}


/*
 * LDM 4 convenience funct.
 * Change signature, Insert at rear of queue, send SIGCONT to process group
 */
int
pqe_xinsert(pqueue *pq, pqe_index index, const signaturet realsignature)
{
        int status = ENOERR;
        off_t offset = pqeOffset(index);

        /* correct the signature in the product */
        {
                riu *rp = NULL;
                char *xp;
                if(riul_r_find(pq->riulp, offset, &rp) == 0)
                {
                        uerror("pqe_xinsert: Couldn't riul_r_find %ld",
                                (long)offset);
                        return EINVAL;
                }
                xp = rp->vp;
                assert(xp != NULL);
                xp += 8; /* xlen_timestampt */
                memcpy(xp, realsignature, sizeof(signaturet));
        }

        status =  (pq->mtof)(pq, offset, RGN_MODIFIED);
        if(status != ENOERR)
                return status;

        /*
         * Write lock pq->xctl.
         */
        status = ctl_get(pq, RGN_WRITE);
        if(status != ENOERR)
                return status;

        {
          sxelem *sxep;
          /*
           * Check for duplicate
           */
          if(sx_find(pq->sxp, realsignature, &sxep) != 0)
            {
              udebug("PQUEUE_DUP");
              status = PQUEUE_DUP;
              (void) rpqe_free(pq, offset, index.signature);
              goto unwind_ctl;
            }
          /* else */
          /* correct the signature in the index */
          
          if(sx_find_delete(pq->sxp, index.signature) == 0)
            {
              uerror("pqe_xinsert: old signature %s: Not Found\n",
                     s_signaturet(NULL, 0, index.signature));
            }
          sxep = sx_add(pq->sxp, realsignature, offset); 
        }

        assert(pq->tqp != NULL && tq_HasSpace(pq->tqp));

        status = tq_add(pq->tqp, offset);
        if(status != ENOERR)
                goto unwind_ctl;
        
        /*
         * Inform others in our process group
         * that there is new data available.
         * (see pq_suspend() below.)
         *  SIGCONT is ignored by default...
         */
        (void)kill(0, SIGCONT);

        /*FALLTHROUGH*/
unwind_ctl:
        (void) ctl_rel(pq, RGN_MODIFIED);
        return status;
}

/*
 * Insert at rear of queue, send SIGCONT to process group
 */
int
pqe_insert(pqueue *pq, pqe_index index)
{
        int status = ENOERR;
        off_t offset = pqeOffset(index);

        status =  (pq->mtof)(pq, offset, RGN_MODIFIED);
        if(status != ENOERR)
                return status;

        /*
         * Write lock pq->xctl.
         */
        status = ctl_get(pq, RGN_WRITE);
        if(status != ENOERR)
                return status;

        assert(pq->tqp != NULL && tq_HasSpace(pq->tqp));

        status = tq_add(pq->tqp, offset);
        if(status != ENOERR)
                goto unwind_ctl;
        
        /*
         * Inform others in our process group
         * that there is new data available.
         * (see pq_suspend() below.)
         *  SIGCONT is ignored by default...
         */
        (void)kill(0, SIGCONT);

        /*FALLTHROUGH*/
unwind_ctl:
        (void) ctl_rel(pq, RGN_MODIFIED);
        return status;
}


typedef struct {
    time_t      start;
    /*
     * Hostnames are limited to 255 bytes.  See
     * <http://www.opengroup.org/onlinepubs/007908799/xns/gethostname.html>.
     */
    char        hostname[256];
} FutureEntry;


static int
compareFutureEntries(
    const void* const   entry1,
    const void* const   entry2)
{
    const char* name1 = ((FutureEntry*)entry1)->hostname;
    const char* name2 = ((FutureEntry*)entry2)->hostname;

    return strcmp(name1, name2);
}


static void
vetCreationTime(
    const prod_info* const      info)
{
    /*
     * Vet the creation-time of a data-product.
     */
    timestampt  initialSearchTime = info->arrival;
    timestampt  now;

    /*
     * Keep the following consonant with the temporal backoff in
     * pq_setCursorFromSignature().
     */
    initialSearchTime.tv_sec -= SEARCH_BACKOFF;

    (void)set_timestamp(&now);

    if (tvCmp(initialSearchTime, now, >)) {
        const char* const       origin = info->origin;
        char*                   cp = strstr(origin, "_v_");
        size_t                  len =
            cp == NULL ?  strlen(origin) : (size_t)(cp - origin);

        if (ulogIsVerbose()) {
            uwarn("Future product from \"%*s\".  Fix local or ingest clock. %s",
                (int)len, origin, s_prod_info(NULL, 0, info, 0));
        }
        else {
            FutureEntry         targetEntry;
            FutureEntry* const* treeEntry;
            static void*        root = NULL;
#           define              FUTURE_INTERVAL (60*60) /* 1 h */

            if (len > (sizeof(targetEntry.hostname)-1))
                len = sizeof(targetEntry.hostname) - 1;
            (void)memcpy(targetEntry.hostname, origin, len);
            targetEntry.hostname[len] = 0;

            treeEntry = tfind(&targetEntry, &root, compareFutureEntries);

            if (treeEntry != NULL) {
                FutureEntry*    entry = *treeEntry;
                time_t          now = time(NULL);

                if (entry->start <= now) {
                    uwarn("Future product from \"%s\".  Fix local or ingest "
                        "clock. %s", entry->hostname,
                        s_prod_info(NULL, 0, info, 0));

                    entry->start = now + FUTURE_INTERVAL;
                }
            }
            else {
                FutureEntry*    newEntry = malloc(sizeof(FutureEntry));

                uwarn("Future product from \"%s\".  Fix local or ingest "
                    "clock. %s", targetEntry.hostname,
                    s_prod_info(NULL, 0, info, 0));

                if (newEntry != NULL) {
                    newEntry->start = time(NULL) + FUTURE_INTERVAL;
                    (void)strcpy(newEntry->hostname, targetEntry.hostname);

                    (void)tsearch(newEntry, &root, compareFutureEntries);
                }                       /* "newEntry" allocated */
            }                           /* host not in database */
        }                               /* non-verbose logging */
    }                                   /* product created in future */
}


/*
 * Insert at rear of queue
 * (Don't signal process group.)
 *
 * Returns:
 *      ENOERR  Success.
 *      EINVAL  Invalid argument.
 *      PQUEUE_DUP      Product already exists in the queue.
 *      PQUEUE_BIG      Product is too large to insert in the queue.
 */
int
pq_insertNoSig(pqueue *pq, const product *prod)
{
        int status = ENOERR;
        size_t extent;
        void *vp = NULL;
        sxelem *sxep;
        
        assert(pq != NULL);
        assert(prod != NULL);

        if(fIsSet(pq->pflags, PQ_READONLY)) {
                udebug("pq_insertNoSig(): queue is read-only");
                return EACCES;
        }

        if (prod->info.sz > (pq->ixo - pq->datao)) {
                udebug("pq_insertNoSig(): product is too big");
                return PQUEUE_BIG;
        }

        /*
         * Write lock pq->ctl.
         */
        status = ctl_get(pq, RGN_WRITE);
        if(status != ENOERR) {
                udebug("pq_insertNoSig(): ctl_get() failure");
                return status;
        }

/* */
        extent = xlen_product(prod);
        status = rpqe_new(pq, extent, prod->info.signature, &vp, &sxep);
        if(status != ENOERR) {
                udebug("pq_insertNoSig(): rpqe_new() failure");
                goto unwind_ctl;
        }

                                                /* cast away const'ness */
        if(xproduct(vp, extent, XDR_ENCODE, (product *)prod) == 0)
        {
                udebug("pq_insertNoSig(): xproduct() failure");
                status = EIO;
                goto unwind_rgn;
        }

        assert(pq->tqp != NULL && tq_HasSpace(pq->tqp));
        status = tq_add(pq->tqp, sxep->offset);
        if(status != ENOERR) {
                udebug("pq_insertNoSig(): tq_add() failure");
                goto unwind_rgn;
        }

        vetCreationTime(&prod->info);

        /*FALLTHROUGH*/
unwind_rgn:
        (void) rgn_rel(pq, sxep->offset, status == ENOERR ? RGN_MODIFIED : 0);

        /*FALLTHROUGH*/
unwind_ctl:
        (void) ctl_rel(pq, RGN_MODIFIED);
        return status;
}


/*
 * Insert at rear of queue, send SIGCONT to process group
 *
 * Returns:
 *      ENOERR  Success.
 *      EINVAL  Invalid argument.
 *      PQUEUE_DUP      Product already exists in the queue.
 *      PQUEUE_BIG      Product is too large to insert in the queue.
 */
int
pq_insert(pqueue *pq, const product *prod)
{
        int status = pq_insertNoSig(pq, prod);
        if(status == ENOERR)
        {
                /*
                 * Inform others in our process group
                 * that there is new data available.
                 * (see pq_suspend() below.)
                 *  SIGCONT is ignored by default...
                 */
                (void)kill(0, SIGCONT);
        }
        return status;
}


/*
 * Get some useful statistics.
 */
int
pq_highwater(pqueue *pq, off_t *highwaterp, size_t *maxproductsp)
{
        /* Read lock pq->xctl. */
        int status = ctl_get(pq, 0);
        if(status != ENOERR)
                return status;
        if(highwaterp)
                *highwaterp = pq->ctlp->highwater;
        if(maxproductsp)
                *maxproductsp = pq->ctlp->maxproducts;
        (void) ctl_rel(pq, 0);

        return status;
}


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
int
pq_stats(pqueue *pq,
         size_t *nprodsp,   size_t *nfreep,   size_t *nemptyp,   size_t *nbytesp,
         size_t *maxprodsp, size_t *maxfreep, size_t *minemptyp, size_t *maxbytesp, 
         double *age_oldestp, size_t *maxextentp)
{
    /* Read lock pq->ctl. */
    int status = ctl_get(pq, 0);
    if(status != ENOERR)
        return status;
    if(nprodsp)
        *nprodsp = pq->rlp->nelems;
    if(nfreep)
        *nfreep = pq->rlp->nfree;
    if(maxextentp)
        *maxextentp = pq->rlp->maxfextent;
    if(nemptyp)
        *nemptyp = pq->rlp->nempty;
    if(nbytesp)
        *nbytesp = pq->rlp->nbytes;
    if(maxprodsp)
        *maxprodsp = pq->rlp->maxelems;
    if(maxfreep)
        *maxfreep = pq->rlp->maxfree;
    if(minemptyp)
        *minemptyp = pq->rlp->minempty;
    if(maxbytesp)
        *maxbytesp = pq->rlp->maxbytes;
    if(age_oldestp) {
        timestampt ts0;
        tqelem *tqep;

        tqep = tqe_first(pq->tqp); /* get oldest */
        if(tqep != NULL) {
          set_timestamp(&ts0);
          *age_oldestp = d_diff_timestamp(&ts0, &tqep->tv);
        } else {
          *age_oldestp = 0;
        }
    }


    (void) ctl_rel(pq, 0);

    return status;
}


/*
 * Returns the insertion-timestamp of the oldest data-product in the
 * product-queue.
 *
 * Arguments:
 *      oldestCursor    Pointer to structure to received the insertion-time
 *                      of the oldest data-product.
 * Returns:
 *      ENOERR          Success.
 *      else            Failure.
 */
int
pq_getOldestCursor(
    pqueue*             pq,
    timestampt* const   oldestCursor)
{
    int                 status = ctl_get(pq, 0);

    if (status == ENOERR) {
        tqelem*         tqep = tqe_first(pq->tqp);

        if (tqep == NULL) {
            *oldestCursor = TS_NONE;
        }
        else {
            *oldestCursor = tqep->tv;
        }

        (void)ctl_rel(pq, 0);
    }                                   /* control region locked */

    return status;
}


/*
 * Returns the number of pq_open()s for writing outstanding on an existing
 * product queue.  If a writing process terminates without calling pq_close(),
 * then the actual number will be less than this number.  This function opens
 * the product-queue read-only, so if there are no outstanding product-queue
 * writers, then the returned count will be zero.
 *
 * Arguments:
 *      path    The pathname of the product-queue.
 *      count   The memory to receive the number of writers.
 * Returns:
 *      0           Success.  *count will be the number of writers.
 *      EINVAL      path is NULL or count is NULL.  *count untouched.
 *      ENOSYS      Function not supported because product-queue doesn't support
 *                  writer-counting.
 *      PQ_CORRUPT  The  product-queue is internally inconsistent.
 *      else        <errno.h> error-code.  *count untouched.
 */
int
pq_get_write_count(const char* const path, unsigned* const count)
{
    int status;

    if (NULL == path || NULL == count) {
        status = EINVAL;
    }
    else {
        pqueue* pq;

        status = pq_open(path, PQ_READONLY, &pq);

        if (!status) {
            /*
             * Get the control-block.
             */
            status = ctl_get(pq, 0);            /* readonly */

            if (!status) {
                pqctl*  ctlp = pq->ctlp;

                if (WRITE_COUNT_MAGIC != ctlp->write_count_magic) {
                    status = ENOSYS;
                }
                else {
                    *count = pq->ctlp->write_count;
                }
                
                (void)ctl_rel(pq, 0);
            }

            (void)pq_close(pq);
        }
    }

    return status;
}


/*
 * Sets to zero the number of pq_open()s for writing outstanding on the
 * product-queue.  This is a dangerous function and should only be used when
 * it is known that there are no outstanding pq_open()s for writing on the
 * product-queue.
 *
 * Arguments:
 *      path    The pathname of the product-queue.
 * Returns:
 *      0           Success.
 *      EINVAL      path is NULL.
 *      PQ_CORRUPT  The  product-queue is internally inconsistent.
 *      else        <errno.h> error-code.
 */
int
pq_clear_write_count(const char* const path)
{
    int status;

    if (NULL == path) {
        status = EINVAL;
    }
    else {
        pqueue* pq;

        status = pq_open(path, 0, &pq);         /* open for writing */

        if (!status) {
            status = ctl_get(pq, RGN_WRITE);

            if (!status) {
                int    rflags = 0;              /* control-block unmodified */
                pqctl* ctlp = pq->ctlp;

                if (ctlp->write_count != 1) {
                    ctlp->write_count = 1;      /* pq_close() will decrement */
                    rflags = RGN_MODIFIED;      /* control-block modified */
                }

                ctl_rel(pq, rflags);
            }

            (void)pq_close(pq);
        }
    }

    return status;
}


/*
 * For debugging: dump extents of regions on free list, in order by extent.
 */
int
pq_fext_dump(pqueue *const pq)
{
    regionl *rl;
    region *rlrp;
    size_t spix;
    size_t sqix;
    const region *spp;
    fb *fbp;
#if !defined(NDEBUG)
    size_t prev_extent = 0;
#endif


    /* Read lock pq->ctl. */
    int status = ctl_get(pq, 0);
    if(status != ENOERR)
        return status;
    rl = pq->rlp;
    rlrp = rl->rp;
    fbp = pq->fbp;

    /* p = l->header; */
    spix = rl->fext;    /* head of skip list by extent */
    spp = rlrp + spix;
    /* q = p->forward[0]; */
    sqix = fbp->fblks[spp->prev];
    udebug("** Free list extents:\t");                  /* debugging */
    while(sqix != RL_FEXT_TL) {
        /* p = q */
        spix = sqix;
        spp = rlrp + spix;
        udebug("%u ", spp->extent);             /* debugging */
#if !defined(NDEBUG)
        assert(spp->extent >= prev_extent);
        prev_extent = spp->extent;
#endif
        /* q = p->forward[0]; */
        sqix = fbp->fblks[spp->prev];
    }
    (void) ctl_rel(pq, 0);
    return status;
}



/*
 * Set cursor used by pq_sequence() or pq_seqdel().
 */
void
pq_cset(pqueue *pq, const timestampt *tvp)
{
        assert(    tvp->tv_sec  >= TS_ZERO.tv_sec
                && tvp->tv_usec >= TS_ZERO.tv_usec
                && tvp->tv_sec  <= TS_ENDT.tv_sec
                && tvp->tv_usec <= TS_ENDT.tv_usec);
        pq->cursor = *tvp;
        if(tvEqual(*tvp, TS_ENDT)) {
            pq->cursor_offset = OFF_NONE;
        } else if (tvEqual(*tvp, TS_ZERO)) {
            pq->cursor_offset = 0;
        }
}



/*
 * Set cursor_offset used by pq_sequence() to disambiguate among
 * multiple products with identical queue insertion times.
 */
void
pq_coffset(pqueue *pq, off_t c_offset)
{
        pq->cursor_offset = c_offset;
}


/*
 * Get current cursor value used by pq_sequence() or pq_seqdel().
 */
void
pq_ctimestamp(const pqueue *pq, timestampt *tvp)
{
        *tvp = pq->cursor;
}


int
pq_cClassSet(pqueue *pq,  pq_match *mtp, const prod_class *clssp)
{
        int status = ENOERR;
        pq_match otherway = TV_LT;
        tqelem *tqep;

        if(clssp == NULL || tvIsNone(clssp->from) || tvIsNone(clssp->to))
                return EINVAL;

        pq_cset(pq, &clssp->from);

        if(tvCmp(clssp->from, clssp->to, >))
        {
                /* reversed scan */
                if(tvEqual(clssp->from, TS_ENDT))
                {
                        /* Edge case */
                        if(mtp != NULL)
                                *mtp = TV_LT;
                        return ENOERR;
                }
                /* else */
                otherway = TV_GT;
        }
        else
        {
                if(tvEqual(clssp->from, TS_ZERO))
                {
                        /* Edge case */
                        if(mtp != NULL)
                                *mtp = TV_GT;
                        return ENOERR;
                }
        }

        /* Read lock pq->xctl.  */
        status = ctl_get(pq, 0);
        if(status != ENOERR)
                return status;

        /* find specified que element just outside the clssp time range */
        tqep = tqe_find(pq->tqp, &clssp->from, otherway);
        if(tqep != NULL)
        {
                /* update cursor */
                pq_cset(pq, &tqep->tv);
                pq_coffset(pq, tqep->offset);
        }
        if(mtp != NULL)
                *mtp = otherway == TV_LT ? TV_GT : TV_LT;
        (void) ctl_rel(pq, 0);
        return status;
}


/*
 * The control-region is assumed to be locked.
 *
 * Arguments:
 *      pq      Pointer to the product-queue structure.
 *      offset  The offset to the region in the product-queue that contains
 *              the data-product.
 *      infoBuf Pointer to a info-buffer for data-product metadata.  ib_init()
 *              will be called on the argument.
 * Returns:
 *      0       Success.  "info" is set.
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
 *      EIO     The metadata of the data-product in the product-queue at offset
 *              "offset" could not be decoded.
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
 *      PQ_NOTFOUND
 *              There is no data-product at the given offset.
 */
static int
getMetadataFromOffset(
    pqueue* const       pq,
    const off_t         offset,
    InfoBuf* const      infoBuf)
{
    int                         status;
    const regionl* const        rlp = pq->rlp;
    const size_t                rlix = rl_find(rlp, offset);

    if (RL_NONE == rlix) {
        status = PQ_NOTFOUND;
    }
    else {
        void*                   vp;
        const region* const     rp = rlp->rp + rlix;
        const size_t            extent = Extent(rp);

        /*
         * Lock the data-product's data-region.
         */
        status = rgn_get(pq, offset, extent, 0, &vp);

        if (0 != status) {
            serror("getMetadataFromOffset(): Couldn't lock data-product's "
                    "data-region in product-queue");
        }
        else {
            prod_info* const    info = &infoBuf->info;
            XDR                 xdrs[1];

            xdrmem_create(xdrs, vp, extent, XDR_DECODE);
            ib_init(infoBuf);

            /*
             * Decode the data-product's metadata.
             */
            if (!xdr_prod_info(xdrs, info)) {
                uerror("getMetadataFromOffset(): xdr_prod_info() failed");

                status = EIO;
            }
            else {
                status = 0;             /* success */
            }

            xdr_destroy(xdrs);

            (void)rgn_rel(pq, offset, 0);
        }                               /* data-product region locked */
    }                                   /* associated data-product exists */

    return status;
}


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
    const signaturet    signature)
{
    int                 status = 0;     /* success */

    /*
     * Read-lock the control-region of the product-queue.
     */
    status = ctl_get(pq, 0);

    if (ENOERR != status) {
        serror("pq_setCursorFromSignature(): Couldn't lock control-region "
            "of product-queue");
    }
    else {
        const sxelem*   signatureEntry;

        /*
         * Get the relevant entry in the signature-map.
         */
        if (!sx_find(pq->sxp, signature, (sxelem**)&signatureEntry)) {
            status = PQ_NOTFOUND;
        }
        else {
            InfoBuf     infoBuf;
            prod_info*  info = &infoBuf.info;

            /*
             * Get the metadata of the data-product referenced by the 
             * signature-entry.
             */
            status = 
                getMetadataFromOffset(pq, signatureEntry->offset, &infoBuf);

            if (PQ_NOTFOUND == status) {
                uerror("pq_setCursorFromSignature(): data-product region "
                    "of signature-map entry doesn't exist");

                status = PQ_CORRUPT;
            }
            else if (0 == status) {
                timestampt          creationTime = info->arrival;
                timestampt          start = creationTime;
                const tqelem*       timeEntry;

                /*
                 * Start the time-map search beginning a little before
                 * the creation-time of the target data-product.  This will
                 * work if 1) the data-product is in the queue; and 2) the
                 * clock on the origination host agrees with the clock on
                 * this host.
                 *
                 * Keep the following consonant with the temporal backoff in
                 * vetCreationTime().
                 */
                start.tv_sec -= SEARCH_BACKOFF;
                timeEntry = tqe_find(pq->tqp, &start, TV_LT);

                if (NULL == timeEntry) {
                    timeEntry = tqe_find(pq->tqp, &start, TV_EQ);

                    if (NULL == timeEntry)
                        timeEntry = tqe_find(pq->tqp, &start, TV_GT);
                }

                if (NULL == timeEntry) {
                    uerror("pq_setCursorFromSignature(): "
                        "The product-queue appears to be empty");

                    status = PQ_CORRUPT;
                }
                else {
                    /*
                     * Search forward in the time-map from the initial entry to
                     * find the matching entry.
                     */
                    const tqelem*       initialTimeEntry = timeEntry;
                    const fb* const     fbp =
                        (fb*)((char*)pq->tqp + pq->tqp->fbp_off);

                    for (;;) {
                        if (OFF_NONE == timeEntry->offset) {
                            /*
                             * The current entry is the end of the 
                             * product-queue.
                             */
                            status = PQ_NOTFOUND;
                            break;
                        }               /* entry is end-of-queue */

                        if (timeEntry->offset == signatureEntry->offset) {
                            /*
                             * Found it.  Set the cursor and stop searching.
                             */
                            pq_cset(pq, &timeEntry->tv);
                            pq_coffset(pq, timeEntry->offset);
                            break;
                        }

                        /*
                         * Advance to the very next entry in the time-map.
                         */
                        timeEntry = &pq->tqp->tqep[fbp->fblks[timeEntry->fblk]];
                    }                   /* time-map entry loop */

                    if (status == PQ_NOTFOUND) {
                        /*
                         * The data-product wasn't found.  This could be
                         * because of an egregious discrepancy between the
                         * clock on the origination system and this system's
                         * clock (the origination clock might be fast, our
                         * clock might be slow, or both).  Therefore, search
                         * from the beginning of the product-queue to the
                         * initial data-product (sigh).
                         */
                        timeEntry = tqe_find(pq->tqp, &TS_ZERO, TV_GT);

                        for (;;) {
                            if (initialTimeEntry == timeEntry) {
                                /*
                                 * The current entry is the initial entry.
                                 */
                                break;
                            }

                            if (timeEntry->offset == signatureEntry->offset) {
                                /*
                                 * Found it.  Set the cursor and stop searching.
                                 */
                                pq_cset(pq, &timeEntry->tv);
                                pq_coffset(pq, timeEntry->offset);
                                status = 0;
                                break;
                            }

                            /*
                             * Advance to the very next entry in the time-map.
                             */
                            timeEntry =
                                &pq->tqp->tqep[fbp->fblks[timeEntry->fblk]];
                        }               /* time-map entry loop */
                    }                   /* product not where it should be */
                }                       /* non-empty product-queue */
            }                           /* have product metadata */
        }                               /* have signature-map entry */

        /*
         * Release control-region of product-queue.
         */
        (void)ctl_rel(pq, 0);
    }                                   /* control region locked */

    return status;
}


/*
 * Step thru the time sorted inventory according to 'mt',
 * and the current cursor value.
 *
 * If no product is in the inventory which meets the
 * spec, return PQUEUE_END.
 *
 * Otherwise, if the product info matches class,
 * execute ifMatch(xprod, len, otherargs) and return the
 * return value from ifMatch().
 */
int
pq_sequence(pqueue *pq, pq_match mt,
        const prod_class *clss, pq_seqfunc *ifMatch, void *otherargs)
{
        int status = ENOERR;
        tqelem *tqep;
        region *rp = NULL;
        off_t  offset = OFF_NONE;
        size_t extent = 0;
        void *vp = NULL;
        struct infobuf
        {
                prod_info b_i;
                char b_origin[HOSTNAMESIZE + 1];
                char b_ident[KEYSIZE + 1];
        } buf; /* static ??? */
        prod_info *info ;
        void *datap;
        XDR xdrs[1] ;
        timestampt pq_time;

        if(pq == NULL)
                return EINVAL;  

        /* all this to avoid malloc in the xdr calls */
        (void) memset(&buf, 0, sizeof(buf));
        info = &buf.b_i;
        info->origin = &buf.b_origin[0];
        info->ident = &buf.b_ident[0];
        
        /* if necessary, initialize cursor */
        if(tvIsNone(pq->cursor))
        {
                assert(mt != TV_EQ);
                if(mt == TV_LT) {
                        pq->cursor = TS_ENDT;
                }
                else {
                        pq->cursor = TS_ZERO;
                }
        }

        /* Read lock pq->xctl.  */
        status = ctl_get(pq, 0);
        if(status != ENOERR)
                return status;

        /* find the specified que element */
        tqep = tqe_find(pq->tqp, &pq->cursor, mt);
        if(tqep == NULL)
        {
                status = PQUEUE_END;
                goto unwind_ctl;
        }
        /* update cursor */
        /* pq->cursor = tqep->tv; */
        pq_cset(pq, &tqep->tv);
        pq_coffset(pq, tqep->offset);

        /*
         * Spec'ing clss NULL or ifMatch NULL
         * _just_ sequences cursor.
         * This feature used by the 'pqexpire' program.
         */
        if(clss == NULL || ifMatch == NULL)
        {
                udebug("pq_sequence NOOP");
                goto unwind_ctl;
        }
        /* else */

        /* get the actual data region */
        status = rl_r_find(pq->rlp, tqep->offset, &rp);
        if(status == 0
                 || rp->offset != tqep->offset
                 || Extent(rp) > pq->ixo - pq->datao
                )
        {
                char ts[20];
                (void) sprint_timestampt(ts, sizeof(ts), &tqep->tv);
                uerror("Queue corrupt: tq: %s %s at %ld",
                        ts,
                        status ? "invalid region" : "no data",
                        tqep->offset);
                /*
                 * we can't fix it (tq_delete(pq->tqp, tqep)) here
                 * since we don't have write permission
                 */
                status = ENOERR;
                goto unwind_ctl;
        }
        status = rgn_get(pq, rp->offset, Extent(rp), 0, &vp);
        if(status != ENOERR)
        {
                goto unwind_ctl;
        }
        assert(vp != NULL);
        offset = rp->offset;
        extent = Extent(rp);

        if(ulogIsDebug()) {     /* delay to process product, useful to see if it's falling behind */
          timestampt now;
          pq_time = tqep->tv;
          if(gettimeofday(&now, 0) == 0) {
            double delay = d_diff_timestamp(&now, &tqep->tv);
            udebug("Delay: %.4f sec", delay);
          }
        }

        /* We've got the data, so we can let go of the ctl */
        status = ctl_rel(pq, 0);
        if(status != ENOERR)
                goto unwind_rgn;

        /*
         * Decode it
         */
        xdrmem_create(xdrs, vp, (u_int)extent, XDR_DECODE) ;

        if(!xdr_prod_info(xdrs, info))
        {
                uerror("pq_sequence: xdr_prod_info() failed\n") ;
                status = EIO;
                goto unwind_rgn;
        }

        assert(info->sz <= xdrs->x_handy);
        /* rather than copy the data, just use the existing buffer */
        datap = xdrs->x_private;

#if PQ_SEQ_TRACE
        udebug("%s %u",
                s_prod_info(NULL, 0, info, 1), xdrs->x_handy) ;
#endif

        /*
         * Log time-interval from product-creation to queue-insertion.
         */
        if(ulogIsDebug()) {
            double latency = d_diff_timestamp(&pq_time, &info->arrival);
            udebug("pq_sequence(): time(insert)-time(create): %.4f s", latency);
        }

        /*
         * Do the work.
         */
        assert(clss != NULL);
        if(clss == PQ_CLASS_ALL || prodInClass(clss, info))
        {
                /* do the ifMatch function */
                assert(ifMatch != NULL);
                {
                        /* change extent into xlen_product */
                        const size_t xsz = _RNDUP(info->sz, 4);
                        if(xdrs->x_handy > xsz)
                        {
                                extent -= (xdrs->x_handy - xsz);
                        }
                }
                status =  (*ifMatch)(info, datap,
                                vp, extent, otherargs);
                if(status)
                  {             /* back up, presumes clock tick > usec (not always true) */
                        if(mt == TV_GT) {
                                timestamp_decr(&pq->cursor);
                                pq_coffset(pq, OFF_NONE);
                        }
                        else if(mt == TV_LT) {
                                pq_coffset(pq, offset + 1);
                        }
                  }
        }

        /*FALLTHROUGH*/
unwind_rgn:
        /* release the data segment */
        (void) rgn_rel(pq, offset, 0);

        return status;
unwind_ctl:
        (void) ctl_rel(pq, 0);
        return status;
}


int
pq_ctimeck(const pqueue *pq, pq_match mt, const prod_class *clssp,
        const timestampt *maxlatencyp)
{
        if(clssp == NULL || tvIsNone(pq->cursor))
                return 0;

        if(clss_eq(clssp, PQ_CLASS_ALL))
                return 1;

        if(mt == TV_LT)
        {
                /* reversed scan */
                if(tvCmp(pq->cursor, clssp->to, <))
                        return 0;
        }
        else
        {
                timestampt to = timestamp_add(&clssp->to, maxlatencyp);
                if(tvCmp(pq->cursor, to, >))
                        return 0;
        }
        /* else, it's in the time range */
        return 1;
}


/*
 * Like pq_sequence(), but the ifmatch action is to remove the
 * product from inventory.
 */
/* TODO: add class filter */
/*ARGSUSED*/
int
pq_seqdel(pqueue *pq, pq_match mt,
        const prod_class *clss, int wait,
        size_t *extentp, timestampt *timestampp) 
{
        int status = ENOERR;
        tqelem *tqep;
        region *rp = NULL;
        off_t  offset = OFF_NONE;
        size_t extent = 0;
        void *vp = NULL;
        struct infobuf
        {
                prod_info b_i;
                char b_origin[HOSTNAMESIZE + 1];
                char b_ident[KEYSIZE + 1];
        } buf; /* static ??? */
        prod_info *info ;
        XDR xdrs[1] ;
        int const rflags = wait ? RGN_WRITE : (RGN_WRITE | RGN_NOWAIT);
        size_t rlix;

        if(pq == NULL)
                return EINVAL;  

        /* all this to avoid malloc in the xdr calls */
        info = &buf.b_i;
        info->origin = &buf.b_origin[0];
        info->ident = &buf.b_ident[0];
        
        /* if necessary, initialize cursor */
        /* We don't need to worry about disambiguating products with
           identical timestamps using offsets here (as in
           pq_sequence), because after a product is deleted, it won't
           be found again */
        if(tvIsNone(pq->cursor))
        {
                if(mt == TV_LT) {
                        pq->cursor = TS_ENDT;
                        pq->cursor_offset = OFF_NONE;
                }
                else {
                        pq->cursor = TS_ZERO;
                        pq->cursor_offset = 0;
                }
        }

        /* write lock pq->ctl.  */
        status = ctl_get(pq, RGN_WRITE);
        if(status != ENOERR)
                return status;

        /* find the specified que element */
        tqep = tqe_find(pq->tqp, &pq->cursor, mt);
        if(tqep == NULL)
        {
                status = PQUEUE_END;
                goto unwind_ctl;
        }
        /* update cursor below, after we get the data */


        /* get the actual data region */
        rlix = rl_find(pq->rlp, tqep->offset);
        assert(rlix != RL_NONE);
        rp = pq->rlp->rp + rlix;
        assert(rp->offset == tqep->offset);
        assert(Extent(rp) <= (pq->ixo - pq->datao));
        status = rgn_get(pq, rp->offset, Extent(rp), rflags, &vp);
        if(status != ENOERR)
        {
                goto unwind_ctl;
        }
        assert(vp != NULL);

        /* update cursor */
        /* pq->cursor = tqep->tv; */
        pq_cset(pq, &tqep->tv);
        pq_coffset(pq, OFF_NONE);

        offset = rp->offset;
        extent = Extent(rp);
        
/*** */
        /*
         * Decode it
         */
        xdrmem_create(xdrs, vp, (u_int)extent, XDR_DECODE) ;

        if(!xdr_prod_info(xdrs, info))
        {
                uerror("pq_seqdel: xdr_prod_info() failed\n") ;
                status = EIO;
                goto unwind_rgn;
        }
        assert(info->sz <= xdrs->x_handy);
                
        /* return timestamp value even if we don't delete it */
        if(timestampp)
                *timestampp = info->arrival;
/*** */


        assert(clss != NULL);
        if(clss != PQ_CLASS_ALL && !prodInClass(clss, info))
        {
                /* skip this one */
                if(ulogIsDebug())
                        udebug("skip %s", s_prod_info(NULL, 0, info, 1));
                goto unwind_rgn;
        }

        /*
         * else, Doit
         */
        if(ulogIsVerbose())
                uinfo(" del %s", s_prod_info(NULL, 0, info, 1));

        /* return extent value */
        if(extentp)
                *extentp = extent;

        tq_delete(pq->tqp, tqep);
        {
                const int found = sx_find_delete(pq->sxp, info->signature);
                if(found == 0)
                {
                        char ts[20];
                        (void) sprint_timestampt(ts, sizeof(ts), &tqep->tv);
                        uerror("Que corrupt: pq_seqdel: %s %s at %ld",
                                ts,
                                found ? "invalid region" : "no signature",
                                tqep->offset);
                }
        }
        rl_free(pq->rlp, rlix);

        /*FALLTHROUGH*/
unwind_rgn:
        /* release the data segment */
        (void) rgn_rel(pq, offset, 0);

        /*FALLTHROUGH*/
unwind_ctl:
        (void) ctl_rel(pq, 0);
        return status;
}


/*
 * Used only by pq_last() below.
 */
/*ARGSUSED*/
static int
didmatch(const prod_info *infop, const void *datap,
                void *xprod, size_t size,  void *vp)
{
        timestampt *tsp = (timestampt *)vp;
        if(tsp != NULL)
                *tsp = infop->arrival;

        if(ulogIsDebug())
        {
                udebug("lastmatch: %s", s_prod_info(NULL, 0, infop, 1));
        }

        return PQUEUE_END; /* done with scan on the first hit */
}


/*
 * Returns the creation-time of the data-product in the product-queue whose
 * insertion-time is closest-to but less-than the "to" time of a class
 * specification.  Sets the cursor of the product-queue to the insertion-
 * time of the data-product, if found.
 *
 * Arguments:
 *      pq      Pointer to product-queue open for reading.
 *      clssp   Pointer to selection-criteria.
 *      tsp     Pointer to timestamp.  Set to creation-time of first, matching
 *              data-product; otherwise, unmodified.
 * Returns:
 *      0       Success (maybe).  *tsp is modified if and only if a matching 
 *              data-product was found.
 *      else    Failure.  <errno.h> error-code.
 */
int
pq_last(pqueue *pq,
        const prod_class *clssp,
        timestampt *tsp) /* modified upon return */
{
        int status = ENOERR;

        pq_cset(pq, &clssp->to); /* Start at the end and work backwards */
        while((status = pq_sequence(pq, TV_LT, clssp, didmatch, tsp))
                        == ENOERR) 
        {
           if((tsp != NULL)&&(pq->cursor.tv_sec < tsp->tv_sec))
           {
                udebug("cursor reset: stop searching\0");
                return status;
           }
        }

        if(status != PQUEUE_END)
        {
                uerror("pq_last: seq:%s (errno = %d)",
                        strerror(status), status);
        }
        else
        {
                status = ENOERR;
        }

        if(tvEqual(pq->cursor, TS_ENDT))
        {
                /* clssp->to is TS_ENDT and queue is empty */
                pq->cursor = TS_NONE; /* clear cursor */
                pq->cursor_offset = OFF_NONE;
        }

        return status;
}


/*
 * Modifies a data-product class-specification according to the most recent
 * data-product in the product-queue that matches the specification.
 *
 * The product-queue cursor is unconditionally cleared.
 *
 * Arguments:
 *      pq              Pointer to the product-queue.
 *      clssp           Pointer to the data-product class-specification.
 *                      Modified on and only on success.
 * Returns:
 *      0               Success.  "clssp" is modified.
 *      PQUEUE_END      There's no matching data-product in the product-queue.
 *      else            <errno.h> error-code.
 */
int
pq_clss_setfrom(pqueue *pq,
         prod_class_t *clssp)     /* modified upon return */
{
        timestampt ts = clssp->from;
        int status = pq_last(pq, clssp, &ts);
        if(status == ENOERR)
        {
                if(tvEqual(ts, clssp->from))
                        status = PQUEUE_END;
                else
                        clssp->from = ts;
        }
        pq->cursor = TS_NONE; /* clear cursor */
        pq->cursor_offset = OFF_NONE;
        return status;
}

/*** ? TODO, move this to another file. Doesn't use internal knowlege **/

#include "timestamp.h"

static int sigalrm_received = 0;

static void
hndlr_noop(int sig)
{
#ifndef NDEBUG
        switch(sig) {
        case SIGALRM :
                udebug("SIGALRM") ;
                sigalrm_received = 1;
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
 * Suspend yourself (sleep) until
 * one of the following events occurs:
 *   You recieve a signal that you handle.
 *   You recieve SIGCONT (sent from an insert proc indicating
 *      data is available).
 *   "maxsleep" seconds elapse.
 *   If "maxsleep" is zero, you could sleep forever. 
 * Returns the requested amount of suspension-time minus the amount of time 
 * actually suspended.
 */
unsigned
pq_suspend(unsigned int maxsleep)
{
        struct sigaction sigact, csavact, asavact;
        sigset_t mask, savmask;
        unsigned unused;
        time_t start;

        /* block CONT and ALRM while we set up */
        sigemptyset(&mask);
        sigaddset(&mask, SIGCONT);
        if(maxsleep)
                sigaddset(&mask, SIGALRM);
        (void) sigprocmask(SIG_BLOCK, &mask, &savmask);

        /*
         * Set up handlers for CONT and ALRM, stashing old
         */
        sigemptyset(&sigact.sa_mask);
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
        sigdelset(&mask, SIGCONT);
        if(maxsleep)
                sigdelset(&mask, SIGALRM);

        /* Nighty night... */
        (void)time(&start);
        sigalrm_received = 0;
        (void) sigsuspend(&mask);

        /* Now we are back, restore state */
        if(maxsleep)
        {
                (void)alarm(0);
                (void) sigaction(SIGALRM, &asavact, NULL );
        }
        (void) sigaction(SIGCONT, &csavact, NULL );
        
        (void) sigprocmask(SIG_SETMASK, &savmask, NULL);

        if (sigalrm_received) {
            unused = 0;
        }
        else {
            unused = (unsigned)(time(NULL) - start);
        }

        return unused;
}


/*
 * Returns an appropriate error-message given a product-queue and error-code.
 *
 * Arguments:
 *      pq      Pointer to the product-queue.
 *      error   The error-code.
 * Returns:
 *      Pointer to appropriate NUL-terminated error-message.
 */
/*ARGSUSED*/
const char*
pq_strerror(
    const pqueue* const pq,
    const int           error)
{
    const char* msg;

    if (0 == error) {
        msg = "Success";
    }
    else if (0 < error) {
        msg = strerror(error);
    }
    else {
        switch (error) {
            case PQ_END:
                msg = "End of product-queue reached";
                break;
            case PQ_NOTFOUND:
                msg = "Desired data-product not found";
                break;
            case PQ_CORRUPT:
                msg = "Product-queue is corrupt";
                break;
            default:
                msg = "Unknown error-code";
        }
    }

    return msg;
}
