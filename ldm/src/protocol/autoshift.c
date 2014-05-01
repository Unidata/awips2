/*
 *   Copyright 2005, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: autoshift.c,v 1.1.2.14 2007/02/12 20:38:54 steve Exp $ */

/*LINTLIBRARY*/

#include <ldmconfig.h>

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stddef.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <time.h>

#include "ldm.h"
#include "error.h"
#include "timestamp.h"
#include "ulog.h"
#include "globals.h"

/*
 * The following parameters condition the behavior of this module.
 */
#define MIN_PERIOD              interval/* data-gathering threshold */
#define CONFIDENCE_LEVEL        0.999   /* degree of confidence in decision */
#define MAX_ENTRY_COUNT         10000   /* limit on database size */
#define MIN_BUFFER_COUNT        2       /* data-gathering threshold */

#undef MIN
#define MIN(a, b)       ((a) <= (b) ? (a) : (b))
#undef MAX
#define MAX(a, b)       ((a) >= (b) ? (a) : (b))
#undef ABS
#define ABS(x)          ((x) >= 0 ? (x) : -(x))


/*
 * Returns the current time.
 *
 * Returns:
 *      Pointer to static buffer containing the current time.
 */
static const timestampt*
getTime(void)
{
    static timestampt   now;
    int                 error = set_timestamp(&now);

    assert(error == 0);

    return &now;
}


/******************************************************************************
 * Begin Database Module                                                      *
 ******************************************************************************/


typedef struct entry {
    struct entry*       next;           /* pointer to subsequent entry */
    timestampt          when;           /* when the entry was created */
    size_t              size;           /* size of the data-product */
    int                 wasAccepted;    /* if the data-product was inserted */
} Entry;

static unsigned         entryCount = 0; /* number of database entries */
static Entry*           head = NULL;    /* head of database linked-list */
static Entry*           tail = NULL;    /* tail of database linked-list */


/*
 * Returns the number of entries in the database.
 *
 * Returns:
 *      The number of entries in the database.
 */
#define db_getCount()   ((const unsigned)entryCount)


/*
 * Returns the oldest entry in the database.
 *
 * Returns:
 *      NULL    The database is empty.
 *      else    A pointer to the oldest entry in the database.
 */
#define db_getOldest()  ((const Entry*)head)


/*
 * Adds an entry to the database.
 *
 * Arguments:
 *      size            Size of the data-product in bytes.
 *      success         If the data-product was accepted.
 * Returns:
 *      NULL            Failure:
 *                              ENOMEM  Out of memory.
 *      else            Pointer to the new entry.
 */
static Entry*
db_add(
    const size_t                size,
    const int                   success)
{
    Entry*      entry = (Entry*)malloc(sizeof(Entry));

    if (NULL != entry) {
        entry->next = NULL;
        entry->size = size;
        entry->wasAccepted = success;

        entry->when = *getTime();

        if (head == NULL) {
            assert(tail == NULL);

            head = entry;
        }

        if (tail != NULL)
            tail->next = entry;

        tail = entry;
        entryCount++;
    }

    return entry;
}


/*
 * Removes the oldest entry from the database.
 */
static void
db_removeOldest(void)
{
    if (head != NULL) {
        Entry*  next = head->next;

        free(head);

        head = next;

        if (head == NULL)
            tail = NULL;

        entryCount--;
    }
}


/*
 * Clears the database.
 */
static void
db_clear(void)
{
    while (head != NULL) {
        Entry*  next = head->next;

        free(head);

        head = next;
    }

    tail = NULL;
    entryCount = 0;
}


/******************************************************************************
 * Begin Statistics Object Module                                             *
 ******************************************************************************/


typedef struct {
    off_t               amount;         /* amount of data in bytes */
    unsigned            count;          /* number of data-products */
}       Stats;


/*
 * Adds an entry to a statistics object.
 *
 * Arguments:
 *      stats           Pointer to the statistics object.
 *      entry           Pointer to the entry.
 */
void
stats_addEntry(
    Stats* const        stats,
    const Entry* const  entry)
{
    assert(stats);
    assert(entry);

    stats->amount += entry->size;
    stats->count++;
}


/*
 * Subtracts an entry from a statistics object.
 *
 * Arguments:
 *      stats           Pointer to the statistics object.
 *      entry           Pointer to the entry.
 */
void
stats_subtractEntry(
    Stats* const        stats,
    const Entry* const  entry)
{
    assert(stats);
    assert(entry);

    stats->amount -= entry->size;
    assert(stats->amount >= 0);

    assert(stats->count > 0);
    stats->count--;
}


/*
 * Resets a statistics object.
 *
 * Arguments:
 *      stats           Pointer to the statistics object.
 */
static void
stats_reset(
    Stats* const        stats)
{
    stats->count = 0;
    stats->amount = 0;
}


/*
 * Returns the count of a statistics object.
 *
 * Arguments:
 *      stats           Pointer to the statistics object.
 * Returns:
 *      The count of the statistics object.
 */
#define stats_getCount(stats)   ((const unsigned)(stats)->count)


/*
 * Returns the amount of a statistics object.
 *
 * Arguments:
 *      stats           Pointer to the statistics object.
 * Returns:
 *      The amount of the statistics object.
 */
#define stats_getAmount(stats)  ((const off_t)(stats)->amount)


/******************************************************************************
 * Begin Summary Statistics Module                                            *
 ******************************************************************************/


static Stats            accepted;       /* inserted data-product statistics */
static Stats            rejected;       /* rejected data-product statistics */
static Stats            prevAccepted;   /* previous "accepted" value */
static Stats            prevRejected;   /* previous "rejected value */
static unsigned         ldmCount = 1;   /* number of LDM-s exchanging data */
static timestampt       start;
static int              haveEnough = 0;
static int              prevHaveEnough = 0;
static int              needComputation = 1;
static int              prevNeedComputation = 1;
static int              isAlternate = 0;/* LDM using COMINGSOON/BLKDATA? */


/*
 * Saves the current state of the summary statistics.
 */
static void
sumStats_save(void)
{
    prevAccepted = accepted;
    prevRejected = rejected;
    prevHaveEnough = haveEnough;
    prevNeedComputation = needComputation;
}


/*
 * Restores the state of the summary statistics.
 */
static void
sumStats_restore(void)
{
    accepted = prevAccepted;
    rejected = prevRejected;
    haveEnough = prevHaveEnough;
    needComputation = prevNeedComputation;
}


/*
 * Resets the state of the summary statistics.
 */
static void
sumStats_reset(void)
{
    stats_reset(&accepted);
    stats_reset(&rejected);

    haveEnough = 0;
    needComputation = 1;
    start = *getTime();

    sumStats_save();
}


/*
 * Adds an entry to the summary statistics.
 * Arguments:
 *      entry           Pointer to the entry.
 */
static void
sumStats_addEntry(
    const Entry* const entry)
{
    assert(NULL != entry);

    stats_addEntry(
        entry->wasAccepted
            ? &accepted
            : &rejected,
        entry);

    needComputation = 1;
}


/*
 * Subtracts an entry from the summary statistics.
 * Arguments:
 *      entry           Pointer to the entry.
 */
static void
sumStats_subtractEntry(
    const Entry* const  entry)
{
    assert(NULL != entry);

    stats_subtractEntry(
        entry->wasAccepted
            ? &accepted
            : &rejected,
        entry);

    needComputation = 1;
}


/*
 * Indicates if a decision can be made based on the summary statistics.
 *
 * Returns:
 *      0               Not enough data.
 *      1               Enough data.
 */
static int
sumStats_haveEnough(void)
{
    /*
     * Previous versions of this function used the metaphore of tossing a coin
     * to determine if sufficient data existed.  This resulted, however, in
     * unacceptable rates of mode-switching.  This version is mathematically
     * simpler yet appears to have acceptable behavior.
     */
    int         enough;

    if (!needComputation) {
        enough = haveEnough;
    }
    else {
        unsigned        acceptedCount = stats_getCount(&accepted);
        unsigned        rejectedCount =
            (unsigned)(stats_getCount(&rejected) / (ldmCount - 1));

        if (2 >= acceptedCount + rejectedCount) {
            enough = 0;                 /* not enough data */
        }
        else {
            double      period =
                d_diff_timestamp(getTime(), &db_getOldest()->when);

            enough = 2*interval < period;

            udebug(
                "sumStats_haveEnough(): A/R counts=%u/%u, period=%g, enough=%d",
                acceptedCount, rejectedCount, period, enough);
        }

        haveEnough = enough;
        needComputation = 0;
    }                                   /* needs recomputation */

    return enough;
}


/*
 * Indicates whether or not the LDM should switch the data-product 
 * exchange-mode.
 *
 * Returns:
 *      0       The LDM should not switch.
 *      1       The LDM should switch.
 */
static int
sumStats_shouldSwitch(void)
{
    int should;

    if (!sumStats_haveEnough()) {
        should = 0;
    }
    else {
        unsigned        acceptedCount =
            stats_getCount(&accepted) * (ldmCount - 1);
        unsigned        rejectedCount = stats_getCount(&rejected);

        should =
            isAlternate
                ? (acceptedCount >= rejectedCount)
                : (acceptedCount < rejectedCount);

        if (should)
            udebug("sumStats_shouldSwitch(): A/R counts = %u/%u, ldmCount = %u",
                stats_getCount(&accepted), stats_getCount(&rejected), ldmCount);
    }

    return should;
}


/*
 * Returns the number of data-products that were inserted into the 
 * product-queue.
 *
 * Returns:
 *      The number of data-products that were inserted into the product-queue.
 */
#define sumStats_getAcceptedCount()     stats_getCount(&accepted)


/*
 * Returns the number of data-products that were not inserted into the 
 * product-queue.
 *
 * Returns:
 *      The number of data-products that were not inserted into the 
 *      product-queue.
 */
#define sumStats_getRejectedCount()     stats_getCount(&rejected)


/******************************************************************************
 * Begin module just above Summary Statistics module                          *
 ******************************************************************************/


static int              shouldSwitch = 0;
static int              isSender = 0;   /* is upstream LDM? */


/*
 * Accumulates statistics on data-product exchange.
 *
 * Arguments:
 *      success         If the data-product was inserted.
 *      size            Size of the data-product in bytes.
 * Returns:
 *      0               Success.
 *      ENOMEM          Out of memory.
 */
static int
accumulate(
    const int           success,
    const size_t        size)
{
    int                 error;
    const Entry* const  entry = db_add(size, success);

    if (entry == NULL) {
        error = ENOMEM;
    }
    else {
        sumStats_addEntry(entry);

        assert(db_getCount() == sumStats_getAcceptedCount() +
            sumStats_getRejectedCount());

        /*
         * Delete excess entries.
         */
        for (;;) {
            if (db_getCount() > MAX_ENTRY_COUNT) {
                sumStats_subtractEntry(db_getOldest());
            }
            else if (!sumStats_haveEnough()) {
                break;
            }
            else {
                sumStats_save();
                sumStats_subtractEntry(db_getOldest());

                if (!sumStats_haveEnough()) {
                    sumStats_restore();
                    break;
                }
            }

            db_removeOldest();
        }

        shouldSwitch = sumStats_shouldSwitch();
        error = 0;                  /* success */
    }                               /* new entry created */

    return error;
}


/*
 * Resets this module.
 */
static void
reset(void)
{
    db_clear();
    sumStats_reset();

    shouldSwitch = 0;
}


/******************************************************************************
 * Begin module just below public interface                                   *
 ******************************************************************************/


/*
 * Processes the status of an exchanged data-product.
 *
 * Arguments:
 *      success         Whether or not the data-product was accepted.
 *                      True or false.
 *      size            Size of the data-product in bytes.
 *      altMode         Alternate exchange-mode?
 * Returns:
 *      0               Success.
 *      ENOMEM          Out of memory.
 *      ENOSYS          Module not yet initialized.
 */
static int
process(
    const int           success,
    const size_t        size,
    const int           altMode)
{
    int errCode;

    if (isSender) {
        errCode = 0;
    }
    else if (ldmCount == 0) {
        errCode = ENOSYS;
    }
    else {
        if (isAlternate != altMode) {
            isAlternate = altMode;

            reset();
        }

        if (ldmCount <= 1) {
            errCode = 0;
        }
        else {
            errCode = accumulate(success, size);
        }
    }

    return errCode;
}


/******************************************************************************
 * Begin public interface                                                     *
 ******************************************************************************/


/*
 * Sets the number of LDM-s exchanging data.  If the number doesn't equal
 * the previous number, then this module is reset.
 *
 * Arguments:
 *      count   The number of LDM-s exchanging data.
 * Returns:
 *      0       Success.  The number will be used to determine when to switch.
 *      EINVAL  "count" is zero.  The previous number will be unchanged.
 */
int
as_setLdmCount(
    unsigned    count)
{
    int         err;

    if (count == 0) {
        err = EINVAL;
    }
    else {
        if (count != ldmCount) {
            ldmCount = count;
            reset();
        }

        err = 0;
    }

    return err;
}


/*
 * Initializes this module.
 *
 * Arguments:
 *      isUpstream      Whether or not the process is an upstream (i.e., 
 *                      sending) LDM.  True or false.
 *      isPrimary       Whether or not the transmission-mode is primary or
 *                      alternate (i.e., uses HEREIS or COMINGSOON/BLKDATA
 *                      messages).
 *      socket          The socket on which data-products are exchanged.
 * Returns:
 *      NULL            Success.
 *      else            Failure.  No failure modes are currently defined.
 */
ErrorObj*
as_init(
    const int   isUpstream,
    const int   isPrimary,
    const int   socket)
{
    reset();

    isSender = isUpstream;
    isAlternate = !isPrimary;

    return NULL;
}


/*
 * Processes the status of a data-product exchanged via the HEREIS protocol.
 *
 * Arguments:
 *      success         Whether or not the data-product was accepted.
 *                      True or false.
 *      size            Size of the data-product in bytes.
 * Returns:
 *      0               Success.
 *      ENOMEM          Out of memory.
 *      ENOSYS          Module not yet initialized.
 */
int
as_hereis(
    const int           success,
    const size_t        size)
{
    return process(success, size, 0);
}


/*
 * Processes the status of a data-product exchanged via the COMINGSOON/BLKDATA
 * protocols.
 *
 * Arguments:
 *      success         Whether or not the data-product was accepted.
 *                      True or false.
 *      size            Size of the data-product in bytes.
 * Returns:
 *      0               Success.
 *      ENOMEM          Out of memory.
 *      ENOSYS          Module not yet initialized.
 */
int
as_comingsoon(
    const int           success,
    const size_t        size)
{
    return process(success, size, 1);
}


/*
 * Indicates whether or not to switch between primary and alternate
 * data-product exchange-modes.
 *
 * Returns:
 *      0       Don't switch.
 *      1       Do switch
 */
int
as_shouldSwitch(void)
{
    return
        isSender
            ? 0
            : ldmCount <= 1
                ? isAlternate
                : shouldSwitch;
}
