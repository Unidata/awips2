/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: timestamp.h,v 1.12.18.1 2007/03/02 19:50:13 steve Exp $ */

#ifndef _TIMESTAMP_H
#define _TIMESTAMP_H

#include <sys/time.h>
#include <rpc/rpc.h>

typedef struct timeval timestampt;

extern const struct timeval TS_NONE; /* an invalid time */
extern const struct timeval TS_ZERO; /* the beginning of time */
extern const struct timeval TS_ENDT; /* the end of time */

#ifndef TOFFSET_NONE
#define TOFFSET_NONE (-2147483647)
#endif

#define tvEqual(tvl, tvr) \
	((tvl).tv_sec == (tvr).tv_sec && (tvl).tv_usec == (tvr).tv_usec)

#define timerEqual(tvpl, tvpr) \
	((tvpl)->tv_sec == (tvpr)->tv_sec && (tvpl)->tv_usec == (tvpr)->tv_usec)

#define tvIsNone(tv) (tvEqual(tv, TS_NONE))

#define	tvCmp(tv, uv, cmp)	\
	((tv).tv_sec cmp (uv).tv_sec || (\
	 (tv).tv_sec == (uv).tv_sec && (tv).tv_usec cmp (uv).tv_usec))

#ifdef __cplusplus
extern "C" {
#endif

extern int
set_timestamp(timestampt *tsp);

extern void
swap_timestamp(timestampt *fr, timestampt *to);

extern bool_t
xdr_timestampt(XDR *, timestampt*);

extern timestampt
timestamp_add(const timestampt *const left, const timestampt *const rght);

/*
 * Increment a timestamp
 */
extern void
timestamp_incr(timestampt *ts);

/*
 * Decrement a timestamp
 */
extern void
timestamp_decr(timestampt *ts);

/*
 * take the difference between two timestamps
 *
 * N.B. Meaningful only if "afta" is later than "b4",
 * negative differences map to TS_ZERO
 */
extern timestampt
diff_timestamp(const timestampt *const afta, const timestampt *const b4);

/*
 * Take the difference between two timevals,
 * return as a double.
 */
extern double
d_diff_timestamp(const timestampt *const afta,
	 const timestampt *const b4);

/*
 * Formats a timestamp.
 *
 * Arguments:
 *	timestamp	Pointer to the timestamp to be formatted.
 * Returns:
 *	Pointer to a static buffer containing the formatted timestamp.
 */
char*
tsFormat(
    const timestampt* const	timestamp);

/*
 * Parses a timestamp.
 *
 * Arguments:
 *	timestamp	Pointer to the timestamp to be formatted.
 * Returns:
 *	-1		Error.  log_errno() called.
 *	else		Number of bytes parsed.
 */
int
tsParse(
    const char* const	string,
    timestampt* const	timestamp);

#ifdef __cplusplus
}
#endif

#endif /*!_TIMESTAMP_H */
