/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: timestamp.c,v 1.15.18.2 2008/03/17 16:16:40 steve Exp $ */

#include <ldmconfig.h>
#include <errno.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "ulog.h"
#include "timestamp.h"
#ifndef NDEBUG
#define pIf(a,b) (!(a) || (b))  /* a implies b */
#endif

const struct timeval TS_NONE = {-1, -1};
const struct timeval TS_ZERO = {0, 0};
const struct timeval TS_ENDT = {0x7fffffff, 999999};

int
set_timestamp(timestampt *tsp)
{
        int status = 0;
        if(gettimeofday(tsp, NULL) < 0)
        {
                /* should never happen ... */
                status = errno;
                uerror("gettimeofday: %s", strerror(status));
        }
        return status;
}

void
swap_timestamp(timestampt *fr, timestampt *to)
{
        timestampt tmp;
        tmp = *fr;
        *fr = *to;
        *to = tmp;
}


bool_t
xdr_timestampt(XDR *xdrs, timestampt *tvp)
{
        assert(pIf(xdrs->x_op == XDR_ENCODE,
                        (tvp->tv_sec  >= TS_ZERO.tv_sec
                        && tvp->tv_usec >= TS_ZERO.tv_usec
                        && tvp->tv_sec  <= TS_ENDT.tv_sec
                        && tvp->tv_usec <= TS_ENDT.tv_usec)));

        /*
         * TV_INT
         * On DEC alpha, tvp->tv_sec is an int.
         * On IRIX, tvp->tv_sec is an time_t, which is 32 bits,
         * which may be  a 'long' or an 'int'.
         * The use of intermediate variables is an attempt
         * to cover all bases.
         */
        /* TODO: use the preprocessor to determine this */
        
        if(xdrs->x_op == XDR_ENCODE)
        {
                long tv_sec = (long)tvp->tv_sec; /* const */
                if (!xdr_long(xdrs, &tv_sec)) {
                         return (FALSE);
                }
        }
        else
        {
                long tv_sec = TS_NONE.tv_sec;
                if (!xdr_long(xdrs, &tv_sec)) {
                        return (FALSE);
                }
                tvp->tv_sec = tv_sec; /* ignore any warning */
                
        }

        if(xdrs->x_op == XDR_ENCODE)
        {
                long tv_usec = (long)tvp->tv_usec; /* const */
                if (!xdr_long(xdrs, &tv_usec)) {
                         return (FALSE);
                }
        }
        else
        {
                long tv_usec = TS_NONE.tv_usec;
                if (!xdr_long(xdrs, &tv_usec)) {
                         return (FALSE);
                }
                tvp->tv_usec = tv_usec; /* ignore any warning */
        }

        return (TRUE);
}


timestampt
timestamp_add(const timestampt *const left, const timestampt *const rght)
{
        timestampt tv;
        tv = TS_ZERO;

        if(left == NULL || rght == NULL)
                return tv;

        tv.tv_sec = left->tv_sec + rght->tv_sec;
        tv.tv_usec = left->tv_usec + rght->tv_usec;
        if(tv.tv_usec >= 1000000)
        {
                tv.tv_sec += 1;
                tv.tv_usec -= 1000000;
        }
        return tv;
}


/*
 * Increment a timestamp
 */
void
timestamp_incr(timestampt *ts)
{
        /* assert(ts != NULL); */
        if(ts->tv_usec == 999999)
        {
#if 0
                if(ts->tv_sec == TS_ENDT.tv_sec)
                        return; /* clamp at TS_ENDT */
#endif
                ts->tv_usec = 0;
                ts->tv_sec++;
        }
        else
        {
                ts->tv_usec++;
        }
}


/*
 * decrement a timestamp
 */
void
timestamp_decr(timestampt *ts)
{
        /* assert(ts != NULL); */
        if(ts->tv_usec == 0)
        {
#if 0
                if(ts->tv_sec == TS_ZERO.tv_sec)
                        return; /* clamp at TS_ZERO */
#endif
                ts->tv_usec = 999999;
                ts->tv_sec--;
        }
        else
        {
                ts->tv_usec--;
        }
}


/*
 * take the difference between two timestamps
 *
 * N.B. Meaningful only if "afta" is later than "b4",
 * negative differences map to TS_ZERO
 */
timestampt
diff_timestamp(const timestampt *const afta, const timestampt *const b4)
{
        timestampt diff;
        diff = TS_ZERO;

        diff.tv_sec = afta->tv_sec -  b4->tv_sec;
        diff.tv_usec = afta->tv_usec -  b4->tv_usec;

        if(diff.tv_usec < 0)
        {
                if(diff.tv_sec > 0)
                {
                        /* borrow */
                        diff.tv_sec--;
                        diff.tv_usec += 1000000;
                }
                else
                {
                        /* truncate to zero */
                        diff.tv_sec = diff.tv_usec = 0;
                }
        }

        return diff;
}


/*
 * Take the difference between two timevals,
 * return as a double.
 */
double
d_diff_timestamp(const timestampt *const afta,
         const timestampt *const b4)
{
    return (afta->tv_sec - b4->tv_sec) + .000001*(afta->tv_usec - b4->tv_usec);
}


#define STRFTIME_FORMAT "%Y%m%dT%H%M%S."
#define USEC_FORMAT     "%06ld"


/*
 * Formats a timestamp.
 *
 * Arguments:
 *      timestamp       Pointer to the timestamp to be formatted.
 * Returns:
 *      Pointer to a static buffer containing the formatted timestamp.
 */
char*
tsFormat(
    const timestampt* const     timestamp)
{
    static char         string[80];
    const struct tm*    tm = gmtime(&timestamp->tv_sec);

    strftime(string, sizeof(string), STRFTIME_FORMAT, tm);
    sprintf(string + strlen(string), USEC_FORMAT, (long)timestamp->tv_usec);

    return string;
}


/*
 * Parses a timestamp.
 *
 * Arguments:
 *      timestamp       Pointer to the timestamp to be parsed.
 * Returns:
 *      -1              Error.
 *      else            Number of bytes parsed.
 */
int
tsParse(
    const char* const   string,
    timestampt* const   timestamp)
{
    int                 nbytes = -1;    /* failure */
    int                 year;
    int                 month;
    int                 day;
    int                 hour;
    int                 minute;
    int                 second;
    long                microseconds;
    const char*         format = "%04d%02d%02dT%02d%02d%02d.%06ld";

    if (sscanf(string, format, &year, &month, &day, &hour, &minute, &second,
            &microseconds)
            != 7) {
        uerror("Couldn't decode timestamp \"%s\" with format \"%s\"", string,
            format);
    }
    else if (month < 1 || month > 12 ||
            day < 1 || day > 31 ||
            hour < 0 || hour > 23 ||
            minute < 0 || minute > 59 ||
            second < 0 || second > 61 ||
            microseconds < 0) {
        uerror("Invalid timestamp \"%s\"", string);
    }
    else {
        struct tm           tm;

        tm.tm_isdst = 0;
        tm.tm_year = year - 1900;
        tm.tm_mon = month - 1;
        tm.tm_mday = day;
        tm.tm_hour = hour;
        tm.tm_min = minute;
        tm.tm_sec = second;
        timestamp->tv_sec = mktime(&tm);
        timestamp->tv_usec = microseconds;
        nbytes = 22;
    }

    return nbytes;
}
