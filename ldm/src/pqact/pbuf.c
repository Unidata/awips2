/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pbuf.c,v 1.21.16.2.2.8 2009/05/21 20:30:44 steve Exp $ */

/* #define _POSIX_SOURCE */

#include <ldmconfig.h>
#include "pbuf.h"
#include <unistd.h>
#include <string.h>
#include <time.h>
#include "ulog.h"
#include "ldmalloc.h"
#include "alrm.h"
#include "error.h"
#include "fdnb.h"

#ifdef NO_MEMMOVE
/* define memmove in terms of bcopy - recividist */
#define memmove(d1, d2, n) bcopy((d2), (d1), (n))
#endif /* NO_MEMMOVE */

#define INSTRUMENT

#ifdef INSTRUMENT
#include <sys/time.h>

/*
 * take the difference between two timevals
 *
 * N.B. Meaningful only if "afta" is later than "b4",
 * negative differences map to 0.0 
 */
static struct timeval 
diff_timeval(struct timeval *afta, struct timeval *b4)
{
        struct timeval diff;

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
                        diff.tv_sec = 0;
                        diff.tv_usec = 0;
                }
        }

        return diff;
}
#endif

void
free_pbuf(pbuf *buf)
{
        if(buf == NULL)
                return;
        free(buf->base);
        free(buf);
}

pbuf *
new_pbuf(
        int pfd,
        size_t bufsize)
{
        long pipe_buf = 512; /* _POSIX_PIPE_BUF */
        pbuf *buf = NULL;
        buf = Alloc(1, pbuf);
        if(buf == NULL)
                return NULL;
#ifdef _PC_PIPE_BUF
        pipe_buf = fpathconf(pfd, _PC_PIPE_BUF);
        if(pipe_buf == -1L)
        {
                serror("fpathconf %d, _PC_PIPE_BUF", pfd);
                goto err;
        }
#endif
        set_fd_nonblock(pfd);
        buf->pfd = pfd;
        if(bufsize < (size_t)pipe_buf)
                bufsize = (size_t)pipe_buf;
        buf->base = Alloc(bufsize, char);
        if(buf->base == NULL)
                goto err;
        buf->ptr = buf->base;
        buf->upperbound = buf->base + bufsize;
        return buf;
err:
        free(buf);
        return NULL;
}

/* returns 0 or errno on error */
int
pbuf_flush(
    pbuf*               buf,
    int                 block,          /* bool_t */
    unsigned int        timeo,          /* N.B. Not a struct timeval */
    const char* const   id)
{
    size_t              len = (size_t)(buf->ptr - buf->base);
    int                 changed = 0;
    int                 nwrote = 0;
    int                 status = ENOERR;        /* success */
    int                 tmpErrno;
    time_t              start;
    time_t              stop;
    time_t              duration;

    udebug("        pbuf_flush fd %d %6d %s",
        buf->pfd, len, block ? "block" : "" );

    if(len == 0)
        return 0; /* nothing to do */
    /* else */

    (void)time(&start);

    if(block)
        changed = clr_fd_nonblock(buf->pfd);

    if(block && timeo != 0)             /* (timeo == 0) => don't set alarm */
        SET_ALARM(timeo, flush_timeo);

    nwrote = (int) write(buf->pfd, buf->base, len);
    tmpErrno = errno;                   /* CLR_ALRM() can change "errno" */

    if(block && timeo != 0)
        CLR_ALRM();

    if(nwrote == -1) {
        if((tmpErrno == EAGAIN) && (!block)) {
            udebug("         pbuf_flush: EAGAIN on %d bytes", len);

            nwrote = 0;
        }
        else {
            status = tmpErrno;

            serror("pbuf_flush (%d) write", buf->pfd);
        }
    }
    else if(nwrote == len) {
        /* wrote the whole buffer */
        udebug("         pbuf_flush: wrote  %d bytes", nwrote);

        buf->ptr = buf->base;
        len = 0;
    }
    else if(nwrote > 0) {
        /* partial write, just shift the amount wrote */
        udebug("         pbuf_flush: partial write %d of %d bytes",
            nwrote, len);

        len -= nwrote;

        /* could be an overlapping copy */
        memmove(buf->base, buf->base + nwrote, len);

        buf->ptr = buf->base +len;
    }

    if(changed)
        set_fd_nonblock(buf->pfd);

    duration = time(NULL) - start;

    if(duration > 5)
        uwarn(id == NULL
                ?  "write(%d,,%d) to decoder took %lu s"
                :  "write(%d,,%d) to decoder took %lu s: %s",
            buf->pfd, nwrote, (unsigned long)duration, id);

    return status;

flush_timeo:
    if(changed)
        set_fd_nonblock(buf->pfd);

    uerror(id == NULL
            ?  "write(%d,,%lu) to decoder timed-out (%lu s)"
            :  "write(%d,,%lu) to decoder timed-out (%lu s): %s",
        buf->pfd, (unsigned long)len, (unsigned long)(time(NULL) - start), id);

    return EAGAIN;
}

int
pbuf_write(
    pbuf*               buf,
    const char*         ptr,
    size_t              nbytes,
    unsigned int        timeo,          /* N.B. Not a struct timeval */
    const char* const   id)
{
    size_t tlen;

    while (nbytes > 0) {
        tlen = (size_t)(buf->upperbound - buf->ptr);
        tlen = (nbytes < tlen) ? nbytes : tlen;

        memcpy(buf->ptr, ptr, tlen);

        buf->ptr += tlen;

        if(buf->ptr == buf->upperbound) {
            const int   status = pbuf_flush(buf, 1, timeo, id);

            if(status != ENOERR)
                return status;
        }

        ptr += tlen;
        nbytes -= tlen;
    }

    /* write what we can */
    return pbuf_flush(buf, 0, 0, id);
}
