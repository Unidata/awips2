/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: xbuf.c,v 1.46 2001/08/24 22:54:13 russ Exp $ */

#include <ldmconfig.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "ldmalloc.h"
#include "xbuf.h"
#ifndef NDEBUG
#include "ulog.h" /* use _uassert() => ulog printing of assert messages */
#endif

#ifdef NO_MEMMOVE
/* define memmove in terms of bcopy - recividist */
#define memmove(d1, d2, n) bcopy((d2), (d1), (n))
#endif /* NO_MEMMOVE */

void
free_xbuf(
	xbuf *buf)
{
	if(buf == NULL) return;
	if(buf->base != NULL)
		free(buf->base);
	free(buf);
}


xbuf *
new_xbuf(
	size_t bufsiz)
{
	xbuf *ret;
	if(bufsiz <= 0) return NULL; /* sanity check. */
	ret = Alloc(1, xbuf);
	if(ret == NULL) return NULL;
	ret->base = NULL;
	ret->base = Alloc(bufsiz, unsigned char);
	if(ret->base == NULL)
		goto err;
	ret->bufsiz = bufsiz;
	ret->cnt = 0;
	ret->put = ret->get = ret->base;
	return ret;
err:
	free_xbuf(ret);
	return NULL;
}


void
free_clonexbuf(
	xbuf *buf)
{
	buf->base = NULL; /* protect the storage */
	free_xbuf(buf);
}


/*
 * Take a counted array and turn it into an xbuf.
 * Assumes there is good data already there
 */
xbuf *
cbuftoxbuf(
	xbuf *ret,
	unsigned char *base,
	size_t bufsiz)
{
	if(bufsiz <= 0) return NULL; /* sanity check. */
	if(ret == NULL) return NULL;
	ret->base = base;
	ret->bufsiz = bufsiz;
	ret->cnt = (ptrdiff_t)bufsiz;
	ret->get = ret->base;
	ret->put = ret->base + bufsiz;
	return ret;
}


/*
 * Intialize 'clone' to use data in xbuf, but ready to be rescanned.
 * Assumes there is good data already there
 */
xbuf *
clone_xbuf(
	xbuf *buf,
	xbuf *clone,
	int backoff) /* number of characters at the end to leave off */
{
	if(buf == NULL || clone == NULL) return NULL;
	clone->bufsiz = (size_t)((buf->get - backoff) - buf->base);
	/* assert(clone->bufsiz > 0) */
	clone->base = buf->base;
	clone->cnt = (ptrdiff_t)clone->bufsiz;
	clone->get = clone->base;
	clone->put = clone->base + clone->bufsiz;
	return clone;
}


xbuf *
expand_xbuf(
	xbuf *buf,
	size_t byhowmuch)
{
	size_t newsize;
	ptrdiff_t putoff;
	ptrdiff_t getoff;
	if(byhowmuch <= 0) return NULL;
	if(buf == NULL || buf->base == NULL)
		return NULL;
	newsize = buf->bufsiz + byhowmuch;
	/* doit */
	putoff = buf->put - buf->base;
	getoff = buf->get - buf->base;
	buf->base = (unsigned char *)realloc(buf->base, newsize);
	if(buf->base == NULL)
		return NULL;
	buf->bufsiz = newsize;
	buf->put = buf->base + putoff;
	buf->get = buf->base + getoff;
	return buf;
}


void
justify_xbuf(
	xbuf *buf,
	size_t offset)
{
	unsigned char *cp = buf->get - offset;
	size_t count = offset + (size_t)buf->cnt;

	if(cp == buf->base) return; /* already justified */

	assert(cp <= buf->put);
	assert(cp >= buf->base);
	assert( buf->cnt == buf->put - buf->get);

	if(count > 0 )
		memmove(buf->base, cp, count); /* may be overlapping */
	buf->get = buf->base + offset;
	buf->put = buf->get + buf->cnt;
	assert( buf->cnt == buf->put - buf->get);
}


size_t
resync_xbuf(
	xbuf *buf)
{
	/* assert(buf->get - buf->base >= 0); */
	size_t nbytes = buf->get - buf->base;
	buf->cnt = 0;
	buf->get = buf->put = buf->base;
	return nbytes; /* # of bytes lost */
}


/*
 * like stdio 'ungetc'
 */
int
unnextc(
	xbuf *buf,
	int ch)
{
	if(buf->get == buf->base)
		return EOB;
	buf->cnt++;
	*--buf->get = (unsigned char)ch;
	return ch;
}


int
skip(
	xbuf *buf,
	int cnt)
{
	int ch;
	while(cnt-- > 0)
		ch = nextc(buf);
	return ch;		
}


/*
 * Delete previous character.
 */
void
xbuf_rubout(
	xbuf *buf)
{
	if (buf->get == buf->base)
		return;
				/* overlapping copy */
	(void) memmove(buf->get-1, buf->get, (size_t)buf->cnt);
	buf->get--;
	buf->put--;
}
