/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: fxbuf.c,v 1.28 2000/06/28 15:59:11 russ Exp $ */

#include <ldmconfig.h>
#include <string.h>
#include "xbuf.h"
#ifndef MCIDAS_ONLY
#include "feed.h"
#endif
#include "ulog.h"
#ifndef NULL
#define NULL 0
#endif

/*
 * application specific xbuf code
 */

#ifndef INIT_CIRCBUFSIZE
#define INIT_CIRCBUFSIZE 16384
#endif

#ifndef CHUNKSIZE
#define CHUNKSIZE 4096
#endif

/* allocated by initTheXbuf() */
static xbuf *theBuf  = NULL;
/* pointer set to the right function by initTheXbuf() */
static int (*read_feed)(int ifd, char *, size_t, size_t *);

/* pointer set to the right function by setTheScanner() */
static int (*theScanner)(xbuf *buf) = NULL;

void
setTheScanner(int (*scanner)(xbuf *buf))
{
	theScanner = scanner;	
}

int
initTheXbuf(int (*readfunct)(int ifd, char *buf, size_t nbytes, size_t *ngotp))
{
	read_feed = readfunct;

	if(theBuf == NULL)
	{
		theBuf = new_xbuf(INIT_CIRCBUFSIZE);
		if(theBuf == NULL)
		{
			const int status = errno == 0 ? ENOMEM : errno;
			serror("new_xbuf");
			return status;
		}
	}
	return ENOERR;
}


/*
 * There is data available on the feed. Read it into the buffer
 * then deal with what we got.
 */
int
feedTheXbuf(int ifd)
{
	int status;
	size_t nn = 0;
	ptrdiff_t remaining; /* space available in buffer */

	remaining = (ptrdiff_t)theBuf->bufsiz - (theBuf->get - theBuf->base);
	if(remaining <= CHUNKSIZE)
	{
#define MAX_CIRCBUFSIZE	1048576
		if(theBuf->bufsiz >= MAX_CIRCBUFSIZE)
		{
			uerror("Buffer would exceed %d, resetting input",
				MAX_CIRCBUFSIZE);
			justify_xbuf(theBuf, 0);
			
		}
		uinfo("Expanding input buffer size to %d\n",
			2 * theBuf->bufsiz);
		theBuf = expand_xbuf(theBuf, theBuf->bufsiz);
		if(theBuf == NULL)
		{
			status = errno == 0 ? ENOMEM : errno;
			serror("expand_xbuf");
			return status;
		}
	}

	status = (*read_feed)(ifd, (char *)theBuf->put, CHUNKSIZE, &nn);
	if(status != ENOERR)
	{
		uerror("feedTheXbuf:read_feed: %d %s",
			status, strerror(status));
		return status;
	}
	/* else */
	if(nn == 0)
		return ENODATA; /* end of file */
	/* else */
	/* usual case */
	/* assert(nn > 0); */
	theBuf->cnt += nn;
	theBuf->put += nn;
	return ENOERR;
}


int
scanTheXbuf(void)
{
	return (*theScanner)(theBuf);
}
