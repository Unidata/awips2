/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: fdnb.c,v 1.5 2002/12/09 18:23:56 steve Exp $ */

#include <ldmconfig.h>
#include <sys/types.h>
#include <fcntl.h>
#include "ulog.h"
#include "fdnb.h"

/* some systems have FNDELAY (== O_NDELAY) different from O_NONBLOCK */
#if defined(__ultrix) || defined(ultrix)
/* ultrix 4.3 (and before?) */
/* #define NB_O_NONBLOCK (FNDELAY | O_NONBLOCK) */
#define NB_O_NONBLOCK FNDELAY
#endif
/* default do like posix sez */
#ifndef NB_O_NONBLOCK
#define NB_O_NONBLOCK O_NONBLOCK
#endif
/*
 * Set descriptor for non blocking
 * return 1 if changed, 0 otherwise.
 */
int
set_fd_nonblock(int fd)
{
	unsigned flags = (unsigned)fcntl(fd, F_GETFL, 0);

	if(flags == (unsigned)-1)
	{
		serror("fcntl(..., F_GETFL,)");
		return 0;
	}
	/* else */

	if(flags & NB_O_NONBLOCK)
		return 0; /* no change required */
	/* else */
	
	flags |= NB_O_NONBLOCK;
	if(fcntl(fd, F_SETFL, (int)flags) == -1)
	{
		serror("fcntl(..., F_SETFL, O_NONBLOCK)");
		return 0;
	}

	return 1;
}


/*
 * clear descriptor for non blocking
 * (EG, set to blocking) 
 * return 1 if changed, 0 otherwise.
 */
int
clr_fd_nonblock(int fd)
{
	int flags = fcntl(fd, F_GETFL, 0);

	if(flags == -1)
	{
		serror("fcntl(..., F_GETFL,)");
		return 0;
	}
	/* else */

	if(!((unsigned int)flags & NB_O_NONBLOCK))
		return 0; /* no change required */
	/* else */
	
	flags &= ~NB_O_NONBLOCK;
	if(fcntl(fd, F_SETFL, flags) == -1)
	{
		serror("fcntl(..., F_SETFL, &= ~O_NONBLOCK)");
		return 0;
	}

	return 1;
}
