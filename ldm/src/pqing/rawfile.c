/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: rawfile.c,v 1.16 1998/10/16 19:28:53 steve Exp $ */

/* It amazes me that POSIX has us pull in 3 include files to use open(2) */
#include <ldmconfig.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "ulog.h"


static int rawfd = -1;

int
open_rawfile(const char *rawfname)
{

	if(rawfname != 0 && *rawfname != 0)
	{
		rawfd = open(rawfname, (O_WRONLY|O_CREAT), 0664);
		if(rawfd < 0)
		{
			serror("Couldn't open feed raw file \"%s\"",
				rawfname);
			return(-1);
		}
	}
	return(rawfd);
}


void
write_rawfile(size_t nbytes, char *buf)
{
	if( rawfd == -1 )
		return;

	if(nbytes > 0)
	{
		const ssize_t nwrote = write(rawfd, buf, nbytes);
		if(nwrote < nbytes)
		{
			serror("Couldn't write raw data, closing raw file");
			(void) close(rawfd);
			rawfd = -1;
		}
	}
}
