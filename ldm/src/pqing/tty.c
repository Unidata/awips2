/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: tty.c,v 1.41 1998/10/16 19:28:55 steve Exp $ */

/* #define _POSIX_SOURCE */

#include <ldmconfig.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>
#include <errno.h>
#ifndef ENOERR
#define ENOERR 0
#endif
#include <stdlib.h>
#include <string.h>
#include "ulog.h"
#include "feed.h"
#include "rawfile.h"

/* Begin rtty */
/*
 * We want berkeley style exclusive mode badly enough to 
 * to do this.
 */
#ifndef TIOCEXCL
#	ifdef sun /* as of SunOS 4.1 */
#	include <sys/ttold.h> /* sys/ioctl.h has clashes with termios.h */
#	endif /* sun */
#	ifdef _AIX /* as of AIX 3.0 */
#	include <sys/ioctl.h>
#	endif /*_AIX */
#	ifdef ultrix /* as of ULTRIX V4.0 */
#	include <sys/ioctl.h>
#	endif /* ultrix */
#endif /* !TIOCEXCL */

enum rtty_csize_t {
	RTTY_CS5 = CS5,
	RTTY_CS6 = CS6,
	RTTY_CS7 = CS7,
	RTTY_CS8 = CS8
};
typedef enum rtty_csize_t rtty_csize_t;

enum rtty_parity_t {
	RTTY_P_NONE = 0,
	RTTY_P_EVEN = PARENB,
	RTTY_P_ODD = (PARENB | PARODD)
};
typedef enum rtty_parity_t rtty_parity_t;

enum rtty_cstopb_t {
	RTTY_CSTOPB1 = 0,
	RTTY_CSTOPB2 = CSTOPB
};
typedef enum rtty_cstopb_t rtty_cstopb_t;


static int
rtty_open(const char *const path, 
	const speed_t speed,
	const rtty_csize_t csize,
	const rtty_parity_t parity,
	const rtty_cstopb_t cstopb,
	int *const fdp,
	struct termios *const savep)
{
	int status;
	int fd;
	struct termios tbuf, sav; 

	fd = open(path, O_RDONLY | O_EXCL | O_NOCTTY);
	if(fd < 0)
		return errno;

#if defined(TIOCEXCL) && !defined(__sgi)
	if(ioctl(fd, TIOCEXCL, NULL) < 0)
	{
		status = errno;
		(void) close(fd);
		return status;
	}
#endif /* TIOCEXCL */

	memset(&tbuf, 0, sizeof(tbuf)); /* ?? paranoid */
	if(tcgetattr(fd, &tbuf) != 0)
	{
		status = errno;
		(void) close(fd);
		return status;
	}
	sav = tbuf;

	/*
	 * Set the speed.
	 */
	if(cfsetospeed(&tbuf, speed) != 0
		|| cfsetispeed(&tbuf, speed) != 0)
	{
		status = EINVAL; /* ?? */
		(void) close(fd);
		return status;
	}

	/*
 	 * Correct the termios to our liking.
	 * We clear most bits and set the few we need.
	 */

	tbuf.c_cflag &= ~( HUPCL | CSIZE | PARENB | PARODD | CSTOPB);
	tbuf.c_cflag |= ( CLOCAL | CREAD );
	tbuf.c_cflag |= (csize | parity | cstopb);

	tbuf.c_oflag &= ~(OPOST);

	tbuf.c_iflag &= ~(BRKINT | ICRNL
		| IGNCR | IGNPAR | INLCR
		| ISTRIP | IXOFF | IXON | PARMRK );
	tbuf.c_iflag  |= IGNBRK;

	if(parity != RTTY_P_NONE)
		tbuf.c_iflag |= INPCK;

	if(csize != RTTY_CS8)
		tbuf.c_iflag |= ISTRIP;

	tbuf.c_lflag  &= ~( ECHO | ECHOE | ECHOK | ECHONL | ICANON
		| IEXTEN | ISIG | NOFLSH | TOSTOP );

#if defined(VMIN)  && defined(VTIME)
	/* Wake up when we've got at least one character. */
	tbuf.c_cc[VMIN] = 1; 
	/* Wait as long as it takes. */
	tbuf.c_cc[VTIME] = 0;
#endif

	if(tcsetattr(fd, TCSAFLUSH, &tbuf) != 0)
	{
		status = errno;
		(void) close(fd);
		return status;
	}
	
	if(savep != NULL)
		*savep = sav;
	*fdp = fd;
	return ENOERR;
}


static int
rtty_close(const int fd,
	const struct termios *const savep)
{
	/* don't try to close what was never opened */
	if(fd < 0)
		return ENOERR;

	if(savep != NULL)
	{
		(void)tcsetattr(fd, TCSAFLUSH, savep);
	}
	
	if(close(fd) < 0)
		return errno;

	return ENOERR;
}


static int
rtty_read(const int fd,
	char *const rbuf, 
	size_t *const nreadp, 
	unsigned long *const perrsp)
{
	ssize_t nread = read(fd, rbuf, *nreadp);
	if(nread == -1)
		return errno;

	if(perrsp != NULL)
	{
		/* check for parity errors */
		char *cp = rbuf;
		char const *end =  &rbuf[nread];
		while(cp < end)
		{
/* "parity or framing error is read as a single ASCII NULL" */
			if(*cp == '\0')
			{
				(*perrsp)++;
			}
			cp++;
		}
		
	}

	*nreadp = (size_t) nread;
	return ENOERR;
}

/* End rtty */
/* Begin tty */

static struct termios sav; 	/* use to reset tty params on close */

/* tty statistics */
static unsigned long total_bytes;
static unsigned long perrs; /* parity errors */

static unsigned long *perrsp = NULL; 


static rtty_parity_t
init_parity(char *td_parity)	/* should be "no[ne]", "odd", or "even" */
{
	if (td_parity != NULL && *td_parity != 0 )
	{
		switch (td_parity[0]) {
		case 'e' : /* "even" */
		case 'E' :
			perrsp = &perrs;
			return RTTY_P_EVEN;
		case 'o' : /* "odd" */
		case 'O' :
			perrsp = &perrs;
			return RTTY_P_ODD;
		}
	} /* else */
	/* default is "none" parity */
	perrsp = NULL;
	return RTTY_P_NONE;
}


/* convert ascii baud string to speed code */
static speed_t
tty_speed(const char *baud)
{
    speed_t            speed;

    if (baud == (char *) 0 || *baud == 0)
	speed = DEFAULT_BAUD;		/* default speed */
    else if (strcmp(baud, "0") == 0)
	speed = B0;
    else if (strcmp(baud, "50") == 0)
	speed = B50;
    else if (strcmp(baud, "75") == 0)
	speed = B75;
    else if (strcmp(baud, "110") == 0)
	speed = B110;
    else if (strcmp(baud, "134") == 0)
	speed = B134;
    else if (strcmp(baud, "150") == 0)
	speed = B150;
    else if (strcmp(baud, "200") == 0)
	speed = B200;
    else if (strcmp(baud, "300") == 0)
	speed = B300;
    else if (strcmp(baud, "600") == 0)
	speed = B600;
    else if (strcmp(baud, "1200") == 0)
	speed = B1200;
    else if (strcmp(baud, "1800") == 0)
	speed = B1800;
    else if (strcmp(baud, "2400") == 0)
	speed = B2400;
    else if (strcmp(baud, "4800") == 0)
	speed = B4800;
    else if (strcmp(baud, "9600") == 0)
	speed = B9600;
    else if (strcmp(baud, "19200") == 0)
	speed = B19200;
    else if (strcmp(baud, "38400") == 0)
	speed = B38400;
    else {
	speed = '?';		/* assumes '?' will never be a valid speed code */
    }
    return (speed);
}


void
tty_stats(void)
{
	unotice("  TTY Bytes read:   %10lu",
		total_bytes);
	if(perrsp != NULL)
		unotice("  TTY parity errors:  %8lu", 
			*perrsp);
}


int
read_tty(int ifd, char *rbuf, size_t nbytes, size_t *ngotp)
{
	size_t nread = nbytes;
	int status = rtty_read(ifd, rbuf, &nread, perrsp);
	if(status != ENOERR)
	{
		uerror("read_tty: %s", strerror(status));
		return status;
	}
	/* else */
	/* TODO: propogate parity error */
	*ngotp = nread;
	if(nread != 0)
	{
		total_bytes += nread;
		write_rawfile(nread, rbuf);
	}

	return ENOERR;
}


int
tty_open(const char *feedfname, int *const fdp)
{
	extern char *baud;
#ifndef MCIDAS_ONLY
	extern char *parity;
#endif /* MCIDAS_ONLY */

	/* set speed from command line argument */
	const speed_t speed = tty_speed(baud);
	const rtty_parity_t pty = init_parity(parity);
	/* With the current set of feeds, eight bits iff no parity */
	const rtty_csize_t csize = (pty == RTTY_P_NONE)
			 ? RTTY_CS8 : RTTY_CS7;
	int status = ENOERR;

	if(speed == '?')
	{
		status = EINVAL;
		uerror("%s is not a supported speed", baud);
		return status;
	}

	status = rtty_open(feedfname,
			speed, csize, pty, RTTY_CSTOPB1,
			fdp, &sav);
	if(status != ENOERR)
	{
		uerror("Couldn't open tty device \"%s\": %s",
			 feedfname, strerror(status));
		return(status);
	}

#ifndef MCIDAS_ONLY
	unotice("TERMIOS \"%s\": %s baud, %s parity",
		feedfname,
		(baud && baud[0]) ? baud : "default",
		(parity && parity[0]) ? parity : "default" );
#else /* !MCIDAS_ONLY */
	unotice("TERMIOS \"%s\": %s baud, %s parity",
		feedfname,
		(baud && baud[0]) ? baud : "default",
		"no" );
#endif /* !MCIDAS_ONLY */

	return status;
}


int
tty_close(int fd)
{
	return rtty_close(fd, &sav);
}

#if 0
#include <stdio.h>
#include <string.h>

main(int ac, char *av[])
{
	int status;
	char *ttypathname = av[ac-1];
	int ttyfd;

	char buf[BUFSIZ];
	size_t nread;
	unsigned long total_bytes = 0;
	unsigned perrs = 0;

	status = rtty_open(ttypathname,
			B19200, RTTY_CS8, RTTY_P_NONE, RTTY_CSTOPB1,
			&ttyfd, NULL);
	if(status != ENOERR)
	{
		fprintf(stderr, "rtty_open: %s: %s\n",
			ttypathname, strerror(status));
		exit(1);
	}

	while(1)
	{
		nread = sizeof(buf);
		status = rtty_read(ttyfd, buf, &nread, NULL);
		if(status != ENOERR)
		{
			fprintf(stderr, "rtty_read: %s\n",
				strerror(status));
			break;
		}
		else if(nread == 0)
			break;
	
		/* else */
		total_bytes += nread;
fprintf(stderr, "total_bytes %lu, perrs %u\n", total_bytes, perrs);
		if(write(STDOUT_FILENO, buf, nread) != nread)
		{
			status = errno;
			fprintf(stderr, "read: %s\n",
				strerror(status));
			break;
		}
	}
	
	(void) rtty_close(ttyfd, NULL);
	
	exit(0);
}
#endif
