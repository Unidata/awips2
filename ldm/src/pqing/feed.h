/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *	    Not for Direct Resale. All copies to include this notice.
 */
/* $Id: feed.h,v 1.40 2000/08/16 15:40:36 rkambic Exp $ */

#ifndef _FEED_H_
#define _FEED_H_

#include <stdlib.h>
#include <errno.h>
#ifndef ENOERR
#define ENOERR 0
#endif

/*
 *  Devices understood by the 'feed.c:which()'
 */
#define UNKNOWN -1

#define TTY 1		/* unix tty (tty.c) */
#define NET 2		/* unix socket */

/* end feed.c definitions */

extern int open_feed(const char *feedfname, int *const fdp);

/* pointer to read funct */
/* extern int (*read_feed)(int fd, char *, size_t, size_t *); */
/* pointer set to the right function by open_feed() */
extern void (*feed_stats)(void); 
/* pointer set to the right function by open_feed() */
extern int (*feed_close)(int fd);

/*  FILE */
	extern int read_file(int, char *, size_t, size_t *);
	extern void file_stats(void);
	extern int file_open(const char *, int *const fdp);
	extern int file_close(int fd);
/*  FILE */
#if TTY
#	define DEFAULT_BAUD B9600
	extern int read_tty(int, char *, size_t, size_t *);
	extern void tty_stats(void);
	extern int tty_open(const char *, int *const fdp);
	extern int tty_close(int fd);
#endif /* TTY */

#if NET
	extern int server_port; /* global */
#	define INPUT_IS_SOCKET (server_port > 0)
	extern int port_open(const char *remote, unsigned short port,
		int *const fdp);
#endif /*NET*/

#if 0
#define PARITY_ERR_CH '/' /* substituted in place of a parity error */
#endif

#endif /* !_FEED_H_ */
