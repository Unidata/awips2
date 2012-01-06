/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: xbuf.h,v 1.24 1998/08/06 21:03:46 davis Exp $ */
#ifndef _XBUF_H_
#define _XBUF_H_
#include <stddef.h>
/*
 * Using event driven input to a buffer, this token signifies that
 * everything in the buffer is processed: "End of Buffer"
 */
#define EOB -2

struct xbuf {
	unsigned char *base; /* base of the actual buffer */
	size_t bufsiz; /* actual size of the buffer */
	ptrdiff_t cnt; /* put - get */
	unsigned char *put; /* where data read will go */
	unsigned char *get; /* where data to be scanned comes from */
};
typedef struct xbuf xbuf;

/* like stdio getc, except obscure use of comma operator replaces _filbuf */
#define	nextc(p)	(--(p)->cnt>=0 ? ((int)*(p)->get++):(++(p)->cnt,EOB))

#define	peekc(p)	((p)->cnt>0 ? ((int)*(p)->get) : EOB)
#define	prevc(p)		(((p)->get != (p)->base)0  ((int)*((p)->get -1)) : EOB)
	

extern void free_xbuf(xbuf *buf);
extern xbuf *new_xbuf(size_t bufsize);

extern xbuf * clone_xbuf(xbuf *buf, xbuf *clone, int backoff);
extern void free_clonexbuf(xbuf *buf);
extern xbuf *cbuftoxbuf(xbuf *ret, unsigned char *base, size_t bufsize);

extern xbuf *expand_xbuf(xbuf *buf, size_t byhowmuch);

extern void justify_xbuf(xbuf *buf, size_t offset);

extern size_t resync_xbuf(xbuf *buf);

extern int unnextc(xbuf *buf, int ch);

extern void xbuf_rubout(xbuf *buf);


/* these routines more application specific, fxbuf.c */
extern void setTheScanner(int (*scanner)(xbuf *buf));
extern int
initTheXbuf(int (*readfunct)(int fd, char *buf, size_t nbytes, size_t *ngotp));
#include <errno.h>
#ifndef ENODATA
#define ENODATA (-1)
#endif
extern int feedTheXbuf(int fd);
extern int scanTheXbuf(void);

#endif /* !_XBUF_H_ */
