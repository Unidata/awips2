/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: pbuf.h,v 1.7.22.1 2007/01/19 20:47:04 steve Exp $ */

#ifndef _PBUF_H_
#define _PBUF_H_

#include <stddef.h>
#include <errno.h>
#ifndef ENOERR
#define ENOERR 0
#endif /*!ENOERR */

typedef struct {
	int pfd;
	char *base; /* actual storage */
	char *ptr; /* current position */
	char *upperbound; /* base + bufsize */
} pbuf;


#ifdef __cplusplus
extern "C" void free_pbuf(pbuf *buf);
extern "C" pbuf * new_pbuf(int pfd, size_t bufsize);
extern "C" int pbuf_flush(pbuf *buf, int /*bool_t*/ block, unsigned int timeo,
	const char* id);
extern "C" int pbuf_write(pbuf *buf, const char *ptr, size_t nbytes,
	unsigned int timeo, const char* id);
#elif defined(__STDC__)
extern void free_pbuf(pbuf *buf);
extern pbuf * new_pbuf(int pfd, size_t bufsize);
extern int pbuf_flush(pbuf *buf, int /*bool_t*/ block, unsigned int timeo,
	const char* id);
extern int pbuf_write(pbuf *buf, const char *ptr, size_t nbytes,
	unsigned int timeo, const char* id);
#else /* Old Style C */
extern void free_pbuf();
extern pbuf * new_pbuf();
extern int pbuf_flush();
extern int pbuf_write();
#endif

#endif /* !_PBUF_H_ */
