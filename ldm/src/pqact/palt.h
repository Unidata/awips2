/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: palt.h,v 1.59 1997/03/12 00:56:11 davis Exp $ */
#ifndef _PALT_H_
#define _PALT_H_

#ifdef __cplusplus
extern "C" int readPatFile(char *path);
extern "C" int processProduct(const prod_info *infop, const void *datap,
	const void *xprod, size_t len,
	void *otherargs);
extern "C" void dummyprod(char *ident);
#elif defined(__STDC__)
extern int readPatFile(char *path);
extern int processProduct(const prod_info *infop, const void *datap,
	void *xprod, size_t len,
	void *otherargs);
extern void dummyprod(char *ident);
#else /* Old Style C */
extern int readPatFile();
extern int processProduct();
extern void dummyprod();
#endif

#endif /* !_PALT_H_ */
