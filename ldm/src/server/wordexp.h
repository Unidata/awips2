/*
 *   Copyright 1994, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: wordexp.h,v 1.5 1994/05/12 20:46:40 davis Exp $ */
#ifndef _WORDEXP_H
#define _WORDEXP_H

#include <stddef.h> /* size_t */

typedef struct {
	size_t we_wordc;
	char **we_wordv;
	size_t we_offs;
} wordexp_t;

/* wordexp() flags Argument */
#define WRDE_APPEND   0x01
#define WRDE_DOOFFS   0x02
#define WRDE_NOCMD    0x04
#define WRDE_REUSE    0x08
#define WRDE_SHOWERR  0x10
#define WRDE_UNDEF    0x20

/*
 * wordexp() Return Values
 */
/* required */
#define WRDE_BADCHAR  1
#define WRDE_BADVAL   2
#define WRDE_BADCMD   3
#define WRDE_NOSPACE  4 
#define WRDE_SYNTAX   5
/* implementation specific */
#define WRDE_INVAL    22 	/* Invalid argument */


extern int wordexp(const char *words, wordexp_t *pwordexp, int flags);
extern void wordfree(wordexp_t *pwordexp);
#endif  /* _WORDEXP_H */
