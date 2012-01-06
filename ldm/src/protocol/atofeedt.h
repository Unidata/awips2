/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: atofeedt.h,v 1.13.2.1.6.1 2005/01/21 21:31:58 steve Exp $ */

#ifndef _ATOFEEDT_H_
#define _ATOFEEDT_H_

/* correlate string names with associated enumerated type */
struct fal {
#define MAXFEEDTYPELEN 14	/* max len of name string */
	char *name;
	feedtypet type;
};
/* defined in misc/feedtype.c */
extern struct fal fassoc[];

/*
 * Feedtype parsing error codes, which are possible returns from
 * strfeedtypet().
 */
#define FEEDTYPE_OK		0 /* parse succeeded */
#define FEEDTYPE_ERR_RP		1 /* right paren expected */ 
#define FEEDTYPE_ERR_PRIM	2 /* syntax error, incomplete expression */
#define	FEEDTYPE_ERR_GARB	3 /* garbage at end of expression */
#define	FEEDTYPE_ERR_UKFT	4 /* unknown feedtype */
#define	FEEDTYPE_ERR_OTHER	5 /* other error */

#ifdef __cplusplus
extern "C" int strfeedtypet(const char *str, feedtypet *result);
extern "C" feedtypet atofeedtypet(const char *str); /* deprecated */
extern "C" char* strfeederr(int);
#elif defined(__STDC__)
extern int strfeedtypet(const char *str, feedtypet *result);
extern feedtypet atofeedtypet(const char *str); /* deprecated */
extern char* strfeederr(int);
#else /* Old Style C */
extern int strfeedtypet();
extern feedtypet atofeedtypet(); /* deprecated */
extern char* strfeederr();
#endif

#endif /* !_ATOFEEDT_H_ */
