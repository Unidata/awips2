/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldmprint.h,v 1.28.18.3 2008/04/15 16:34:11 steve Exp $ */

#ifndef _LDMPRINT_H_
#define _LDMPRINT_H_
#include <stddef.h>	/* size_t */
#if defined(__cplusplus) || defined(__STDC__)

extern int
sprint_time_t(char *buf, size_t bufsize, time_t ts); /* obsolete ? */

extern int
sprint_timestampt(char *buf, size_t bufsize, const timestampt *tvp);

extern int
sprint_feedtypet(char *buf, size_t bufsize, feedtypet feedtype);

extern char *
s_feedtypet(feedtypet feedtype);

extern char *
s_rendezvoust(char *buf, size_t bufsize, const rendezvoust *rdv);

extern int
sprint_signaturet(char *buf,
	size_t bufsize, const signaturet signaturep);

extern char *
s_signaturet(char *buf, size_t bufsize, const signaturet signaturep);

extern int
sprint_prod_spec(char *buf,
	size_t bufsize, const prod_spec *specp);

extern char *
s_prod_class(char *buf,
	size_t bufsize, const prod_class_t *clssp);

extern char *
s_prod_info(char *buf, size_t bufsize, const prod_info *infop,
	int doSignature);

extern char *
s_ldm_errt(ldm_errt code);

extern char *
s_ldmproc(unsigned long proc);

/*
 * Parses a formatted signature.
 *
 * Arguments:
 *	string	Pointer to the formatted signature.
 * Returns:
 *	-1	Failure.  log_errno() called.
 *	else	Number of bytes parsed.
 */
int
sigParse(
    const char* const	string,
    signaturet* const	signature);

#else /* Old Style C */
/* TODO Will not be done, we now specify an ansi compiler */
#endif

#endif /* !_LDMPRINT_H_ */
