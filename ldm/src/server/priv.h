#ifndef _PRIV_H_
#define _PRIV_H_
/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: priv.h,v 1.3.22.1 2005/09/22 14:41:07 steve Exp $ */

extern void
ensureDumpable();

extern void
rootpriv(void);

extern void
unpriv(void);

extern void
endpriv(void);

#endif /* !_PRIV_H_ */
