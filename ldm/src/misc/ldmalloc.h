/* $Id: ldmalloc.h,v 1.1.22.1 2005/09/21 18:37:12 steve Exp $ */
#ifndef _ALLOC_H_
#define _ALLOC_H_

#ifndef NO_STDLIB_H
/* assume __STDC__ */

#include <stdlib.h>

#else
/* old style declarations */

extern char *malloc();
extern char *realloc();

#ifndef NULL
#define NULL  0
#endif /* !NULL */

#endif /* !NO_STDLIB_H */


#define Alloc(theNum, theType) \
	(theType *)malloc((size_t)(sizeof(theType) * (theNum))) 


#define ARRAYLEN(arr) (sizeof(arr)/sizeof(arr[0]))

#endif /* !_ALLOC_H_ */
