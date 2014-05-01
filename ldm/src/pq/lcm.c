/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: lcm.c,v 1.3 1998/10/16 19:28:13 steve Exp $ */

#include <ldmconfig.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#ifndef NDEBUG
#include <stdio.h>
#endif
#include "lcm.h"

/*
 * Compute the Greatest Common Divisor
 * of mm and nn using modified Euclid's algorithm
 */
static unsigned long
gcd0(unsigned long mm, unsigned long nn)
{
	unsigned long tmp;

	assert(nn <= mm);

	while(nn > 1)
	{
		tmp = nn;
		nn = mm % nn;
		mm = tmp;
	}
	if(nn == 0)
		return mm;
	/* else */
	assert(nn == 1);
	return 1;
}


/*
 * Public driver for above
 */
unsigned long
gcd(unsigned long mm, unsigned long nn)
{
	if(mm < nn)
	{
		/* swap */
		unsigned long tmp = mm;
		mm = nn;
		nn = tmp;
	}
	return gcd0(mm, nn);
}


/*
 * Compute the Least Common Multiple
 * of mm and nn.
 * If the result would overflow, return
 * ULONG_MAX and set errno to ERANGE.
 * (ERANGE chosen because strtoul uses this.
 * I personally think EDOM would be a better choice.)
 */
unsigned long
lcm(unsigned long mm, unsigned long nn)
{ 
	unsigned long gg;

	if(mm == nn)
		return mm;

	if(mm < nn)
	{
		/* swap */
		gg = mm;
		mm = nn;
		nn = gg;
	}
	
	if(nn == 0)
		return 0;

	if(nn == 1)
		return mm;

	gg = gcd0(mm, nn);
	assert(gg != 0); /* covered by (nn == 0) case above */
	mm /= gg;

	/* bounds check */
	gg = ULONG_MAX/nn;
	if(mm < gg || (mm == gg && ((ULONG_MAX % nn) != 0)))
		return (mm * nn);
	/* else, overflow */
	errno = ERANGE;
	return ULONG_MAX;
}


#ifdef TEST_LCM /** Test driver **/

#include <stdio.h>
#include <stdlib.h>

#ifndef ENOERR
#define ENOERR 0
#endif

static int
ul_arg(const char *const optarg, unsigned long *result)
{
	char *cp;
	unsigned long ul = strtoul(optarg, &cp, 10);
	if(ul == ULONG_MAX && errno == ERANGE)
		return ERANGE;
	if(ul == 0 && cp == optarg)
		return EDOM;
	*result = ul;
	return ENOERR;
}


main(int ac, char *av[])
{
	unsigned long mm = 0;
	unsigned long nn = 0;
	unsigned long gg = 0;

	if(ac != 3)
	{
		fprintf(stderr, "Usage: %s mm nn\n",
			av[0]);
		exit(EXIT_FAILURE);
	}
	if(ul_arg(av[1], &mm) != ENOERR)
	{
		fprintf(stderr,
			 "Illegal operand \"%s\"\n",
				av[1]);
		exit(EXIT_FAILURE);
	}
	if(ul_arg(av[2], &nn) != ENOERR)
	{
		fprintf(stderr,
			 "Illegal operand \"%s\"\n",
				av[2]);
		exit(EXIT_FAILURE);
	}

	gg = gcd(mm,nn);	

	printf("gcd of %lu, %lu is %lu\n", mm, nn, gg);

	gg = lcm(mm,nn);	
	if(gg == ULONG_MAX && errno == ERANGE)
		fprintf(stderr,"Overflow\n");

	printf("lcm of %lu, %lu is %lu\n", mm, nn, gg);

	exit(EXIT_SUCCESS);
}
#endif /* TEST_LCM */
