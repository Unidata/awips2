/*
 * This module contains statistical mathematical functions.
 *
 * $Id: statsMath.c,v 1.1.2.3 2005/07/05 18:24:00 steve Exp $
 */

#include <errno.h>
#include <math.h>

#include "statsMath.h"


/*
 * Returns the sum of binomial coefficients, nCi, where i goes from 0 through
 * k, inclusive.
 *
 * Arguments:
 *      n       The number of possibilities.
 *      k       The maximum number of unordered outcomes.
 * Returns:
 *      NaN             The value could not be computed.
 *      HUGE_VAL        The value is too large to be represented.
 *      else            the sum of the binomial coefficients, nCi, where i goes
 *                      from 0 through k, inclusive.
 */
double
sumBinomCoeff(
    unsigned    n,
    unsigned    k)
{
    double      sum = 1;                /* nC0 coefficient */
    int         i;

    for (i = 0; i < k; i++)
        sum += ((double)(n - i) / (i + 1)) * sum;       /* += nC(i+1) */

    return floor(sum + 0.5);
}
