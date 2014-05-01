#include "g2libc_inc/grib2.h"
/*
 * w. ebisuzaki
 *
 *  return x**y
 *
 *
 *  input: double x
 *         int y
 */
double int_power(double x, g2int y) {

        double value;

        if (y < 0) {
                y = -y;
                x = 1.0 / x;
        }
        value = 1.0;

        while (y) {
                if (y & 1) {
                        value *= x;
                }
                x = x * x;
                y >>= 1;
        }
        return value;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/g2libc/RCS/int_power.c,v $";
 static char rcs_id2[] = "$Id: int_power.c,v 1.1 2004/09/16 17:42:10 dsa Exp $";}
/*  ===================================================  */

}

