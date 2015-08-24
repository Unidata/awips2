<<<<<<< HEAD
/*							tandg.c
 *
 *	Circular tangent of argument in degrees
=======
/*                                                     tandg.c
 *
 *     Circular tangent of argument in degrees
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, tandg();
 *
 * y = tandg( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns the circular tangent of the argument x in degrees.
 *
 * Range reduction is modulo pi/4.  A rational function
 *       x + x**3 P(x**2)/Q(x**2)
 * is employed in the basic interval [0, pi/4].
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    DEC      0,10          8000      3.4e-17      1.2e-17
 *    IEEE     0,10         30000      3.2e-16      8.4e-17
 *
 * ERROR MESSAGES:
 *
 *   message         condition          value returned
 * tandg total loss   x > 8.0e14 (DEC)      0.0
 *                    x > 1.0e14 (IEEE)
<<<<<<< HEAD
 * tandg singularity  x = 180 k  +  90     MAXNUM
=======
 * tandg singularity  x = 180 k  +  90     NPY_INFINITY
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
 */
/*							cotdg.c
 *
 *	Circular cotangent of argument in degrees
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, cotdg();
 *
 * y = cotdg( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns the circular cotangent of the argument x in degrees.
 *
 * Range reduction is modulo pi/4.  A rational function
 *       x + x**3 P(x**2)/Q(x**2)
 * is employed in the basic interval [0, pi/4].
 *
 *
 * ERROR MESSAGES:
 *
 *   message         condition          value returned
 * cotdg total loss   x > 8.0e14 (DEC)      0.0
 *                    x > 1.0e14 (IEEE)
<<<<<<< HEAD
 * cotdg singularity  x = 180 k            MAXNUM
 */

/*
Cephes Math Library Release 2.0:  April, 1987
Copyright 1984, 1987 by Stephen L. Moshier
Direct inquiries to 30 Frost Street, Cambridge, MA 02140
*/
=======
 * cotdg singularity  x = 180 k            NPY_INFINITY
 */

/*
 * Cephes Math Library Release 2.0:  April, 1987
 * Copyright 1984, 1987 by Stephen L. Moshier
 * Direct inquiries to 30 Frost Street, Cambridge, MA 02140
 */
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

#include "mconf.h"

#ifdef UNK
static double PI180 = 1.74532925199432957692E-2;
static double lossth = 1.0e14;
#endif

#ifdef DEC
<<<<<<< HEAD
static unsigned short P1[] = {0036616,0175065,0011224,0164711};
=======
static unsigned short P1[] = { 0036616, 0175065, 0011224, 0164711 };

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#define PI180 *(double *)P1
static double lossth = 8.0e14;
#endif

#ifdef IBMPC
<<<<<<< HEAD
static unsigned short P1[] = {0x9d39,0xa252,0xdf46,0x3f91};
=======
static unsigned short P1[] = { 0x9d39, 0xa252, 0xdf46, 0x3f91 };

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#define PI180 *(double *)P1
static double lossth = 1.0e14;
#endif

#ifdef MIEEE
static unsigned short P1[] = {
<<<<<<< HEAD
0x3f91,0xdf46,0xa252,0x9d39
};
=======
    0x3f91, 0xdf46, 0xa252, 0x9d39
};

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#define PI180 *(double *)P1
static double lossth = 1.0e14;
#endif

static double tancot(double, int);
<<<<<<< HEAD
extern double MAXNUM;

double
tandg(double x)
{
    return( tancot(x,0) );
}


double
cotdg(double x)
{
    return( tancot(x,1) );
}


static double
tancot(double xx, int cotflg)
=======

double tandg(double x)
{
    return (tancot(x, 0));
}


double cotdg(double x)
{
    return (tancot(x, 1));
}


static double tancot(double xx, int cotflg)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
{
    double x;
    int sign;

    /* make argument positive but save the sign */
<<<<<<< HEAD
    if( xx < 0 ) {
        x = -xx;
        sign = -1;
    } else {
        x = xx;
        sign = 1;
    }

    if( x > lossth ) {
        mtherr("tandg", TLOSS);
        return 0.0;
    }

    /* modulo 180 */
    x = x - 180.0*floor(x/180.0);
    if (cotflg) {
        if (x <= 90.0) {
            x = 90.0 - x;
        } else {
            x = x - 90.0;
            sign *= -1;
        }
    } else {
        if (x > 90.0) {
            x = 180.0 - x;
            sign *= -1;
        }
    }
    if (x == 0.0) {
        return 0.0;
    } else if (x == 45.0) {
        return sign*1.0;
    } else if (x == 90.0) {
        mtherr( (cotflg ? "cotdg" : "tandg"), SING );
        return MAXNUM;
    }
    /* x is now transformed into [0, 90) */
    return sign * tan(x*PI180);
=======
    if (xx < 0) {
	x = -xx;
	sign = -1;
    }
    else {
	x = xx;
	sign = 1;
    }

    if (x > lossth) {
	mtherr("tandg", TLOSS);
	return 0.0;
    }

    /* modulo 180 */
    x = x - 180.0 * floor(x / 180.0);
    if (cotflg) {
	if (x <= 90.0) {
	    x = 90.0 - x;
	}
	else {
	    x = x - 90.0;
	    sign *= -1;
	}
    }
    else {
	if (x > 90.0) {
	    x = 180.0 - x;
	    sign *= -1;
	}
    }
    if (x == 0.0) {
	return 0.0;
    }
    else if (x == 45.0) {
	return sign * 1.0;
    }
    else if (x == 90.0) {
	mtherr((cotflg ? "cotdg" : "tandg"), SING);
	return NPY_INFINITY;
    }
    /* x is now transformed into [0, 90) */
    return sign * tan(x * PI180);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}
