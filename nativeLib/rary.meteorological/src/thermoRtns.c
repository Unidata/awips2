/****************************************************************/
/*   Library routines to compute the equivalent potential temp  */
/*   (ept) and the temp of saturation adiabat (tsa).  These     */
/*   routines were lifted from the sharp code used in the       */
/*   interactive Skew-T application.  Original codes used on    */
/*   DARE were unavailable.  This was resurrected for the       */
/*   FORTRAN routine 'wbzero.f'.                                */
/*   Dale Perry, 5/97                                           */
/****************************************************************/

#include "meteoLib.h"
#include <math.h>

#if 0
/* Use this un-called function stub to fool the compiler to    */
/* accept these routines as extern "C" routines in the header  */
/* file 'meteoLib.h'.  If this is not included, the function   */
/* call names get name mangled and are not recognizable to the */
/* linking process in igc.                                     */
void thermoRtns() { }
#endif

float FTN_MANGLE (ept) (float *t, float *td, float *p)
{
    /* System generated locals */
    float ret_val;
    double d1, d2;

    /* Local variables */
    static float tlcl;
    extern double tcon(float *, float *);
    static float eptk, w, tk, tc, tl, tdc, pt;
    extern double wmr(float *, float *);


/*   THIS FUNCTION RETURNS THE EQUIVALENT POTENTIAL TEMP EPT           */
/*   (KELVIN) FOR A PARCEL OF AIR INITIALLY AT TEMP T (KELVIN),        */
/*   DEW POINT TD (KELVIN) AND PRESSURE P (MILLIBARS).                 */

/*      BAKER,SCHLATTER 17-MAY-1982     Original version               */

/*   THE FORMULA USED                                                  */
/*   IS EQ.(43) IN BOLTON, DAVID, 1980: "THE COMPUTATION OF EQUIVALENT */
/*   POTENTIAL TEMPERATURE," MONTHLY WEATHER REVIEW, VOL. 108, NO. 7   */
/*   (JULY), PP. 1046-1053. THE MAXIMUM ERROR IN EPT IN 0.3C.  IN MOST */
/*   CASES THE ERROR IS LESS THAN 0.1C.                                */

/*   COMPUTE THE MIXING RATIO (GRAMS OF WATER VAPOR PER KILOGRAM OF    */
/*   DRY AIR). */
    
    tdc = *td - 273.16;
    w = wmr(p, &tdc);
    
/*   COMPUTE THE TEMP (CELSIUS) AT THE LIFTING CONDENSATION LEVEL.     */
    tc = *t - 273.16;
    tlcl = tcon(&tc, &tdc);
    tk = *t; /* + 273.16; */
    tl = tlcl + 273.16;
    d1 = (double) (1e3 / *p);
    d2 = (double) ((1.0 - w * 2.8e-4) * 0.2854);
    pt = tk * pow(d1, d2);
    eptk = pt * exp((3.376 / tl - 0.00254) * w * (w * 8.1e-4 + 1.0));
    ret_val = eptk; /* - 273.16; */
    return ret_val;
} /* ept */
 
double wmr(float *p, float *t)
{
    /* Initialized data */

    static float eps = 0.62197;

    /* System generated locals */
    float ret_val;

    /* Local variables */
    static float r, x, fwesw;
    extern double esw(float *);
    static float wfw;


/*   THIS FUNCTION APPROXIMATES THE MIXING RATIO WMR (GRAMS OF WATER   */
/*   VAPOR PER KILOGRAM OF DRY AIR) GIVEN THE PRESSURE P (MB) AND THE  */
/*   TEMPERATURE T (CELSIUS).                                          */

/*      BAKER,SCHLATTER 17-MAY-1982     Original version               */

/*   THE FORMULA USED IS GIVEN ON P. 302 OF THE                        */
/*   SMITHSONIAN METEOROLOGICAL TABLES BY ROLAND LIST (6TH EDITION).   */

/*   EPS = RATIO OF THE MEAN MOLECULAR WEIGHT OF WATER (18.016 G/MOLE) */
/*   TO THAT OF DRY AIR (28.966 G/MOLE)                                */
/*   THE NEXT TWO LINES CONTAIN A FORMULA BY HERMAN WOBUS FOR THE      */
/*   CORRECTION FACTOR WFW FOR THE DEPARTURE OF THE MIXTURE OF AIR     */
/*   AND WATER VAPOR FROM THE IDEAL GAS LAW. THE FORMULA FITS VALUES   */
/*   IN TABLE 89, P. 340 OF THE SMITHSONIAN METEOROLOGICAL TABLES,     */
/*   BUT ONLY FOR TEMPERATURES AND PRESSURES NORMALLY ENCOUNTERED IN   */
/*   IN THE ATMOSPHERE.                                                */
    
    x = (*t - 12.5 + 7500.0 / *p) * 0.02;
    wfw = *p * 4.5e-6 + 1.0 + x * 0.0014 * x;
    fwesw = wfw * esw(t);
    r = eps * fwesw / (*p - fwesw);
    
/*   CONVERT R FROM A DIMENSIONLESS RATIO TO GRAMS/KILOGRAM. */
    
    ret_val = r * 1e3;
    return ret_val;
} /* wmr */

double esw(float *t)
{
    /* Initialized data */

    static float es0 = 6.1078;

    /* System generated locals */
    float ret_val, r1;

    /* Local variables */
    static float pol;


/*   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE ESW (MILLIBARS) */
/*   OVER LIQUID WATER GIVEN THE TEMPERATURE T (CELSIUS).                */

/*      BAKER,SCHLATTER 17-MAY-1982     Original version                 */

/*   THE POLYNOMIAL APPROXIMATION BELOW IS DUE TO HERMAN WOBUS, A        */
/*   MATHEMATICIAN WHO WORKED AT THE NAVY WEATHER RESEARCH FACILITY,     */
/*   NORFOLK, VIRGINIA, BUT WHO IS NOW RETIRED. THE COEFFICIENTS OF THE  */
/*   POLYNOMIAL WERE CHOSEN TO FIT THE VALUES IN TABLE 94 ON PP. 351-353 */
/*   OF THE SMITHSONIAN METEOROLOGICAL TABLES BY ROLAND LIST (6TH ED).   */
/*   THE APPROXIMATION IS VALID FOR -50 < T < 100C.                      */

/*   ES0 = SATURATION VAPOR RESSURE OVER LIQUID WATER AT 0C */
    pol = *t * (*t * (*t * (*t * (*t * (*t * (*t * (*t * (*t * 
            -3.0994571e-20 + 1.1112018e-17) - 1.7892321e-15) + 
            2.1874425e-13) - 2.9883885e-11) + 4.3884187e-9) -
            6.1117958e-7) + 7.8736169e-5) - 0.0090826951) + 0.99999683;
            
/* Computing 8th power    */
    r1 = pol, r1 *= r1, r1 *= r1;
    ret_val = es0 / (r1 * r1);
    return ret_val;
} /* esw */

double tcon(float *t, float *d)
{
    /* System generated locals */
    float ret_val;

    /* Local variables */
    static float s, dlt;


/*   THIS FUNCTION RETURNS THE TEMPERATURE TCON (CELSIUS) AT THE LIFTING */
/*   CONDENSATION LEVEL, GIVEN THE TEMPERATURE T (CELSIUS) AND THE       */
/*   DEW POINT D (CELSIUS).                                              */

/*      BAKER,SCHLATTER 17-MAY-1982     Original version                 */

/*   COMPUTE THE DEW POINT DEPRESSION S. */
    
    s = *t - *d;
    
/*   THE APPROXIMATION BELOW, A THIRD ORDER POLYNOMIAL IN S AND T, */
/*   IS DUE TO HERMAN WOBUS. THE SOURCE OF DATA FOR FITTING THE    */
/*   POLYNOMIAL IS UNKNOWN.                                        */
    
    dlt = s * (*t * 0.001278 + 1.2185 + s * (s * 1.173e-5 
            - 0.00219 - *t * 5.2e-6));
    ret_val = *t - dlt;
    return ret_val;
} /* tcon */

/*   Very little documentation on these following routines, so unsure */
/*   of the origin and derivation of these algorithms.                */

float FTN_MANGLE (tsa) (float * os, float * pres)
{
    extern double w(float, float);
    const float rocp = 0.28571482;
    float tq, tqk, x, a, d;
    unsigned short int i;
    float sign;

    a= *os; /*+273.16; */
    tq=253.16;
    d=120.0;

    for (i=0; i<12; i++)
        {
        tqk=tq-273.16;
        d /=2;
        x=a*exp(-2.6518986*w(tqk, *pres)/tq)-tq*pow((1000.0/ *pres),rocp);
        if (fabs(x) <= 0.0)
            break;
        sign = (x < 0.0) ? -1:1;
        tq += (d*sign);
        }

    return tq; /*-273.16); */
}

double w(float temp, float pres)
{
    float x;

    x=esat(&temp);
    return(622.0*x/(pres-x));
}
