#include "meteoLib.h"
#include <stdio.h>
#include <math.h>

/* This routine calculates the saturation tempurature of an equivalent
   temperature at given pressure using the adiabatic definition */

float FTN_MANGLE (temp_of_te) (const float * te, const float * press)
    {
    static const int tmin = 193;
    static const int tmax = 333;
    static const int nval = 141; // 1 + tmax - tmin
    static float TeData[987]; // 7 * nval - gfortran doesn't allow non-hardcoded values in const
    static float * Te1000 = 0;
    static float * Te850 = 0;
    static float * Te700 = 0;
    static float * Te600 = 0;
    static float * Te500 = 0;
    static float * Te350 = 0;
    static float * Te200 = 0;
    float * TeLookup;
    float base;
    float t,p,t1,t2,d,d1,d2,w;
    int i;

/*  int diag = (*te>243 && *te<243.05 && *press>394.9 && *press<395); */

    /* very first time, construct lookup table Te's of T from 193 to 343 K */
    if (Te1000==0)
        {
        Te1000 = TeData-tmin;
        Te850 = Te1000+nval;
        Te700 = Te850+nval;
        Te600 = Te700+nval;
        Te500 = Te600+nval;
        Te350 = Te500+nval;
        Te200 = Te350+nval;
        p = 1000;
        for (t=tmin; t<=tmax; t+=1)
             Te1000[(int)t] = adiabatic_te(&t,&p);
        p = 850;
        for (t=tmin; t<=tmax; t+=1)
             Te850[(int)t] = adiabatic_te(&t,&p);
        p = 700;
        for (t=tmin; t<=tmax; t+=1)
             Te700[(int)t] = adiabatic_te(&t,&p);
        p = 600;
        for (t=tmin; t<=tmax; t+=1)
             Te600[(int)t] = adiabatic_te(&t,&p);
        p = 500;
        for (t=tmin; t<=tmax; t+=1)
             Te500[(int)t] = adiabatic_te(&t,&p);
        p = 350;
        for (t=tmin; t<=tmax; t+=1)
             Te350[(int)t] = adiabatic_te(&t,&p);
        p = 200;
        for (t=tmin; t<=tmax; t+=1)
             Te200[(int)t] = adiabatic_te(&t,&p);
        }

    /* find correct table, check for beyond bounds of table */
    if (*press<=250)
        {
        TeLookup = Te200;
        base = 200;
        }
    else if (*press<=400)
        {
        TeLookup = Te350;
        base = 350;
        }
    else if (*press<=550)
        {
        TeLookup = Te500;
        base = 500;
        }
    else if (*press<=650)
        {
        TeLookup = Te600;
        base = 600;
        }
    else if (*press<=750)
        {
        TeLookup = Te700;
        base = 700;
        }
    else if (*press<=900)
        {
        TeLookup = Te850;
        base = 850;
        }
    else
        {
        TeLookup = Te1000;
        base = 1000;
        }
    if (*te<TeLookup[tmin+1]) return *te;
    if (*te>=TeLookup[tmax]) return 1e37;

    /* use table to get first guesses for value of temp */
/*  if (diag) printf("te,base %.2f %.0f\n",*te,base); */
    t1 = tmin;
    t2 = (int)(*te);
    if (t2>tmax) t2 = tmax;
    while (t2-t1>=3)
        {
        t = (int)((t1+t2)/2);
        if (TeLookup[(int)t]>*te)
            t2 = t;
        else if (TeLookup[(int)t]<*te)
            t1 = t;
        else
            {
            if (t1<t-1) t1 = t-1;
            if (t2>t+1) t2 = t+1;
            break;
            }
        }
/*  if (diag) printf("t1,t2,te1,te2  %.2f %.2f  %.2f %.2f\n",t1,t2,
            TeLookup[(int)t1],TeLookup[(int)t2]); */
    w = sqrt(base/(*press));
    t1 = (1-w)*TeLookup[(int)t1]+w*t1;
    t2 = (1-w)*TeLookup[(int)t2]+w*t2;
/*  if (diag) printf("t1,t2 %.2f %.2f\n",t1,t2); */

    /* Iterate to find the exact solution */
    d1 = *te - adiabatic_te(&t1,press);
    d2 = adiabatic_te(&t2,press) - *te;
    w = d2/(d1+d2);
    t = w*t1+(1-w)*t2;
    d = adiabatic_te(&t,press) - *te;
    i = 0;
    while (i++<10)
        {
        if (d>0.01)
            {
            d2 = d;
            t2 = t;
            }
        else if (d<-0.01)
            {
            d1 = -d;
            t1 = t;
            }
        else
            break;
        w = d2/(d1+d2);
        t = w*t1+(1-w)*t2;
        d = adiabatic_te(&t,press) - *te;
        }
/*  if (diag) printf("t,i %.2f %d\n",t,i); */
    return t;

    }
