/* Written by Charles Harris charles.harris@sdl.usu.edu */

<<<<<<< HEAD
#include "zeros.h"

double
bisect(callback_type f, double xa, double xb, double xtol, double rtol, int iter, default_parameters *params)
=======
#include <math.h>
#include "zeros.h"

double
bisect(callback_type f, double xa, double xb, double xtol, double rtol,
       int iter, default_parameters *params)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
{
    int i;
    double dm,xm,fm,fa,fb,tol;

    tol = xtol + rtol*(fabs(xa) + fabs(xb));

    fa = (*f)(xa,params);
    fb = (*f)(xb,params);
    params->funcalls = 2;
<<<<<<< HEAD
    if (fa*fb > 0) {ERROR(params,SIGNERR,0.0);}
    if (fa == 0) return xa;
    if (fb == 0) return xb;
    dm = xb - xa;
    params->iterations = 0;
    for(i=0; i<iter; i++) {
=======
    if (fa*fb > 0) {
        params->error_num = SIGNERR;
        return 0.;
    }
    if (fa == 0) {
        return xa;
    }
    if (fb == 0) {
        return xb;
    }
    dm = xb - xa;
    params->iterations = 0;
    for (i=0; i<iter; i++) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        params->iterations++;
        dm *= .5;
        xm = xa + dm;
        fm = (*f)(xm,params);
        params->funcalls++;
        if (fm*fa >= 0) {
            xa = xm;
        }
<<<<<<< HEAD
        if (fm == 0 || fabs(dm) < tol)
            return xm;
    }
    ERROR(params,CONVERR,xa);
=======
        if (fm == 0 || fabs(dm) < tol) {
            return xm;
        }
    }
    params->error_num = CONVERR;
    return xa;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}
