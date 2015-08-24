/* This file is a collection of wrappers around the
 *  Special Function  Fortran library of functions 
 *  to be compiled with the other special functions in cephes
 *
 * Functions written by Shanjie Zhang and Jianming Jin.
 * Interface by
 *  Travis E. Oliphant
 */

#ifndef _SPEC_WRAPPERS_H
#define _SPEC_WRAPPERS_H
#include "Python.h"
#include <numpy/npy_math.h>
<<<<<<< HEAD

extern double PI;
=======
#include <math.h>

#include "sf_error.h"
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

#define REAL(z) (z).real
#define IMAG(z) (z).imag
#define ABSQ(z) (z).real*(z).real + (z).imag*(z).imag;
<<<<<<< HEAD
#define ZCONVINF(z) if (REAL((z))==1.0e300) REAL((z))=NPY_INFINITY; if (REAL((z))==-1.0e300) REAL((z))=-NPY_INFINITY
#define CONVINF(x) if ((x)==1.0e300) (x)=NPY_INFINITY; if ((x)==-1.0e300) (x)=-NPY_INFINITY
#define ABS(x) ((x)<0 ? -(x) : (x))

Py_complex cgamma_wrap( Py_complex z);
Py_complex clngamma_wrap( Py_complex z);
Py_complex cpsi_wrap( Py_complex z);
Py_complex crgamma_wrap( Py_complex z);
Py_complex chyp2f1_wrap( double a, double b, double c, Py_complex z);
Py_complex chyp1f1_wrap( double a, double b, Py_complex z);
=======
#define ZCONVINF(func,z)                                                \
    do {                                                                \
        if (REAL((z)) == 1.0e300) {                                     \
            sf_error(func, SF_ERROR_OVERFLOW, NULL);                    \
            REAL((z)) = NPY_INFINITY;                                   \
        }                                                               \
        if (REAL((z)) == -1.0e300) {                                    \
            sf_error(func, SF_ERROR_OVERFLOW, NULL);                    \
            REAL((z)) = -NPY_INFINITY;                                  \
        }                                                               \
    } while (0)
#define CONVINF(func, x)                                                \
    do {                                                                \
        if ((x) == 1.0e300) {                                           \
            sf_error(func, SF_ERROR_OVERFLOW, NULL);                    \
            (x)=NPY_INFINITY;                                           \
        }                                                               \
        if ((x)==-1.0e300) {                                            \
            sf_error(func, SF_ERROR_OVERFLOW, NULL);                    \
            (x)=-NPY_INFINITY;                                          \
        }                                                               \
    } while (0)
#define ABS(x) ((x)<0 ? -(x) : (x))

npy_cdouble cgamma_wrap( npy_cdouble z);
npy_cdouble clngamma_wrap( npy_cdouble z);
npy_cdouble cpsi_wrap( npy_cdouble z);
npy_cdouble crgamma_wrap( npy_cdouble z);
npy_cdouble chyp2f1_wrap( double a, double b, double c, npy_cdouble z);
npy_cdouble chyp1f1_wrap( double a, double b, npy_cdouble z);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
double hyp1f1_wrap( double a, double b, double x);
double hypU_wrap(double a, double b, double x);
double exp1_wrap(double x);
double expi_wrap(double x);
<<<<<<< HEAD
Py_complex cexp1_wrap(Py_complex z);
Py_complex cexpi_wrap(Py_complex z);
Py_complex cerf_wrap(Py_complex z);
=======
npy_cdouble cexp1_wrap(npy_cdouble z);
npy_cdouble cexpi_wrap(npy_cdouble z);
npy_cdouble cerf_wrap(npy_cdouble z);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
int itairy_wrap(double x, double *apt, double *bpt, double *ant, double *bnt);

double struve_wrap(double v, double x);
double itstruve0_wrap(double x);
double it2struve0_wrap(double x);

double modstruve_wrap(double v, double x);
double itmodstruve0_wrap(double x);

double ber_wrap(double x);
double bei_wrap(double x);
double ker_wrap(double x);
double kei_wrap(double x);
double berp_wrap(double x);
double beip_wrap(double x);
double kerp_wrap(double x);
double keip_wrap(double x);

<<<<<<< HEAD
int kelvin_wrap(double x, Py_complex *Be, Py_complex *Ke, Py_complex *Bep, Py_complex *Kep);
=======
int kelvin_wrap(double x, npy_cdouble *Be, npy_cdouble *Ke, npy_cdouble *Bep, npy_cdouble *Kep);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

int it1j0y0_wrap(double x, double *, double *);
int it2j0y0_wrap(double x, double *, double *);
int it1i0k0_wrap(double x, double *, double *);
int it2i0k0_wrap(double x, double *, double *);

<<<<<<< HEAD
int cfresnl_wrap(Py_complex x, Py_complex *sf, Py_complex *cf);
=======
int cfresnl_wrap(npy_cdouble x, npy_cdouble *sf, npy_cdouble *cf);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
double cem_cva_wrap(double m, double q);
double sem_cva_wrap(double m, double q);
int cem_wrap(double m, double q, double x, double *csf, double *csd);
int sem_wrap(double m, double q, double x, double *csf, double *csd);
int mcm1_wrap(double m, double q, double x, double *f1r, double *d1r);
int msm1_wrap(double m, double q, double x, double *f1r, double *d1r);
int mcm2_wrap(double m, double q, double x, double *f2r, double *d2r);
int msm2_wrap(double m, double q, double x, double *f2r, double *d2r);
double pmv_wrap(double, double, double);
int pbwa_wrap(double, double, double *, double *);
int pbdv_wrap(double, double, double *, double *);
int pbvv_wrap(double, double, double *, double *);

int prolate_aswfa_wrap(double, double, double, double, double, double *, double *);
int prolate_radial1_wrap(double, double, double, double, double, double *, double *);
int prolate_radial2_wrap(double, double, double, double, double, double *, double *);
int oblate_aswfa_wrap(double, double, double, double, double, double *, double *);
int oblate_radial1_wrap(double, double, double, double, double, double *, double *);
int oblate_radial2_wrap(double, double, double, double, double, double *, double *);
double prolate_aswfa_nocv_wrap(double, double, double, double, double *);
double prolate_radial1_nocv_wrap(double, double, double, double, double *);
double prolate_radial2_nocv_wrap(double, double, double, double, double *);
double oblate_aswfa_nocv_wrap(double, double, double, double, double *);
double oblate_radial1_nocv_wrap(double, double, double, double, double *);
double oblate_radial2_nocv_wrap(double, double, double, double, double *);
double prolate_segv_wrap(double, double, double);
double oblate_segv_wrap(double, double, double);



<<<<<<< HEAD
int modified_fresnel_plus_wrap(double x, Py_complex *F, Py_complex *K);
int modified_fresnel_minus_wrap(double x, Py_complex *F, Py_complex *K);
=======
int modified_fresnel_plus_wrap(double x, npy_cdouble *F, npy_cdouble *K);
int modified_fresnel_minus_wrap(double x, npy_cdouble *F, npy_cdouble *K);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#endif




  







