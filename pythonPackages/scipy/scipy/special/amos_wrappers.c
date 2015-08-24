/* This file is a collection of wrappers around the
 *  Amos Fortran library of functions that take complex
 *  variables (see www.netlib.org) so that they can be called from
 *  the cephes library of corresponding name but work with complex
 *  arguments.
 */

#include "amos_wrappers.h"

#define CADDR(z) (double *)(&((z).real)), (double*)(&((z).imag))
#define F2C_CST(z) (double *)&((z)->real), (double *)&((z)->imag)

#if defined(NO_APPEND_FORTRAN)
#if defined(UPPERCASE_FORTRAN)
#define F_FUNC(f,F) F
#else
#define F_FUNC(f,F) f
#endif
#else
#if defined(UPPERCASE_FORTRAN)
#define F_FUNC(f,F) F##_
#else
#define F_FUNC(f,F) f##_
#endif
#endif

extern int F_FUNC(zairy,ZAIRY)
     (double*, double*, int*, int*, double*, double*, int*, int*);
extern int F_FUNC(zbiry,ZBIRY)
<<<<<<< HEAD
     (double*, double*, int*, int*, double*, double*, int*, int*);
=======
     (double*, double*, int*, int*, double*, double*, int*);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
extern int F_FUNC(zbesi,ZBESI)
     (double*, double*, double*, int*, int*, double*, double*, int*, int*);
extern int F_FUNC(zbesj,ZBESJ)
     (double*, double*, double*, int*, int*, double*, double*, int*, int*);
extern int F_FUNC(zbesk,ZBESK)
     (double*, double*, double*, int*, int*, double*, double*, int*, int*);
extern int F_FUNC(zbesy,ZBESY)
     (double*, double*, double*, int*, int*, double*, double*, int*, double*, double*, int*);
extern int F_FUNC(zbesh,ZBESH)
     (double*, double*, double*, int*, int*, int*, double*, double*, int*, int*);

/* This must be linked with fortran
 */

<<<<<<< HEAD
int ierr_to_mtherr( int nz, int ierr) {
     /* Return mtherr equivalents for ierr values */
  
  if (nz != 0) return UNDERFLOW;

  switch (ierr) {
  case 1:
    return DOMAIN;
  case 2:
    return OVERFLOW;
  case 3:
    return PLOSS;
  case 4:
    return TLOSS;
  case 5:   /* Algorithm termination condition not met */
    return TLOSS;    
=======
int ierr_to_sferr(int nz, int ierr) {
  /* Return sf_error equivalents for ierr values */
  
  if (nz != 0) return SF_ERROR_UNDERFLOW;

  switch (ierr) {
  case 1:
    return SF_ERROR_DOMAIN;
  case 2:
    return SF_ERROR_OVERFLOW;
  case 3:
    return SF_ERROR_LOSS;
  case 4:
    return SF_ERROR_NO_RESULT;
  case 5:   /* Algorithm termination condition not met */
    return SF_ERROR_NO_RESULT;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }
  return -1;
}

<<<<<<< HEAD
void set_nan_if_no_computation_done(Py_complex *v, int ierr) {
=======
void set_nan_if_no_computation_done(npy_cdouble *v, int ierr) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (v != NULL && (ierr == 1 || ierr == 2 || ierr == 4 || ierr == 5)) {
    v->real = NPY_NAN;
    v->imag = NPY_NAN;
  }
}

<<<<<<< HEAD
static Py_complex
rotate(Py_complex z, double v)
{
    Py_complex w;
    double c = cos(v * M_PI);
    double s = sin(v * M_PI);
=======
static double sin_pi(double x)
{
    if (floor(x) == x && fabs(x) < 1e14) {
        /* Return 0 when at exact zero, as long as the floating point number is
         * small enough to distinguish integer points from other points.
         */
        return 0;
    }
    return sin(NPY_PI * x);
}

static double cos_pi(double x)
{
    double x05 = x + 0.5;
    if (floor(x05) == x05 && fabs(x) < 1e14) {
        /* Return 0 when at exact zero, as long as the floating point number is
         * small enough to distinguish integer points from other points.
         */
        return 0;
    }
    return cos(NPY_PI * x);
}

static npy_cdouble
rotate(npy_cdouble z, double v)
{
    npy_cdouble w;
    double c = cos_pi(v);
    double s = sin_pi(v);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    w.real = z.real*c - z.imag*s;
    w.imag = z.real*s + z.imag*c;
    return w;
}

<<<<<<< HEAD
static Py_complex
rotate_jy(Py_complex j, Py_complex y, double v)
{
    Py_complex w;
    double c = cos(v * M_PI);
    double s = sin(v * M_PI);
=======
static npy_cdouble
rotate_jy(npy_cdouble j, npy_cdouble y, double v)
{
    npy_cdouble w;
    double c = cos_pi(v);
    double s = sin_pi(v);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    w.real = j.real * c - y.real * s;
    w.imag = j.imag * c - y.imag * s;
    return w;
}

static int
<<<<<<< HEAD
reflect_jy(Py_complex *jy, double v)
=======
reflect_jy(npy_cdouble *jy, double v)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
{
    /* NB: Y_v may be huge near negative integers -- so handle exact
     *     integers carefully
     */
    int i;
    if (v != floor(v))
        return 0;
    
    i = v - 16384.0 * floor(v / 16384.0);
    if (i & 1) {
        jy->real = -jy->real;
        jy->imag = -jy->imag;
    }
    return 1;
}

static int
<<<<<<< HEAD
reflect_i(Py_complex *ik, double v)
=======
reflect_i(npy_cdouble *ik, double v)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
{
    if (v != floor(v))
        return 0;
    return 1; /* I is symmetric for integer v */
}

<<<<<<< HEAD
static Py_complex
rotate_i(Py_complex i, Py_complex k, double v)
{
    Py_complex w;
    double s = sin(v * M_PI)*(2.0/M_PI);
=======
static npy_cdouble
rotate_i(npy_cdouble i, npy_cdouble k, double v)
{
    npy_cdouble w;
    double s = sin(v * NPY_PI)*(2.0/NPY_PI);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    w.real = i.real + s*k.real;
    w.imag = i.imag + s*k.imag;
    return w;
}

<<<<<<< HEAD
int cairy_wrap(Py_complex z, Py_complex *ai, Py_complex *aip, Py_complex *bi, Py_complex *bip) {
=======
int cephes_airy(double x, double *ai, double *aip, double *bi, double *bip);

int airy_wrap(double x, double *ai, double *aip, double *bi, double *bip)
{
    npy_cdouble z, zai, zaip, zbi, zbip;

    /* For small arguments, use Cephes as it's slightly faster.
     * For large arguments, use AMOS as it's more accurate.
     */
    if (x < -10 || x > 10) {
        z.real = x;
        z.imag = 0;
        cairy_wrap(z, &zai, &zaip, &zbi, &zbip);
        *ai  = zai.real;
        *aip = zaip.real;
        *bi  = zbi.real;
        *bip = zbip.real;
    }
    else {
        cephes_airy(x, ai, aip, bi, bip);
    }
    return 0;
}

int cairy_wrap(npy_cdouble z, npy_cdouble *ai, npy_cdouble *aip, npy_cdouble *bi, npy_cdouble *bip) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int id = 0;
  int ierr = 0;
  int kode = 1;
  int nz;

<<<<<<< HEAD
  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(ai), &nz, &ierr);
  DO_MTHERR("airy:", ai);
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bi), &nz, &ierr);
  DO_MTHERR("airy:", bi);
  
  id = 1;
  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(aip), &nz, &ierr);
  DO_MTHERR("airy:", aip);
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bip), &nz, &ierr);
  DO_MTHERR("airy:", bip);
  return 0;
}

int cairy_wrap_e(Py_complex z, Py_complex *ai, Py_complex *aip, Py_complex *bi, Py_complex *bip) {
=======
  ai->real = NPY_NAN;
  ai->imag = NPY_NAN;
  bi->real = NPY_NAN;
  bi->imag = NPY_NAN;
  aip->real = NPY_NAN;
  aip->imag = NPY_NAN;
  bip->real = NPY_NAN;
  bip->imag = NPY_NAN;

  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(ai), &nz, &ierr);
  DO_SFERR("airy:", ai);
  nz = 0;
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bi), &ierr);
  DO_SFERR("airy:", bi);
  
  id = 1;
  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(aip), &nz, &ierr);
  DO_SFERR("airy:", aip);
  nz = 0;
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bip), &ierr);
  DO_SFERR("airy:", bip);
  return 0;
}

int cairy_wrap_e(npy_cdouble z, npy_cdouble *ai, npy_cdouble *aip, npy_cdouble *bi, npy_cdouble *bip) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int id = 0;
  int kode = 2;        /* Exponential scaling */
  int nz, ierr;

<<<<<<< HEAD
  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(ai), &nz, &ierr);
  DO_MTHERR("airye:", ai);
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bi), &nz, &ierr);
  DO_MTHERR("airye:", bi);
  
  id = 1;
  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(aip), &nz, &ierr);
  DO_MTHERR("airye:", aip);
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bip), &nz, &ierr);
  DO_MTHERR("airye:", bip);
=======
  ai->real = NPY_NAN;
  ai->imag = NPY_NAN;
  bi->real = NPY_NAN;
  bi->imag = NPY_NAN;
  aip->real = NPY_NAN;
  aip->imag = NPY_NAN;
  bip->real = NPY_NAN;
  bip->imag = NPY_NAN;

  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(ai), &nz, &ierr);
  DO_SFERR("airye:", ai);
  nz = 0;
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bi), &ierr);
  DO_SFERR("airye:", bi);
  
  id = 1;
  F_FUNC(zairy,ZAIRY)(CADDR(z), &id, &kode, F2C_CST(aip), &nz, &ierr);
  DO_SFERR("airye:", aip);
  nz = 0;
  F_FUNC(zbiry,ZBIRY)(CADDR(z), &id, &kode, F2C_CST(bip), &ierr);
  DO_SFERR("airye:", bip);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return 0;
}

int cairy_wrap_e_real(double z, double *ai, double *aip, double *bi, double *bip) {
  int id = 0;
  int kode = 2;        /* Exponential scaling */
  int nz, ierr;
<<<<<<< HEAD
  Py_complex cz, cai, caip, cbi, cbip;

=======
  npy_cdouble cz, cai, caip, cbi, cbip;

  cai.real = NPY_NAN;
  cai.imag = NPY_NAN;
  cbi.real = NPY_NAN;
  cbi.imag = NPY_NAN;
  caip.real = NPY_NAN;
  caip.imag = NPY_NAN;
  cbip.real = NPY_NAN;
  cbip.imag = NPY_NAN;
  
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  cz.real = z;
  cz.imag = 0;

  if (z < 0) {
      *ai = NPY_NAN;
  } else {
      F_FUNC(zairy,ZAIRY)(CADDR(cz), &id, &kode, CADDR(cai), &nz, &ierr);
<<<<<<< HEAD
      DO_MTHERR("airye:", &cai);
      *ai = cai.real;
  }
  F_FUNC(zbiry,ZBIRY)(CADDR(cz), &id, &kode, CADDR(cbi), &nz, &ierr);
  DO_MTHERR("airye:", &cbi);
=======
      DO_SFERR("airye:", &cai);
      *ai = cai.real;
  }
  nz = 0;
  F_FUNC(zbiry,ZBIRY)(CADDR(cz), &id, &kode, CADDR(cbi), &ierr);
  DO_SFERR("airye:", &cbi);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  *bi = cbi.real;
  
  id = 1;
  if (z < 0) {
      *aip = NPY_NAN;
  } else {
      F_FUNC(zairy,ZAIRY)(CADDR(cz), &id, &kode, CADDR(caip), &nz, &ierr);
<<<<<<< HEAD
      DO_MTHERR("airye:", &caip);
      *aip = caip.real;
  }
  F_FUNC(zbiry,ZBIRY)(CADDR(cz), &id, &kode, CADDR(cbip), &nz, &ierr);
  DO_MTHERR("airye:", &cbip);
=======
      DO_SFERR("airye:", &caip);
      *aip = caip.real;
  }
  nz = 0;
  F_FUNC(zbiry,ZBIRY)(CADDR(cz), &id, &kode, CADDR(cbip), &ierr);
  DO_SFERR("airye:", &cbip);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  *bip = cbip.real;
  return 0;
}

<<<<<<< HEAD
Py_complex cbesi_wrap( double v, Py_complex z) {
=======
npy_cdouble cbesi_wrap( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 1;
  int sign = 1;
  int nz, ierr;
<<<<<<< HEAD
  Py_complex cy, cy_k;
=======
  npy_cdouble cy, cy_k;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
  cy_k.real = NPY_NAN;
  cy_k.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesi,ZBESI)(CADDR(z), &v,  &kode, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("iv:", &cy);
=======
  DO_SFERR("iv:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (ierr == 2) {
    /* overflow */
    if (z.imag == 0 && (z.real >= 0 || v == floor(v))) {
        if (z.real < 0 && v/2 != floor(v/2))
            cy.real = -NPY_INFINITY;
        else
            cy.real = NPY_INFINITY;
        cy.imag = 0;
    } else {
        cy = cbesi_wrap_e(v*sign, z);
        cy.real *= NPY_INFINITY;
        cy.imag *= NPY_INFINITY;
    }
  }

  if (sign == -1) {
    if (!reflect_i(&cy, v)) {
      F_FUNC(zbesk,ZBESK)(CADDR(z), &v,  &kode, &n, CADDR(cy_k), &nz, &ierr);
<<<<<<< HEAD
      DO_MTHERR("iv(kv):", &cy_k);
=======
      DO_SFERR("iv(kv):", &cy_k);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      cy = rotate_i(cy, cy_k, v);
    }
  }

  return cy;
}

<<<<<<< HEAD
Py_complex cbesi_wrap_e( double v, Py_complex z) {
=======
npy_cdouble cbesi_wrap_e( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 2;
  int sign = 1;
  int nz, ierr;
<<<<<<< HEAD
  Py_complex cy, cy_k;
=======
  npy_cdouble cy, cy_k;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
  cy_k.real = NPY_NAN;
  cy_k.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesi,ZBESI)(CADDR(z), &v,  &kode, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("ive:", &cy);
=======
  DO_SFERR("ive:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (sign == -1) {
    if (!reflect_i(&cy, v)) {
      F_FUNC(zbesk,ZBESK)(CADDR(z), &v,  &kode, &n, CADDR(cy_k), &nz, &ierr);
<<<<<<< HEAD
      DO_MTHERR("ive(kv):", &cy_k);
      /* adjust scaling to match zbesi */
      cy_k = rotate(cy_k, -z.imag/M_PI);
=======
      DO_SFERR("ive(kv):", &cy_k);
      /* adjust scaling to match zbesi */
      cy_k = rotate(cy_k, -z.imag/NPY_PI);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      if (z.real > 0) {
          cy_k.real *= exp(-2*z.real);
          cy_k.imag *= exp(-2*z.real);
      }
      /* v -> -v */
      cy = rotate_i(cy, cy_k, v);
    }
  }

  return cy;
}

double cbesi_wrap_e_real(double v, double z) {
<<<<<<< HEAD
  Py_complex cy, w;
=======
  npy_cdouble cy, w;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (v != floor(v) && z < 0) {
    return NPY_NAN;
  } else {
    w.real = z;
    w.imag = 0;
    cy = cbesi_wrap_e(v, w);
    return cy.real;
  }
}
<<<<<<< HEAD
  
Py_complex cbesj_wrap( double v, Py_complex z) {
=======

npy_cdouble cbesj_wrap( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 1;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy_j, cy_y, cwork;
=======
  npy_cdouble cy_j, cy_y, cwork;

  cy_j.real = NPY_NAN;
  cy_j.imag = NPY_NAN;
  cy_y.real = NPY_NAN;
  cy_y.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesj,ZBESJ)(CADDR(z), &v,  &kode, &n, CADDR(cy_j), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("jv:", &cy_j);
=======
  DO_SFERR("jv:", &cy_j);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (ierr == 2) {
    /* overflow */
    cy_j = cbesj_wrap_e(v, z);
    cy_j.real *= NPY_INFINITY;
    cy_j.imag *= NPY_INFINITY;
  }

  if (sign == -1) {
    if (!reflect_jy(&cy_j, v)) {
      F_FUNC(zbesy,ZBESY)(CADDR(z), &v,  &kode, &n, CADDR(cy_y), &nz, CADDR(cwork), &ierr);
<<<<<<< HEAD
      DO_MTHERR("jv(yv):", &cy_y);
=======
      DO_SFERR("jv(yv):", &cy_y);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      cy_j = rotate_jy(cy_j, cy_y, v);
    }
  }
  return cy_j;
}

<<<<<<< HEAD
Py_complex cbesj_wrap_e( double v, Py_complex z) {
=======
double cephes_jv(double v, double x);

double cbesj_wrap_real(double v, double x)
{
    npy_cdouble z, r;

    if (x < 0 && v != (int)v) {
        sf_error("yv", SF_ERROR_DOMAIN, NULL);
        return NPY_NAN;
    }

    z.real = x;
    z.imag = 0;
    r = cbesj_wrap(v, z);
    if (r.real != r.real) {
        /* AMOS returned NaN, possibly due to overflow */
        return cephes_jv(v, x);
    }
    return r.real;
}

npy_cdouble cbesj_wrap_e( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 2;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy_j, cy_y, cwork;
=======
  npy_cdouble cy_j, cy_y, cwork;

  cy_j.real = NPY_NAN;
  cy_j.imag = NPY_NAN;
  cy_y.real = NPY_NAN;
  cy_y.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesj,ZBESJ)(CADDR(z), &v, &kode, &n, CADDR(cy_j), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("jve:", &cy_j);
  if (sign == -1) {
    if (!reflect_jy(&cy_j, v)) {
      F_FUNC(zbesy,ZBESY)(CADDR(z), &v,  &kode, &n, CADDR(cy_y), &nz, CADDR(cwork), &ierr);
      DO_MTHERR("jve(yve):", &cy_y);
=======
  DO_SFERR("jve:", &cy_j);
  if (sign == -1) {
    if (!reflect_jy(&cy_j, v)) {
      F_FUNC(zbesy,ZBESY)(CADDR(z), &v,  &kode, &n, CADDR(cy_y), &nz, CADDR(cwork), &ierr);
      DO_SFERR("jve(yve):", &cy_y);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      cy_j = rotate_jy(cy_j, cy_y, v);
    }
  }
  return cy_j;
}

double cbesj_wrap_e_real(double v, double z) {
<<<<<<< HEAD
  Py_complex cy, w;
=======
  npy_cdouble cy, w;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (v != floor(v) && z < 0) {
    return NPY_NAN;
  } else {
    w.real = z;
    w.imag = 0;
    cy = cbesj_wrap_e(v, w);
    return cy.real;
  }
}
  
<<<<<<< HEAD
Py_complex cbesy_wrap( double v, Py_complex z) {
=======
npy_cdouble cbesy_wrap( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 1;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy_y, cy_j, cwork;
=======
  npy_cdouble cy_y, cy_j, cwork;

  cy_j.real = NPY_NAN;
  cy_j.imag = NPY_NAN;
  cy_y.real = NPY_NAN;
  cy_y.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
<<<<<<< HEAD
  F_FUNC(zbesy,ZBESY)(CADDR(z), &v,  &kode, &n, CADDR(cy_y), &nz, CADDR(cwork), &ierr);
  DO_MTHERR("yv:", &cy_y);
  if (ierr == 2) {
    if (z.real >= 0 && z.imag == 0) {
      /* overflow */
      cy_y.real = NPY_INFINITY;
      cy_y.imag = 0;
    }
=======

  if (z.real == 0 && z.imag == 0) {
      /* overflow */
      cy_y.real = -NPY_INFINITY;
      cy_y.imag = 0;
      sf_error("yv", SF_ERROR_OVERFLOW, NULL);
  }
  else {
      F_FUNC(zbesy,ZBESY)(CADDR(z), &v,  &kode, &n, CADDR(cy_y), &nz, CADDR(cwork), &ierr);
      DO_SFERR("yv:", &cy_y);
      if (ierr == 2) {
          if (z.real >= 0 && z.imag == 0) {
              /* overflow */
              cy_y.real = -NPY_INFINITY;
              cy_y.imag = 0;
          }
      }
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }

  if (sign == -1) {
    if (!reflect_jy(&cy_y, v)) {
      F_FUNC(zbesj,ZBESJ)(CADDR(z), &v,  &kode, &n, CADDR(cy_j), &nz, &ierr);
<<<<<<< HEAD
      DO_MTHERR("yv(jv):", &cy_j);
=======
      DO_SFERR("yv(jv):", &cy_j);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      cy_y = rotate_jy(cy_y, cy_j, -v);
    }
  }
  return cy_y;
}

<<<<<<< HEAD
Py_complex cbesy_wrap_e( double v, Py_complex z) {
=======
double cephes_yv(double v, double x);

double cbesy_wrap_real(double v, double x)
{
    npy_cdouble z, r;

    if (x < 0.0) {
        sf_error("yv", SF_ERROR_DOMAIN, NULL);
        return NPY_NAN;
    }

    z.real = x;
    z.imag = 0;
    r = cbesy_wrap(v, z);
    if (r.real != r.real) {
        /* AMOS returned NaN, possibly due to overflow */
        return cephes_yv(v, x);
    }
    return r.real;
}

npy_cdouble cbesy_wrap_e( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 2;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy_y, cy_j, cwork;
=======
  npy_cdouble cy_y, cy_j, cwork;

  cy_j.real = NPY_NAN;
  cy_j.imag = NPY_NAN;
  cy_y.real = NPY_NAN;
  cy_y.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesy,ZBESY)(CADDR(z), &v, &kode, &n, CADDR(cy_y), &nz, CADDR(cwork), &ierr);
<<<<<<< HEAD
  DO_MTHERR("yve:", &cy_y);
=======
  DO_SFERR("yve:", &cy_y);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (ierr == 2) {
    if (z.real >= 0 && z.imag == 0) {
      /* overflow */
      cy_y.real = NPY_INFINITY;
      cy_y.imag = 0;
    }
  }

  if (sign == -1) {
    if (!reflect_jy(&cy_y, v)) {
      F_FUNC(zbesj,ZBESJ)(CADDR(z), &v,  &kode, &n, CADDR(cy_j), &nz, &ierr);
<<<<<<< HEAD
      DO_MTHERR("yv(jv):", &cy_j);
=======
      DO_SFERR("yv(jv):", &cy_j);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      cy_y = rotate_jy(cy_y, cy_j, -v);
    }
  }
  return cy_y;
}

double cbesy_wrap_e_real(double v, double z) {
<<<<<<< HEAD
  Py_complex cy, w;
=======
  npy_cdouble cy, w;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (z < 0) {
    return NPY_NAN;
  } else {
    w.real = z;
    w.imag = 0;
    cy = cbesy_wrap_e(v, w);
    return cy.real;
  }
}
  
<<<<<<< HEAD
Py_complex cbesk_wrap( double v, Py_complex z) {
  int n = 1;
  int kode = 1;
  int nz, ierr;
  Py_complex cy;
=======
npy_cdouble cbesk_wrap( double v, npy_cdouble z) {
  int n = 1;
  int kode = 1;
  int nz, ierr;
  npy_cdouble cy;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    /* K_v == K_{-v} even for non-integer v */
    v = -v;
  }
  F_FUNC(zbesk,ZBESK)(CADDR(z), &v,  &kode, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("kv:", &cy);
=======
  DO_SFERR("kv:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (ierr == 2) {
    if (z.real >= 0 && z.imag == 0) {
      /* overflow */
      cy.real = NPY_INFINITY;
      cy.imag = 0;
    }
  }

  return cy;
}

<<<<<<< HEAD
Py_complex cbesk_wrap_e( double v, Py_complex z) {
  int n = 1;
  int kode = 2;
  int nz, ierr;
  Py_complex cy;
=======
npy_cdouble cbesk_wrap_e( double v, npy_cdouble z) {
  int n = 1;
  int kode = 2;
  int nz, ierr;
  npy_cdouble cy;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    /* K_v == K_{-v} even for non-integer v */
    v = -v;
  }
  F_FUNC(zbesk,ZBESK)(CADDR(z), &v, &kode, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("kve:", &cy);
=======
  DO_SFERR("kve:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (ierr == 2) {
    if (z.real >= 0 && z.imag == 0) {
      /* overflow */
      cy.real = NPY_INFINITY;
      cy.imag = 0;
    }
  }

  return cy;
}
  
double cbesk_wrap_real( double v, double z) {
<<<<<<< HEAD
  Py_complex cy, w;
  if (z < 0) {
    return NPY_NAN;
  } else {
=======
  npy_cdouble cy, w;
  if (z < 0) {
    return NPY_NAN;
  }
  else if (z > 710 * (1 + fabs(v))) {
      /* Underflow. See uniform expansion http://dlmf.nist.gov/10.41
       * This condition is not a strict bound (it can underflow earlier),
       * rather, we are here working around a restriction in AMOS.
       */
      return 0;
  }
  else {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    w.real = z;
    w.imag = 0;
    cy = cbesk_wrap(v, w);
    return cy.real;
  }
}

<<<<<<< HEAD
double cbesk_wrap_e_real( double v, double z) {
  Py_complex cy, w;
=======
double cbesk_wrap_real_int(int n, double z)
{
    return cbesk_wrap_real(n, z);
}

double cbesk_wrap_e_real( double v, double z) {
  npy_cdouble cy, w;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (z < 0) {
    return NPY_NAN;
  } else {
    w.real = z;
    w.imag = 0;
    cy = cbesk_wrap_e(v, w);
    return cy.real;
  }
}
  
<<<<<<< HEAD
Py_complex cbesh_wrap1( double v, Py_complex z) {
=======
npy_cdouble cbesh_wrap1( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 1;
  int m = 1;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy;
=======
  npy_cdouble cy;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesh,ZBESH)(CADDR(z), &v,  &kode, &m, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("hankel1:", &cy);
=======
  DO_SFERR("hankel1:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (sign == -1) {
    cy = rotate(cy, v);
  }
  return cy;
}

<<<<<<< HEAD
Py_complex cbesh_wrap1_e( double v, Py_complex z) {
=======
npy_cdouble cbesh_wrap1_e( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 2;
  int m = 1;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy;
=======
  npy_cdouble cy;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesh,ZBESH)(CADDR(z), &v, &kode, &m, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("hankel1e:", &cy);
=======
  DO_SFERR("hankel1e:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (sign == -1) {
    cy = rotate(cy, v);
  }
  return cy;
}
  
<<<<<<< HEAD
Py_complex cbesh_wrap2( double v, Py_complex z) {
=======
npy_cdouble cbesh_wrap2( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 1;
  int m = 2;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy;
=======
  npy_cdouble cy;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesh,ZBESH)(CADDR(z), &v, &kode, &m, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("hankel2:", &cy);
=======
  DO_SFERR("hankel2:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (sign == -1) {
    cy = rotate(cy, -v);
  }
  return cy;
}

<<<<<<< HEAD
Py_complex cbesh_wrap2_e( double v, Py_complex z) {
=======
npy_cdouble cbesh_wrap2_e( double v, npy_cdouble z) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int n = 1;
  int kode = 2;
  int m = 2;
  int nz, ierr;
  int sign = 1;
<<<<<<< HEAD
  Py_complex cy;
=======
  npy_cdouble cy;

  cy.real = NPY_NAN;
  cy.imag = NPY_NAN;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (v < 0) {
    v = -v;
    sign = -1;
  }
  F_FUNC(zbesh,ZBESH)(CADDR(z), &v, &kode, &m, &n, CADDR(cy), &nz, &ierr);
<<<<<<< HEAD
  DO_MTHERR("hankel2e:", &cy);
=======
  DO_SFERR("hankel2e:", &cy);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (sign == -1) {
    cy = rotate(cy, -v);
  }
  return cy;
}
