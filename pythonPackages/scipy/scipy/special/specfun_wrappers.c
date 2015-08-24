/* This file is a collection of wrappers around the
 *  Specfun Fortran library of functions 
 */

#include "specfun_wrappers.h"
<<<<<<< HEAD
#include <stdio.h>
=======

#include "sf_error.h"
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

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

extern double cephes_psi(double);
extern double cephes_struve(double, double);

extern void F_FUNC(cgama,CGAMA)(double*,double*,int*,double*,double*);
extern void F_FUNC(cpsi,CPSI)(double*,double*,double*,double*);
<<<<<<< HEAD
extern void F_FUNC(hygfz,HYGFZ)(double*,double*,double*,Py_complex*,Py_complex*);
extern void F_FUNC(cchg,CCHG)(double*,double*,Py_complex*,Py_complex*);
=======
extern void F_FUNC(hygfz,HYGFZ)(double*,double*,double*,npy_cdouble*,npy_cdouble*);
extern void F_FUNC(cchg,CCHG)(double*,double*,npy_cdouble*,npy_cdouble*);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
extern void F_FUNC(chgm,CHGM)(double*,double*,double*,double*);
extern void F_FUNC(chgu,CHGU)(double*,double*,double*,double*,int*);
extern void F_FUNC(itairy,ITAIRY)(double*,double*,double*,double*,double*);
extern void F_FUNC(e1xb,E1XB)(double*,double*);
<<<<<<< HEAD
extern void F_FUNC(e1z,E1Z)(Py_complex*,Py_complex*);
extern void F_FUNC(eix,EIX)(double*,double*);
extern void F_FUNC(cerror,CERROR)(Py_complex*,Py_complex*);
=======
extern void F_FUNC(e1z,E1Z)(npy_cdouble*,npy_cdouble*);
extern void F_FUNC(eix,EIX)(double*,double*);
extern void F_FUNC(cerror,CERROR)(npy_cdouble*,npy_cdouble*);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
extern void F_FUNC(stvh0,STVH0)(double*,double*);
extern void F_FUNC(stvh1,STVH1)(double*,double*);
extern void F_FUNC(stvhv,STVHV)(double*,double*,double*);
extern void F_FUNC(stvl0,STVL0)(double*,double*);
extern void F_FUNC(stvl1,STVL1)(double*,double*);
extern void F_FUNC(stvlv,STVLV)(double*,double*,double*);
extern void F_FUNC(itsh0,ITSH0)(double*,double*);
extern void F_FUNC(itth0,ITTH0)(double*,double*);
extern void F_FUNC(itsl0,ITSL0)(double*,double*);
extern void F_FUNC(klvna,KLVNA)(double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern void F_FUNC(itjya,ITJYA)(double*,double*,double*);
extern void F_FUNC(ittjya,ITTJYA)(double*,double*,double*);
extern void F_FUNC(itika,ITIKA)(double*,double*,double*);
extern void F_FUNC(ittika,ITTIKA)(double*,double*,double*);
<<<<<<< HEAD
extern void F_FUNC(cfc,CFC)(Py_complex*,Py_complex*,Py_complex*);
extern void F_FUNC(cfs,CFS)(Py_complex*,Py_complex*,Py_complex*);
=======
extern void F_FUNC(cfc,CFC)(npy_cdouble*,npy_cdouble*,npy_cdouble*);
extern void F_FUNC(cfs,CFS)(npy_cdouble*,npy_cdouble*,npy_cdouble*);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
extern void F_FUNC(cva2,CVA2)(int*,int*,double*,double*);
extern void F_FUNC(mtu0,MTU0)(int*,int*,double*,double*,double*,double*);
extern void F_FUNC(mtu12,MTU12)(int*,int*,int*,double*,double*,double*,double*,double*,double*);
extern void F_FUNC(lpmv,LPMV)(double*,int*,double*,double*);
extern void F_FUNC(pbwa,PBWA)(double*,double*,double*,double*,double*,double*);
extern void F_FUNC(pbdv,PBDV)(double*,double*,double*,double*,double*,double*);
extern void F_FUNC(pbvv,PBVV)(double*,double*,double*,double*,double*,double*);
extern void F_FUNC(segv,SEGV)(int*,int*,double*,int*,double*,double*);
extern void F_FUNC(aswfa,ASWFA)(int*,int*,double*,double*,int*,double*,double*,double*);
extern void F_FUNC(rswfp,RSWFP)(int*,int*,double*,double*,double*,int*,double*,double*,double*,double*);
extern void F_FUNC(rswfo,RSWFO)(int*,int*,double*,double*,double*,int*,double*,double*,double*,double*);
extern void F_FUNC(ffk,FFK)(int*,double*,double*,double*,double*,double*,double*,double*,double*,double*);


/* This must be linked with fortran
 */

<<<<<<< HEAD
Py_complex cgamma_wrap( Py_complex z) {
  int kf = 1;
  Py_complex cy;
=======
npy_cdouble cgamma_wrap( npy_cdouble z) {
  int kf = 1;
  npy_cdouble cy;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  F_FUNC(cgama,CGAMA)(CADDR(z), &kf, CADDR(cy));
  return cy;
}

<<<<<<< HEAD
Py_complex clngamma_wrap( Py_complex z) {
  int kf = 0;
  Py_complex cy;
=======
npy_cdouble clngamma_wrap( npy_cdouble z) {
  int kf = 0;
  npy_cdouble cy;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  F_FUNC(cgama,CGAMA)(CADDR(z), &kf, CADDR(cy));
  return cy;
}

<<<<<<< HEAD
Py_complex cpsi_wrap( Py_complex z) {
  Py_complex cy;
=======
npy_cdouble cpsi_wrap( npy_cdouble z) {
  npy_cdouble cy;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  
  if (IMAG(z)==0.0) {
    REAL(cy) = cephes_psi(REAL(z));
    IMAG(cy) = 0.0;
  }
  else {
    F_FUNC(cpsi,CPSI)(CADDR(z), CADDR(cy));
  }
  return cy;
}

<<<<<<< HEAD
Py_complex crgamma_wrap( Py_complex z) {
  int kf = 1;
  Py_complex cy;
  Py_complex cy2;
=======
npy_cdouble crgamma_wrap( npy_cdouble z) {
  int kf = 1;
  npy_cdouble cy;
  npy_cdouble cy2;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  double magsq;

  F_FUNC(cgama,CGAMA)(CADDR(z), &kf, CADDR(cy));
  magsq = ABSQ(cy);
  REAL(cy2) = REAL(cy) / magsq;
  IMAG(cy2) = -IMAG(cy) / magsq;
  return cy2;
}

<<<<<<< HEAD
Py_complex chyp2f1_wrap( double a, double b, double c, Py_complex z) {
  Py_complex outz;
=======
npy_cdouble chyp2f1_wrap( double a, double b, double c, npy_cdouble z) {
  npy_cdouble outz;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int l1, l0;
 
 
  l0 = ((c == floor(c)) && (c < 0));
  l1 = ((fabs(1-REAL(z)) < 1e-15) && (IMAG(z) == 0) && (c-a-b <= 0));
  if (l0 || l1) {
<<<<<<< HEAD
=======
    sf_error("chyp2f1", SF_ERROR_OVERFLOW, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    REAL(outz) = NPY_INFINITY;
    IMAG(outz) = 0.0;
    return outz;
  }
  F_FUNC(hygfz, HYGFZ)(&a, &b, &c, &z, &outz);
  return outz;
}

<<<<<<< HEAD
Py_complex chyp1f1_wrap(double a, double b, Py_complex z) {
  Py_complex outz;

  F_FUNC(cchg,CCHG)(&a, &b, &z, &outz);
  if (REAL(outz) == 1e300) {
=======
npy_cdouble chyp1f1_wrap(double a, double b, npy_cdouble z) {
  npy_cdouble outz;

  F_FUNC(cchg,CCHG)(&a, &b, &z, &outz);
  if (REAL(outz) == 1e300) {
    sf_error("chyp1f1", SF_ERROR_OVERFLOW, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    REAL(outz) = NPY_INFINITY;
  }
  return outz;
}


double hypU_wrap(double a, double b, double x) {
  double out;
  int md; /* method code --- not returned */

  F_FUNC(chgu,CHGU)(&a, &b, &x, &out, &md);
<<<<<<< HEAD
  if (out == 1e300) out = NPY_INFINITY;
=======
  if (out == 1e300) {
      sf_error("hypU", SF_ERROR_OVERFLOW, NULL);
      out = NPY_INFINITY;
  }
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return out;
  
}

double hyp1f1_wrap(double a, double b, double x) {
   double outy;
 
   F_FUNC(chgm,CHGM)(&a, &b, &x, &outy);
   if (outy == 1e300) {
<<<<<<< HEAD
=======
     sf_error("hyp1f1", SF_ERROR_OVERFLOW, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
     outy = NPY_INFINITY;
   }
   return outy;
}

int itairy_wrap(double x, double *apt, double *bpt, double *ant, double *bnt) {
  double tmp; 
  int flag = 0;
  
  if (x < 0) {
    x = -x;
    flag = 1;
  }
  F_FUNC(itairy,ITAIRY)(&x, apt, bpt, ant, bnt);
  if (flag) {  /* negative limit -- switch signs and roles */
    tmp = *apt;
    *apt = -*ant;
    *ant = -tmp;
    tmp = *bpt;
    *bpt = -*bnt;
    *bnt = -tmp;
  }
  return 0;
}


double exp1_wrap(double x) {
  double out;
  
  F_FUNC(e1xb,E1XB)(&x, &out);
<<<<<<< HEAD
  CONVINF(out);
  return out;
}

Py_complex cexp1_wrap(Py_complex z) {
  Py_complex outz;
  
  F_FUNC(e1z,E1Z)(&z, &outz);
  ZCONVINF(outz);
=======
  CONVINF("exp1", out);
  return out;
}

npy_cdouble cexp1_wrap(npy_cdouble z) {
  npy_cdouble outz;
  
  F_FUNC(e1z,E1Z)(&z, &outz);
  ZCONVINF("cexp1", outz);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return outz;
}

double expi_wrap(double x) {
  double out;
  
  F_FUNC(eix,EIX)(&x, &out);
<<<<<<< HEAD
  CONVINF(out);
  return out;
}

Py_complex cexpi_wrap(Py_complex z) {
  Py_complex outz;

  F_FUNC(eixz,EIXZ)(&z, &outz);
  ZCONVINF(outz);
  return outz;
}

Py_complex cerf_wrap(Py_complex z) {
  Py_complex outz;
=======
  CONVINF("expi", out);
  return out;
}

npy_cdouble cexpi_wrap(npy_cdouble z) {
  npy_cdouble outz;

  F_FUNC(eixz,EIXZ)(&z, &outz);
  ZCONVINF("cexpi", outz);
  return outz;
}

npy_cdouble cerf_wrap(npy_cdouble z) {
  npy_cdouble outz;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  
  F_FUNC(cerror,CERROR)(&z, &outz);
  return outz;
}

double struve_wrap(double v, double x) {
  double out;
  double rem;
  int flag=0;

  if (x < 0) {
      rem = fmod(v, 2.0);
      if (rem == 0) {
          x = -x;
          flag = 1;
      } else if (rem == 1 || rem == -1) {
          x = -x;
          flag = 0;
      } else {
          /* non-integer v and x < 0 => complex-valued */
          return NPY_NAN;
      }
  }

  if ((v<-8.0) || (v>12.5)) {
    out = cephes_struve(v, x);  /* from cephes */
  }
  else if (v==0.0) {
    F_FUNC(stvh0,STVH0)(&x,&out);
<<<<<<< HEAD
    CONVINF(out);
  }
  else if (v==1.0) {
    F_FUNC(stvh1,STVH1)(&x,&out);
    CONVINF(out);
  }
  else {
    F_FUNC(stvhv,STVHV)(&v,&x,&out);
    CONVINF(out);
=======
    CONVINF("struve", out);
  }
  else if (v==1.0) {
    F_FUNC(stvh1,STVH1)(&x,&out);
    CONVINF("struve", out);
  }
  else {
    F_FUNC(stvhv,STVHV)(&v,&x,&out);
    CONVINF("struve", out);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }
  if (flag) out = -out;
  return out;
}

double modstruve_wrap(double v, double x) {
  double out;
  int flag=0;

  if ((x < 0) & (floor(v)!=v)) return NPY_NAN;
  if (v==0.0) {
    if (x < 0) {x = -x; flag=1;}
    F_FUNC(stvl0,STVL0)(&x,&out);
<<<<<<< HEAD
    CONVINF(out);
=======
    CONVINF("modstruve", out);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if (flag) out = -out;
    return out;
  }
  if (v==1.0) {
    if (x < 0) x=-x;
    F_FUNC(stvl1,STVL1)(&x,&out);
<<<<<<< HEAD
    CONVINF(out);
=======
    CONVINF("modstruve", out);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return out;
  }
  if (x<0) {
    x = -x;
    flag = 1;
  }
  F_FUNC(stvlv,STVLV)(&v,&x,&out);
<<<<<<< HEAD
  CONVINF(out);
=======
  CONVINF("modstruve", out);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (flag && (!((int)floor(v) % 2))) out = -out;
  return out;  
}

double itstruve0_wrap(double x) {
  double out;

  if (x<0) x=-x;
  F_FUNC(itsh0,ITSH0)(&x,&out);
<<<<<<< HEAD
  CONVINF(out);
=======
  CONVINF("itstruve0", out);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return out;
}

double it2struve0_wrap(double x) {
  double out;
  int flag=0;
  
  if (x<0) {x=-x; flag=1;}
  F_FUNC(itth0,ITTH0)(&x,&out);
<<<<<<< HEAD
  CONVINF(out);
  if (flag) {
    out = PI - out;
=======
  CONVINF("it2struve0", out);
  if (flag) {
    out = NPY_PI - out;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }
  return out;
}

double itmodstruve0_wrap(double x) {
  double out;

  if (x<0) x=-x;
  F_FUNC(itsl0,ITSL0)(&x,&out);
<<<<<<< HEAD
  CONVINF(out);
=======
  CONVINF("itmodstruve0", out);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return out;
}


double ber_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;

  if (x<0) x=-x;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF(Be);
=======
  npy_cdouble Be, Ke, Bep, Kep;

  if (x<0) x=-x;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF("ber", Be);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return REAL(Be);
}

double bei_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;

  if (x<0) x=-x;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF(Be);
=======
  npy_cdouble Be, Ke, Bep, Kep;

  if (x<0) x=-x;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF("bei", Be);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return IMAG(Be);
}

double ker_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF(Ke);
=======
  npy_cdouble Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF("ker", Ke);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return REAL(Ke);  
}

double kei_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF(Ke);
=======
  npy_cdouble Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF("kei", Ke);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return IMAG(Ke);  
}

double berp_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;
=======
  npy_cdouble Be, Ke, Bep, Kep;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int flag = 0;

  if (x<0) {x=-x; flag=1;}
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
<<<<<<< HEAD
  ZCONVINF(Bep);
=======
  ZCONVINF("berp", Bep);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (flag) return -REAL(Bep);
  return REAL(Bep);
}

double beip_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;
=======
  npy_cdouble Be, Ke, Bep, Kep;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int flag = 0;

  if (x<0) {x=-x; flag=1;}
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
<<<<<<< HEAD
  ZCONVINF(Bep);
=======
  ZCONVINF("beip", Bep);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (flag) return -IMAG(Bep);
  return IMAG(Bep);
}

double kerp_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF(Kep);
=======
  npy_cdouble Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF("kerp", Kep);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return REAL(Kep);  
}

double keip_wrap(double x)
{
<<<<<<< HEAD
  Py_complex Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF(Kep);
=======
  npy_cdouble Be, Ke, Bep, Kep;

  if (x<0) return NPY_NAN;
  F_FUNC(klvna,KLVNA)(&x, CADDR(Be), CADDR(Ke), CADDR(Bep), CADDR(Kep));
  ZCONVINF("keip", Kep);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return IMAG(Kep);  
}


<<<<<<< HEAD
int kelvin_wrap(double x, Py_complex *Be, Py_complex *Ke, Py_complex *Bep, Py_complex *Kep) {
=======
int kelvin_wrap(double x, npy_cdouble *Be, npy_cdouble *Ke, npy_cdouble *Bep, npy_cdouble *Kep) {
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int flag = 0;
  
  if (x<0) {x=-x; flag=1;}
  F_FUNC(klvna,KLVNA)(&x, F2C_CST(Be), F2C_CST(Ke), F2C_CST(Bep), F2C_CST(Kep));
<<<<<<< HEAD
  ZCONVINF(*Be);
  ZCONVINF(*Ke);
  ZCONVINF(*Bep);
  ZCONVINF(*Kep);
=======
  ZCONVINF("klvna", *Be);
  ZCONVINF("klvna", *Ke);
  ZCONVINF("klvna", *Bep);
  ZCONVINF("klvna", *Kep);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  if (flag) {
    REAL(*Bep) = -REAL(*Bep);
    IMAG(*Bep) = -IMAG(*Bep);
    REAL(*Ke) = NPY_NAN;
    IMAG(*Ke) = NPY_NAN;
    REAL(*Kep) = NPY_NAN;
    IMAG(*Kep) = NPY_NAN;    
  }    
  return 0;
}

/* Integrals of bessel functions */

/* int(j0(t),t=0..x) */
/* int(y0(t),t=0..x) */

int it1j0y0_wrap(double x, double *j0int, double *y0int)
{
  int flag = 0;

  if (x < 0) {x = -x; flag=1;}
  F_FUNC(itjya, ITJYA)(&x, j0int, y0int);
  if (flag) {
    *j0int = -(*j0int);
    *y0int = NPY_NAN;    /* domain error */
  }
  return 0;
}

/* int((1-j0(t))/t,t=0..x) */
/* int(y0(t)/t,t=x..inf) */

int it2j0y0_wrap(double x, double *j0int, double *y0int) 
{
  int flag = 0;

  if (x < 0) {x=-x; flag=1;}
  F_FUNC(ittjya, ITTJYA)(&x, j0int, y0int);
  if (flag) {
    *y0int = NPY_NAN;  /* domain error */
  }
  return 0;
}

/* Integrals of modified bessel functions */

int it1i0k0_wrap(double x, double *i0int, double *k0int)
{
  int flag = 0;

  if (x < 0) {x = -x; flag=1;}
  F_FUNC(itika, ITIKA)(&x, i0int, k0int);
  if (flag) {
    *i0int = -(*i0int);
    *k0int = NPY_NAN;    /* domain error */
  }
  return 0;
}

int it2i0k0_wrap(double x, double *i0int, double *k0int) 
{
  int flag = 0;

  if (x < 0) {x=-x; flag=1;}
  F_FUNC(ittika, ITTIKA)(&x, i0int, k0int);
  if (flag) {
    *k0int = NPY_NAN;  /* domain error */
  }
  return 0;
}


/* Fresnel integrals of complex numbers */

<<<<<<< HEAD
int cfresnl_wrap(Py_complex z, Py_complex *zfs, Py_complex *zfc)
{
  Py_complex zfd;
=======
int cfresnl_wrap(npy_cdouble z, npy_cdouble *zfs, npy_cdouble *zfc)
{
  npy_cdouble zfd;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  F_FUNC(cfs,CFS)(&z,zfs,&zfd);
  F_FUNC(cfc,CFC)(&z,zfc,&zfd); 
  return 0;
}

/* Mathieu functions */
/* Characteristic values */
double cem_cva_wrap(double m, double q) {
  int int_m, kd=1;
  double out;

<<<<<<< HEAD
  if ((m < 0) || (m != floor(m))) 
    return NPY_NAN;
  int_m = (int )m;
  if (int_m % 2) kd=2;
  F_FUNC(cva2,CVA2)(&kd, &int_m, &q, &out);
  return out;               
=======
  if ((m < 0) || (m != floor(m))) {
    sf_error("cem_cva", SF_ERROR_DOMAIN, NULL);
    return NPY_NAN;
  }
  int_m = (int )m;
  if (q < 0) {
    /* http://dlmf.nist.gov/28.2#E26 */
    if (int_m % 2 == 0) {
      return cem_cva_wrap(m, -q);
    }
    else {
      return sem_cva_wrap(m, -q);
    }
  }

  if (int_m % 2) kd=2;
  F_FUNC(cva2,CVA2)(&kd, &int_m, &q, &out);
  return out;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}

double sem_cva_wrap(double m, double q) {
  int int_m, kd=4;
  double out;

<<<<<<< HEAD
  if ((m < 1) || (m != floor(m))) 
    return NPY_NAN;
  int_m = (int )m;
  if (int_m % 2) kd=3;
  F_FUNC(cva2,CVA2)(&kd, &int_m, &q, &out);
  return out;               
=======
  if ((m <= 0) || (m != floor(m))) {
    sf_error("cem_cva", SF_ERROR_DOMAIN, NULL);
    return NPY_NAN;
  }
  int_m = (int)m;
  if (q < 0) {
    /* http://dlmf.nist.gov/28.2#E26 */
    if (int_m % 2 == 0) {
      return sem_cva_wrap(m, -q);
    }
    else {
      return cem_cva_wrap(m, -q);
    }
  }
  if (int_m % 2) kd=3;
  F_FUNC(cva2,CVA2)(&kd, &int_m, &q, &out);
  return out;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}

/* Mathieu functions */
int cem_wrap(double m, double q, double x, double *csf, double *csd)
{
<<<<<<< HEAD
  int int_m, kf=1;
  if ((m < 1) || (m != floor(m)) || (q<0)) {
    *csf = NPY_NAN;
    *csd = NPY_NAN;
  }
  int_m = (int )m;
=======
  int int_m, kf=1, sgn;
  double f, d;
  if ((m < 0) || (m != floor(m))) {
    *csf = NPY_NAN;
    *csd = NPY_NAN;
    sf_error("cem", SF_ERROR_DOMAIN, NULL);
    return -1;
  }
  int_m = (int)m;
  if (q < 0) {
      /* http://dlmf.nist.gov/28.2#E34 */
      if (int_m % 2 == 0) {
        sgn = ((int_m/2) % 2 == 0) ? 1 : -1;
        cem_wrap(m, -q, 90 - x, &f, &d);
        *csf =  sgn * f;
        *csd = -sgn * d;
        return 0;
      }
      else {
        sgn = ((int_m/2) % 2 == 0) ? 1 : -1;
        sem_wrap(m, -q, 90 - x, &f, &d);
        *csf =  sgn * f;
        *csd = -sgn * d;
        return 0;
      }
  }
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  F_FUNC(mtu0,MTU0)(&kf,&int_m, &q, &x, csf, csd);
  return 0;  
}

int sem_wrap(double m, double q, double x, double *csf, double *csd)
{
<<<<<<< HEAD
  int int_m, kf=2;
  if ((m < 1) || (m != floor(m)) || (q<0)) {
    *csf = NPY_NAN;
    *csd = NPY_NAN;
  }
  int_m = (int )m;
=======
  int int_m, kf=2, sgn;
  double f, d;
  if ((m < 0) || (m != floor(m))) {
    *csf = NPY_NAN;
    *csd = NPY_NAN;
    sf_error("sem", SF_ERROR_DOMAIN, NULL);
    return -1;
  }
  int_m = (int)m;
  if (int_m == 0) {
    *csf = 0;
    *csd = 0;
    return 0;
  }
  if (q < 0) {
      /* http://dlmf.nist.gov/28.2#E34 */
      if (int_m % 2 == 0) {
        sgn = ((int_m/2) % 2 == 0) ? -1 : 1;
        sem_wrap(m, -q, 90 - x, &f, &d);
        *csf =  sgn * f;
        *csd = -sgn * d;
        return 0;
      }
      else {
        sgn = ((int_m/2) % 2 == 0) ? 1 : -1;
        cem_wrap(m, -q, 90 - x, &f, &d);
        *csf =  sgn * f;
        *csd = -sgn * d;
        return 0;
      }
  }
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  F_FUNC(mtu0,MTU0)(&kf,&int_m, &q, &x, csf, csd);
  return 0;  
}


int mcm1_wrap(double m, double q, double x, double *f1r, double *d1r)
{
  int int_m, kf=1, kc=1;
  double f2r, d2r;

<<<<<<< HEAD
  if ((m < 1) || (m != floor(m)) || (q<0)) {
    *f1r = NPY_NAN;
    *d1r = NPY_NAN;
=======
  if ((m < 0) || (m != floor(m)) || (q<0)) {
    *f1r = NPY_NAN;
    *d1r = NPY_NAN;
    sf_error("mcm1", SF_ERROR_DOMAIN, NULL);
    return -1;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }
  int_m = (int )m;
  F_FUNC(mtu12,MTU12)(&kf,&kc,&int_m, &q, &x, f1r, d1r, &f2r, &d2r);
  return 0;  
}

int msm1_wrap(double m, double q, double x, double *f1r, double *d1r)
{
  int int_m, kf=2, kc=1;
  double f2r, d2r;

  if ((m < 1) || (m != floor(m)) || (q<0)) {
    *f1r = NPY_NAN;
    *d1r = NPY_NAN;
<<<<<<< HEAD
=======
    sf_error("msm1", SF_ERROR_DOMAIN, NULL);
    return -1;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }
  int_m = (int )m;
  F_FUNC(mtu12,MTU12)(&kf,&kc,&int_m, &q, &x, f1r, d1r, &f2r, &d2r);
  return 0;  
}

int mcm2_wrap(double m, double q, double x, double *f2r, double *d2r)
{
  int int_m, kf=1, kc=2;
  double f1r, d1r;

<<<<<<< HEAD
  if ((m < 1) || (m != floor(m)) || (q<0)) {
    *f2r = NPY_NAN;
    *d2r = NPY_NAN;
=======
  if ((m < 0) || (m != floor(m)) || (q<0)) {
    *f2r = NPY_NAN;
    *d2r = NPY_NAN;
    sf_error("mcm2", SF_ERROR_DOMAIN, NULL);
    return -1;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }
  int_m = (int )m;
  F_FUNC(mtu12,MTU12)(&kf,&kc,&int_m, &q, &x, &f1r, &d1r, f2r, d2r);
  return 0;  
}

int msm2_wrap(double m, double q, double x, double *f2r, double *d2r)
{
  int int_m, kf=2, kc=2;
  double f1r, d1r;

  if ((m < 1) || (m != floor(m)) || (q<0)) {
    *f2r = NPY_NAN;
    *d2r = NPY_NAN;
<<<<<<< HEAD
=======
    sf_error("msm2", SF_ERROR_DOMAIN, NULL);
    return -1;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }
  int_m = (int )m;
  F_FUNC(mtu12,MTU12)(&kf,&kc,&int_m, &q, &x, &f1r, &d1r, f2r, d2r);
  return 0;  
}


double pmv_wrap(double m, double v, double x){
  int int_m;
  double out;

  if (m != floor(m)) return NPY_NAN;
  int_m = (int ) m;
  F_FUNC(lpmv,LPMV)(&v, &int_m, &x, &out);
<<<<<<< HEAD
=======
  CONVINF("pmv", out);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  return out;
}


/* if x > 0 return w1f and w1d.
    otherwise return w2f and w2d (after abs(x))
*/
int pbwa_wrap(double a, double x, double *wf, double *wd) {
  int flag = 0;
  double w1f, w1d, w2f, w2d;
   
  if (x < 0) {x=-x; flag=1;}
  F_FUNC(pbwa,PBWA)(&a, &x, &w1f, &w1d, &w2f, &w2d);
  if (flag) {
    *wf = w2f;
    *wd = w2d;
  }
  else {
    *wf = w1f;
    *wd = w1d;
  }
  return 0;
}

int pbdv_wrap(double v, double x, double *pdf, double *pdd) {

  double *dv;
  double *dp;
  int num;

  /* NB. Indexing of DV/DP in specfun.f:PBDV starts from 0, hence +2 */
  num = ABS((int)v) + 2;
  dv = (double *)PyMem_Malloc(sizeof(double)*2*num);
  if (dv==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("pbdv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *pdf = NPY_NAN;
    *pdd = NPY_NAN;
    return -1;
  }
  dp = dv + num;
  F_FUNC(pbdv,PBDV)(&v, &x, dv, dp, pdf, pdd);
  PyMem_Free(dv);
  return 0;
}

int pbvv_wrap(double v, double x, double *pvf, double *pvd) {
  double *vv;
  double *vp;
  int num;

  /* NB. Indexing of DV/DP in specfun.f:PBVV starts from 0, hence +2 */
  num = ABS((int)v) + 2;
  vv = (double *)PyMem_Malloc(sizeof(double)*2*num);
  if (vv==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("pbvv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *pvf = NPY_NAN;
    *pvd = NPY_NAN;
    return -1;
  }
  vp = vv + num;
  F_FUNC(pbvv,PBVV)(&v, &x, vv, vp, pvf, pvd);
  PyMem_Free(vv);
  return 0;
}

double prolate_segv_wrap(double m, double n, double c)
{
  int kd=1;
  int int_m, int_n;
  double cv, *eg;

  if ((m<0) || (n<m) || (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
    return NPY_NAN;
  }
  int_m = (int) m;
  int_n = (int) n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("prolate_segv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  PyMem_Free(eg);
  return cv;
}

double oblate_segv_wrap(double m, double n, double c)
{
  int kd=-1;
  int int_m, int_n;
  double cv, *eg;

  if ((m<0) || (n<m) || (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
    return NPY_NAN;
  }
  int_m = (int) m;
  int_n = (int) n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("oblate_segv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  PyMem_Free(eg);
  return cv;
}


double prolate_aswfa_nocv_wrap(double m, double n, double c, double x, double *s1d)
{
  int kd = 1;
  int int_m, int_n;
  double cv, s1f, *eg;

  if ((x >=1) || (x <=-1) || (m<0) || (n<m) || \
      (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
<<<<<<< HEAD
=======
    sf_error("prolate_aswfa_nocv", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *s1d = NPY_NAN;
    return NPY_NAN;
  }
  int_m = (int )m;
  int_n = (int )n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("prolate_aswfa_nocv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *s1d = NPY_NAN;
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  F_FUNC(aswfa,ASWFA)(&int_m,&int_n,&c,&x,&kd,&cv,&s1f,s1d);
  PyMem_Free(eg);
  return s1f;
}


double oblate_aswfa_nocv_wrap(double m, double n, double c, double x, double *s1d)
{
  int kd = -1;
  int int_m, int_n;
  double cv, s1f, *eg;

  if ((x >=1) || (x <=-1) || (m<0) || (n<m) || \
      (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
<<<<<<< HEAD
=======
    sf_error("oblate_aswfa_nocv", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *s1d = NPY_NAN;
    return NPY_NAN;
  }
  int_m = (int )m;
  int_n = (int )n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("oblate_aswfa_nocv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *s1d = NPY_NAN;
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  F_FUNC(aswfa,ASWFA)(&int_m,&int_n,&c,&x,&kd,&cv,&s1f,s1d);
  PyMem_Free(eg);
  return s1f;
}


int prolate_aswfa_wrap(double m, double n, double c, double cv, double x, double *s1f, double *s1d)
{
  int kd = 1;
  int int_m, int_n;

  if ((x >=1) || (x <=-1) || (m<0) || (n<m) || \
      (m!=floor(m)) || (n!=floor(n))) {
<<<<<<< HEAD
=======
    sf_error("prolate_aswfa", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *s1f = NPY_NAN;
    *s1d = NPY_NAN;
    return 0;
  }
  int_m = (int )m;
  int_n = (int )n;
  F_FUNC(aswfa,ASWFA)(&int_m,&int_n,&c,&x,&kd,&cv,s1f,s1d);
  return 0;
}


int oblate_aswfa_wrap(double m, double n, double c, double cv, double x, double *s1f, double *s1d)
{
  int kd = -1;
  int int_m, int_n;

  if ((x >=1) || (x <=-1) || (m<0) || (n<m) || \
      (m!=floor(m)) || (n!=floor(n))) {
<<<<<<< HEAD
=======
    sf_error("oblate_aswfa", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *s1f = NPY_NAN;
    *s1d = NPY_NAN;
    return 0;
  }
  int_m = (int )m;
  int_n = (int )n;
  F_FUNC(aswfa,ASWFA)(&int_m,&int_n,&c,&x,&kd,&cv,s1f,s1d);
  return 0;
}


double prolate_radial1_nocv_wrap(double m, double n, double c, double x, double *r1d)
{
  int kf=1, kd=1;
  double r2f, r2d, r1f, cv, *eg;
  int int_m, int_n;

  if ((x <=1.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
<<<<<<< HEAD
=======
    sf_error("prolate_radial1_nocv", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r1d = NPY_NAN;
    return NPY_NAN;
  }
  int_m = (int )m;
  int_n = (int )n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("prolate_radial1_nocv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r1d = NPY_NAN;
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  F_FUNC(rswfp,RSWFP)(&int_m,&int_n,&c,&x,&cv,&kf,&r1f,r1d,&r2f,&r2d);
  PyMem_Free(eg);
  return r1f;
}

double prolate_radial2_nocv_wrap(double m, double n, double c, double x, double *r2d)
{
  int kf=2, kd=1;
  double r1f, r1d, r2f, cv, *eg;
  int int_m, int_n;

  if ((x <=1.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
<<<<<<< HEAD
=======
    sf_error("prolate_radial2_nocv", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r2d = NPY_NAN;
    return NPY_NAN;
  }
  int_m = (int )m;
  int_n = (int )n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("prolate_radial2_nocv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r2d = NPY_NAN;
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  F_FUNC(rswfp,RSWFP)(&int_m,&int_n,&c,&x,&cv,&kf,&r1f,&r1d,&r2f,r2d);
  PyMem_Free(eg);
  return r2f;
}

int prolate_radial1_wrap(double m, double n, double c, double cv, double x, double *r1f, double *r1d)
{
  int kf=1;
  double r2f, r2d;
  int int_m, int_n;

  if ((x <= 1.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n))) {
<<<<<<< HEAD
=======
    sf_error("prolate_radial1", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r1f = NPY_NAN;
    *r1d = NPY_NAN;
    return 0;
  }
  int_m = (int )m;
  int_n = (int )n;
  F_FUNC(rswfp,RSWFP)(&int_m,&int_n,&c,&x,&cv,&kf,r1f,r1d,&r2f,&r2d);
  return 0;  
}

int prolate_radial2_wrap(double m, double n, double c, double cv, double x, double *r2f, double *r2d)
{
  int kf=2;
  double r1f, r1d;
  int int_m, int_n;

  if ((x <= 1.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n))) {
<<<<<<< HEAD
=======
    sf_error("prolate_radial2", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r2f = NPY_NAN;
    *r2d = NPY_NAN;
    return 0;
  }
  int_m = (int )m;
  int_n = (int )n;
  F_FUNC(rswfp,RSWFP)(&int_m,&int_n,&c,&x,&cv,&kf,&r1f,&r1d,r2f,r2d);
  return 0;  
}

double oblate_radial1_nocv_wrap(double m, double n, double c, double x, double *r1d)
{
  int kf=1, kd=-1;
  double r2f, r2d, r1f, cv, *eg;
  int int_m, int_n;

  if ((x < 0.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
<<<<<<< HEAD
=======
    sf_error("oblate_radial1_nocv", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r1d = NPY_NAN;
    return NPY_NAN;
  }
  int_m = (int )m;
  int_n = (int )n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("oblate_radial1_nocv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r1d = NPY_NAN;
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  F_FUNC(rswfo,RSWFO)(&int_m,&int_n,&c,&x,&cv,&kf,&r1f,r1d,&r2f,&r2d);
  PyMem_Free(eg);
  return r1f;
}

double oblate_radial2_nocv_wrap(double m, double n, double c, double x, double *r2d)
{
  int kf=2, kd=-1;
  double r1f, r1d, r2f, cv, *eg;
  int int_m, int_n;

  if ((x < 0.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n)) || ((n-m)>198)) {
<<<<<<< HEAD
=======
    sf_error("oblate_radial2_nocv", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r2d = NPY_NAN;
    return NPY_NAN;
  }
  int_m = (int )m;
  int_n = (int )n;
  eg = (double *)PyMem_Malloc(sizeof(double)*(n-m+2));
  if (eg==NULL) {
<<<<<<< HEAD
    printf("Warning: Memory allocation error.\n"); 
=======
    sf_error("oblate_radial2_nocv", SF_ERROR_OTHER, "memory allocation error");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r2d = NPY_NAN;
    return NPY_NAN;
  }
  F_FUNC(segv,SEGV)(&int_m,&int_n,&c,&kd,&cv,eg);
  F_FUNC(rswfo,RSWFO)(&int_m,&int_n,&c,&x,&cv,&kf,&r1f,&r1d,&r2f,r2d);
  PyMem_Free(eg);
  return r2f;
}

int oblate_radial1_wrap(double m, double n, double c, double cv, double x, double *r1f, double *r1d)
{
  int kf=1;
  double r2f, r2d;
  int int_m, int_n;

  if ((x <0.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n))) {
<<<<<<< HEAD
=======
    sf_error("oblate_radial1", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r1f = NPY_NAN;
    *r1d = NPY_NAN;
    return 0;
  }
  int_m = (int )m;
  int_n = (int )n;
  F_FUNC(rswfo,RSWFO)(&int_m,&int_n,&c,&x,&cv,&kf,r1f,r1d,&r2f,&r2d);
  return 0;  
}

int oblate_radial2_wrap(double m, double n, double c, double cv, double x, double *r2f, double *r2d)
{
  int kf=2;
  double r1f, r1d;
  int int_m, int_n;

  if ((x <0.0) || (m<0) || (n<m) || \
     (m!=floor(m)) || (n!=floor(n))) {
<<<<<<< HEAD
=======
    sf_error("oblate_radial2", SF_ERROR_DOMAIN, NULL);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    *r2f = NPY_NAN;
    *r2d = NPY_NAN;
    return 0;
  }
  int_m = (int )m;
  int_n = (int )n;
  F_FUNC(rswfo,RSWFO)(&int_m,&int_n,&c,&x,&cv,&kf,&r1f,&r1d,r2f,r2d);
  return 0;  
}


<<<<<<< HEAD
int modified_fresnel_plus_wrap(double x, Py_complex *Fplus, Py_complex *Kplus)
=======
int modified_fresnel_plus_wrap(double x, npy_cdouble *Fplus, npy_cdouble *Kplus)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
{
  int ks=0;
  double fm, fa, gm, ga;
  
  F_FUNC(ffk,FFK)(&ks,&x,F2C_CST(Fplus),&fm,&fa,F2C_CST(Kplus),&gm,&ga);
  return 0;
}

<<<<<<< HEAD
int modified_fresnel_minus_wrap(double x, Py_complex *Fminus, Py_complex *Kminus)
=======
int modified_fresnel_minus_wrap(double x, npy_cdouble *Fminus, npy_cdouble *Kminus)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
{
  int ks=1;
  double fm, fa, gm, ga;
  
  F_FUNC(ffk,FFK)(&ks,&x,F2C_CST(Fminus),&fm,&fa,F2C_CST(Kminus),&gm,&ga);
  return 0;
}

