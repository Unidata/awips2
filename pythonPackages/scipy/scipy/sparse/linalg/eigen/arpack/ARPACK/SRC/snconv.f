c-----------------------------------------------------------------------
c\BeginDoc
c
c\Name: snconv
c
c\Description: 
c  Convergence testing for the nonsymmetric Arnoldi eigenvalue routine.
c
c\Usage:
c  call snconv
c     ( N, RITZR, RITZI, BOUNDS, TOL, NCONV )
c
c\Arguments
c  N       Integer.  (INPUT)
c          Number of Ritz values to check for convergence.
c
c  RITZR,  Real arrays of length N.  (INPUT)
c  RITZI   Real and imaginary parts of the Ritz values to be checked
c          for convergence.

c  BOUNDS  Real array of length N.  (INPUT)
c          Ritz estimates for the Ritz values in RITZR and RITZI.
c
c  TOL     Real scalar.  (INPUT)
c          Desired backward error for a Ritz value to be considered
c          "converged".
c
c  NCONV   Integer scalar.  (OUTPUT)
c          Number of "converged" Ritz values.
c
c\EndDoc
c
c-----------------------------------------------------------------------
c
c\BeginLib
c
c\Local variables:
c     xxxxxx  real
c
c\Routines called:
<<<<<<< HEAD
c     second  ARPACK utility routine for timing.
c     slamch  LAPACK routine that determines machine constants.
c     slapy2  LAPACK routine to compute sqrt(x**2+y**2) carefully.
=======
c     arscnd  ARPACK utility routine for timing.
c     wslamch  LAPACK routine that determines machine constants.
c     wslapy2  LAPACK routine to compute sqrt(x**2+y**2) carefully.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
c
c\Author
c     Danny Sorensen               Phuong Vu
c     Richard Lehoucq              CRPC / Rice University 
c     Dept. of Computational &     Houston, Texas
c     Applied Mathematics 
c     Rice University           
c     Houston, Texas    
c
c\Revision history:
c     xx/xx/92: Version ' 2.1'
c
c\SCCS Information: @(#) 
c FILE: nconv.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
c
c\Remarks
c     1. xxxx
c
c\EndLib
c
c-----------------------------------------------------------------------
c
      subroutine snconv (n, ritzr, ritzi, bounds, tol, nconv)
c
c     %----------------------------------------------------%
c     | Include files for debugging and timing information |
c     %----------------------------------------------------%
c
      include   'debug.h'
      include   'stat.h'
c
c     %------------------%
c     | Scalar Arguments |
c     %------------------%
c
      integer    n, nconv
      Real
     &           tol
c
c     %-----------------%
c     | Array Arguments |
c     %-----------------%

      Real
     &           ritzr(n), ritzi(n), bounds(n)
c
c     %---------------%
c     | Local Scalars |
c     %---------------%
c
      integer    i
      Real
     &           temp, eps23
c
c     %--------------------%
c     | External Functions |
c     %--------------------%
c
      Real
<<<<<<< HEAD
     &           slapy2, slamch
      external   slapy2, slamch
=======
     &           wslapy2, wslamch
      external   wslapy2, wslamch
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

c     %-----------------------%
c     | Executable Statements |
c     %-----------------------%
c 
c     %-------------------------------------------------------------%
c     | Convergence test: unlike in the symmetric code, I am not    |
c     | using things like refined error bounds and gap condition    |
c     | because I don't know the exact equivalent concept.          |
c     |                                                             |
c     | Instead the i-th Ritz value is considered "converged" when: |
c     |                                                             |
c     |     bounds(i) .le. ( TOL * | ritz | )                       |
c     |                                                             |
c     | for some appropriate choice of norm.                        |
c     %-------------------------------------------------------------%
c
<<<<<<< HEAD
      call second (t0)
=======
      call arscnd (t0)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
c
c     %---------------------------------%
c     | Get machine dependent constant. |
c     %---------------------------------%
c
<<<<<<< HEAD
      eps23 = slamch('Epsilon-Machine')
=======
      eps23 = wslamch('Epsilon-Machine')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      eps23 = eps23**(2.0E+0 / 3.0E+0)
c
      nconv  = 0
      do 20 i = 1, n
<<<<<<< HEAD
         temp = max( eps23, slapy2( ritzr(i), ritzi(i) ) )
         if (bounds(i) .le. tol*temp)   nconv = nconv + 1
   20 continue
c 
      call second (t1)
=======
         temp = max( eps23, wslapy2( ritzr(i), ritzi(i) ) )
         if (bounds(i) .le. tol*temp)   nconv = nconv + 1
   20 continue
c 
      call arscnd (t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      tnconv = tnconv + (t1 - t0)
c 
      return
c
c     %---------------%
c     | End of snconv |
c     %---------------%
c
      end
