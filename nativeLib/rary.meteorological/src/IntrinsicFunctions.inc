c IntrinsicFunctions.inc
c
c logical tests between integer and/or logical variables
      EXTERNAL  b1_odd, i4_odd, bitest
      Logical*2 b1_odd, i4_odd, bitest
c
c These are function declarations of g77 unimplemented intrinsic functions 
c
      EXTERNAL  iiand,  iishft, iior,  imod
      integer*2 iiand,  iishft, iior,  imod

      EXTERNAL  jnint,  jint,   jmax0, jmin0, jisign, jiabs
      integer*4 jnint,  jint,   jmax0, jmin0, jisign, jiabs
c
c  This function is an intrinsic function in the gcc-2.95.1 release, SOD 1 Nov.99 
c      EXTERNAL  dabs
c      real*8    dabs
c



