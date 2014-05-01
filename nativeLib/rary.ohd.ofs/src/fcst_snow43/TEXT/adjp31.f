C    Module ADJP31
C
C
      SUBROUTINE ADJP31(oldx, newx, p)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     This routine adjusts the state error covariance matrix 
C     for a change in water equivalent or snow cover when done 
C     outside of the filter.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C     Nalneesh Gaur - RTi. July 1995
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      oldx          I          Array containing updated states from 
C                               filter.
C      newx          I          Array containing new states updated 
C                               outside of filter.
C      p            I/O         contains P (state error cov. matrix)
C                               New P matrix is returned
C.......................................
C      
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real*4 newx, oldx, p
C     --- L O C A L ---
      integer i, j
      real*4 newp(5,5)
      real*4 varmult
C
C----- D I M E N S I O N --------------------------------
      DIMENSION oldx(5), newx(5), p(5,5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/adjp31.f,v $
     . $',                                                             '
     .$Id: adjp31.f,v 1.1 1996/05/07 10:59:49 page Exp $
     . $' /
C    ===================================================================
C
C
C.......................................
      do 200 i=1,5
          do 100 j=1,5
              newp(i,j) = p(i,j)
  100     continue
  200 continue
C
C
C     Set variance Diagonal elements
C
      do 300 i=1,5
          if(oldx(i) .ne. 0. ) then
              newp(i,i) = p(i,i) * ((newx(i)/oldx(i)) ** 2)
          endif
  300 continue
C
C     set the covariance elements (non-diagonal elements)
C     
      do 410 i=1,4
          do 400 j=i+1,5
           if(p(i,i).eq.0. .OR. p(j,j).eq.0.) then
               varmult = 0.
           else
               varmult = (newp(i,i)*newp(j,j))/(p(i,i)*p(j,j))
           endif
           newp(i,j) = p(i,j) * sqrt(varmult)
  400     continue
  410 continue
C
C     Save full matrix
C
      do 500 i=1,5
          p(i,i) = newp(i,i)
  500 continue
C
      do 510 i=1,4
          do 505 j=i+1,5
              p(i,j) = newp(i,j)
              p(j,i) = p(i,j)
  505     continue
  510 continue
C
      RETURN 
      END
