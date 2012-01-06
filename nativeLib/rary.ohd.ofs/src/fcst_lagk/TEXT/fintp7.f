C MEMBER FINTP7
C  (from old member FCEX7)
C-----------------------------------------------------------------------
C
      FUNCTION FINTP7(VALUE,TABLE)
C.......................................................................
C
C    THIS FUNCTION DOES A LINEAR INTERPOLATION FOR THE MULTIPLE
C    INTERCEPT PROBLEM.
C.......................................................................
C
C    FUNCTION ORIGINALLY PROGRAMMED BY
C              JANICE M. LEWIS - HRL   OCTOBER 1992
C.......................................................................
      DIMENSION TABLE(2,2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fintp7.f,v $
     . $',                                                             '
     .$Id: fintp7.f,v 1.1 1995/09/17 18:57:51 dws Exp $
     . $' /
C    ===================================================================
C
      RATIO=(TABLE(1,2)-TABLE(1,1))/(TABLE(2,2)-TABLE(2,1))
      FINTP7=TABLE(1,1)+RATIO*(VALUE-TABLE(2,1))
C     WRITE(6,9999) TABLE(1,1),TABLE(1,2),TABLE(2,1),TABLE(2,2),RATIO
C9999 FORMAT(10X,'T11 T12 T21 T22 RATIO = ',5F10.3)
      RETURN
      END
