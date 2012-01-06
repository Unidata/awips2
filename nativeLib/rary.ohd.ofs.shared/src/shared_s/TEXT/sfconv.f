C MEMBER SFCONV
C-----------------------------------------------------------------------
C
      SUBROUTINE SFCONV(IJ,IK,N)
C
      DIMENSION IJ(N),IK(N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sfconv.f,v $
     . $',                                                             '
     .$Id: sfconv.f,v 1.1 1995/09/17 19:20:28 dws Exp $
     . $' /
C    ===================================================================
C
C
      DO 10 I=1,N
   10 IJ(I)=IK(I)
C
      RETURN
      END
