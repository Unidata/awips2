C*********************************************************
C
C          ROUND GAGE HEIGHTS TO NEAREST TENTH OF A FOOT
C
      SUBROUTINE RND59(H,I1,I2)
      DIMENSION H(720)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/rnd59.f,v $
     . $',                                                             '
     .$Id: rnd59.f,v 1.1 1998/04/07 13:31:29 page Exp $
     . $' /
C    ===================================================================
C
      DO 10 I=I1,I2
         DUM=H(I)*10.
         IDUM=INT(DUM)
         DIFF=DUM-REAL(IDUM)
         IF(ABS(DIFF).GT.0.5) THEN
           IF(DUM.LT.0.) THEN
            IDUM=IDUM-1
           ELSE
            IDUM=IDUM+1
           END IF
         END IF
   10    H(I)=REAL(IDUM)/10.
      RETURN
      END
