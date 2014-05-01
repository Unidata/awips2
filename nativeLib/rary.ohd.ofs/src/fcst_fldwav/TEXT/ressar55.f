      SUBROUTINE RESSAR55(J,L,YY,SAR,HSAR,SA,DSA,K1,K16)
C  COMPUTES RESERVOIR SURFACE AT ELEVATION YY

      INCLUDE 'common/fdbug'

      DIMENSION SAR(8,K16,K1),HSAR(8,K16,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/ressar55.f,v $
     . $',                                                             '
     .$Id: ressar55.f,v 1.1 1999/04/23 18:08:52 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HRESS,4HAR55/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      DO 5 KK=2,8
      K=KK
      IF(YY.GE.HSAR(K,L,J)) GO TO 10
      IF(K.EQ.8) GO TO 10
      IF(ABS(HSAR(K+1,L,J)-0.001).LT.0.01) GO TO 10
    5 CONTINUE
   10 KL=K-1
      IF(ABS(HSAR(2,L,J)).LT.0.02) GO TO 13
      DSA=(SAR(K,L,J)-SAR(KL,L,J))/(HSAR(K,L,J)-HSAR(KL,L,J))
      SA=SAR(KL,L,J)+DSA*(YY-HSAR(KL,L,J))
      IF(SA.GT.0.0) GO TO 15
      SA=0.
      DSA=0.
      GO TO 15
   13 DEP=YY-HSAR(1,L,J)
      SA=SAR(1,L,J)+SAR(2,L,J)*DEP+SAR(3,L,J)*DEP*DEP+SAR(4,L,J)*
     1  DEP*DEP*DEP
      DSA=SAR(2,L,J)+2.*SAR(3,L,J)*DEP+3.*SAR(4,L,J)*DEP*DEP
   15 RETURN
      END
