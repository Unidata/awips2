      SUBROUTINE INITY55(IYI,I,J,YDI,NGAGE,NGS,STT,LTSTT,K1,K2,K4)
C  THIS SUBROUTINE DETERMINES IF A GIVEN CROSS-SECTION'S INITIAL WATER
C  ELEVATION HAS BEEN READ-IN OR WILL BE COMPUTED VIA BACKWATER OR
C  DOWNWATER
C
C  IYI =0, WATER ELEV. TO BE COMPUTED
C  IYI =1, KNOWN WATER ELEV
C  YDI =0., TO BE COMPUTED
C  YDI =1., OBSERVED ELEV. FROM TIME SERIES
C  YDI >1., OBSERVED ELEV.

      INCLUDE 'common/fdbug'

      DIMENSION YDI(K2,K1),NGAGE(K1),NGS(K4,K1),STT(*),LTSTT(*),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/inity55.f,v $
     . $',                                                             '
     .$Id: inity55.f,v 1.1 1999/04/23 18:08:37 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HINIT,4HY55 /

      CALL FPRBUG(SNAME,1,55,IBUG)

      IYI=0
      IF(ABS(YDI(I,J)-1.0) .LE. 0.01) GO TO 10
      IF(ABS(YDI(I,J)) .GT. 0.01) GO TO 50
      GO TO 60
   10 NGAG=NGAGE(J)
      IF(NGAG.LE.0) GO TO 60

      DO 15 K=1,NGAG
      IF (I .EQ. NGS(K,J)) GO TO 20
   15 CONTINUE
      GO TO 60
   20 KJ=LCAT21(K,J,NGAGE)
      LKJ=LTSTT(KJ)
      YS=STT(1+LKJ)
      YDI(I,J)=YS
      IF (YS .LT. -100.0) GO TO 60
   50 IYI=1
   60 CONTINUE
      RETURN
      END

