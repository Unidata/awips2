      SUBROUTINE GETLAG21(STN,XNOS,NU,LAG)

C  THIS SUBROUTING LAGS THE TIME SERIES ACCORDING THE DIFFERENCE IN
C  PEAK OBS AND NOS T.S. POINT

      INCLUDE 'common/fdbug'
C
      DIMENSION STN(*),XNOS(*)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/getlag21.f,v $
     . $',                                                             '
     .$Id: getlag21.f,v 1.3 2000/09/27 16:12:22 page Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'GETLAG21' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)

      K1=5
      K2=NU-1
      IF(K1.GT.NU) K1=2
      ITOBS=K1
      DO 100 K=K1,K2
        IF(STN(K-1).LT.-900.0.OR.STN(K+1).LT.-900.0) GO TO 100
        IF(STN(K).GT.STN(K-1).AND.STN(K).GT.STN(K+1)) THEN
          HPOBS=STN(K)
          ITOBS=K
          GO TO 150
        ENDIF
  100 CONTINUE

  150 I1=ITOBS-4
      I2=ITOBS+4
      IF(I1.LT.K1) I1=K1
      IF(I2.GT.K2) I2=K2
      ITNOS=I1
      DO 200 K=I1,I2
        IF(XNOS(K).GE.XNOS(K-1).AND.XNOS(K).GT.XNOS(K+1)) THEN
          HPNOS=XNOS(K)
          ITNOS=K
          GO TO 250
        ENDIF
  200 CONTINUE

  250 LAG=ITNOS-ITOBS
      IF(LAG.LT.0.OR.LAG.GT.24) LAG=0

      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
C ----------------------------------------------------------------------










