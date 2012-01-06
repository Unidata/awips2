C     THIS PROGRAM DETERMINES THE TIME STEP FOR NEXT RUN
C     FOR TT>0.0 (KIT>KWARM+1)
C     DTH/DTHNEW  CURRENT/NEXT TIME STEP
C     KAUTO=1  Automatic increase DT after failure


      SUBROUTINE TSTEP55(DTHNEW,DTH,TFMN,TU,TU2,DTIN,TDTIN,TPK1,IDAM,
     $  N1,KAUTO,QD,QU,DTHS1,JK,K1,K2)
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/TKEP55/DTHII,MDT,NDT,DTHS,TFH1
      COMMON/CHECKT55/CKT1(5),CKT2(5)
      DIMENSION DTIN(1),TDTIN(1),QD(K2,K1),QU(K2,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/tstep55.f,v $
     . $',                                                             '
     .$Id: tstep55.f,v 1.4 2004/02/02 21:54:10 jgofus Exp $
     . $' /
C    ===================================================================
C

      IF(NDT.LT.2) GOTO 210
      DO 200 K=1,NDT
        IF((TDTIN(K)-TT).GT.0.00001) THEN
          DTHNEW=DTIN(K)
          GOTO 201
        ENDIF
  200 CONTINUE
  201 IF (DTHNEW.GT.TFMN .AND. TT.LT.TPK1) DTHNEW=TFMN
      GOTO 990

C BACK TO TRACK IN THE CASE OF DT REDUCTION IN SOLVE
c.......................
  210 IF(TT.GT.0.0.AND.TT.LT.(TU-0.000001)) THEN
        DTHNEW=TU-TT
        GOTO 999
      ENDIF

      IF(TT.GT.0.0.AND.TT.LT.(TU2-0.0000001)) THEN
        DTHNEW=TU2-TT
        GOTO 990
      ENDIF
      DTHNEW=DTHS
      DDTT=1.2*DTH
      IF (DTHNEW.GT.TFMN .AND. TFMN.GT.0.000001) DTHNEW=TFMN
      IF (KAUTO.EQ.0) GOTO 990
      IF (DTHII.GT.0.0) GOTO 990
      IF (TT.LT.TPK1) GOTO 990
      TOL=20.0
           DO 280 K=1,5
           IS=N1-(K-1)*(N1-IDAM)/5
           DQ=QU(IS,JK)-QD(IS,JK)
           IF (DQ.GE.TOL .AND. CKT1(K).EQ.0.0) CKT1(K)=TT
           IF (DQ.LE.-TOL .AND. CKT2(K).EQ.0.0) CKT2(K)=TT
           IF (CKT1(K).GT.0.0 .AND. CKT2(K).GT.0.0) THEN
              DTHNEW=(CKT2(K)-CKT1(K))/MDT
              IF (DTHNEW.LE.DTH) DTHNEW=DTH
              IF (DTHNEW.LE.DTHS) DTHNEW=DTHS
              IF (DTHNEW.GT.DDTT) DTHNEW=DDTT
              IF (JN.GT.1 .AND. DTHNEW.GT.DTHS1) DTHNEW=DTHS1
              GOTO 300
              ENDIF
280        CONTINUE   
300   CONTINUE
990   IF (NLEV.GE.1 .AND. DTHNEW.GT.2.0*DTH) DTHNEW=2.0*DTH
999   RETURN
      END
