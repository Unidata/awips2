C
C THIS SUBROUTINE COMPUTES INITIAL DISCHARGE
C BY ADDING LATERAL AND TRIBUTARY FLOWS
C

      SUBROUTINE QMAIN55(PO,CO,Z,ST1,LTST1,T1,LTT1,POLH,LTPOLH,ITWT,
     . LTITWT,ITRBW,ITQ,NB,QDI,QOTHR,QGH,YU,YDUMY,NJUN,
     . NQL,LQ1,LQN,QL,LTQL,QLI,KU,KRCH,HS,DDX,YQI,SLFI,NQCM,
     . NCS,IORDR,MRV,MRU,NJUM,K1,K2,K7,K8,K9,K10,K15,K16,K19,K20,K21)
      COMMON/METR55/METRIC
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M655/KTIME,DTHYD,J1
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/IDOS55/IDOS,IFCST
      COMMON/IONUM/IN,IPR,IPU
      COMMON/NETWK55/NET

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'
C
      DIMENSION PO(*),CO(*),Z(*),ST1(*),LTST1(*),T1(*),LTT1(*)
      DIMENSION POLH(*),LTPOLH(*),ITWT(*),LTITWT(*),NB(K1),QDI(K2,K1)
      DIMENSION QOTHR(K16,K1),LQ1(K10,K1)
      DIMENSION QGH(20,K16,K1),YU(K2,K1),YDUMY(K2,K1),NJUN(K1),NQL(K1)
      DIMENSION QL(*),LTQL(*),QLI(*),HS(K9,K2,K1),LQN(K10,K1)
      DIMENSION KU(K1),KRCH(K2,K1),DDX(K2,K1),YQI(K1)
      DIMENSION NQCM(K1),IORDR(K1),MRV(K1),MRU(K1),NJUM(K1)
      DIMENSION SLFI(K2,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/qmain55.f,v $
     . $',                                                             '
     .$Id: qmain55.f,v 1.3 2000/12/19 15:55:10 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/ 'QMAIN55 ' /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      TOL=0.000001
      AQ1=ABS(QDI(1,1))
      AQ2=ABS(QDI(2,1))
      AQ3=ABS(QDI(3,1))
      IF(ITRBW.EQ.1.AND.AQ2.GT.0.01.AND.AQ3.GT.0.01) GO TO 999
C COMPUTATION STARTS FROM LAST-ORDERED RIVER TO FIRST RIVER
      DO 70 M=1,JN
      J=IORDR(JN-M+1)
      MJ=MRV(J)
      N=NB(J)
      ITRS=1
      IX=1
      IU=1
c.......................................................................
      L1J=LTST1(J)
c .. set qdi equal to 1st point in inflow hydrograph (t=0)
c .. if in fcst mode set qdi equal iflow point at t=dt if qdi=0

      IF(KU(J).EQ.2.AND.IDOS.LT.3) QDI(1,J)=ST1(L1J)
      IF(KU(J).EQ.2.AND.IDOS.GE.3.AND.AQ1.LE.TOL) QDI(1,J)=ST1(L1J)

c.......................................................................
c .. if u/s boundary is stage, estimate qdi
      IF (KU(J).GT.1) GOTO 50
      ITQ=1
C     ESTIMATE Q CODE REQUIRED ??????????????????????????????????
      IF (ABS(QDI(1,J)) .LE. 0.1) THEN
        WRITE(IPR,*)
      WRITE(IPR,*) 'YOU MUST INPUT QDI(1,J) FOR KU(J)=1'
        STOP
        ENDIF
c.......................................................................
c .. if in calibration mode, set qdi equal to 1st flow in calibration pd
  50  IF(IOBS.LE.-1.AND.KU(J).EQ.2) QDI(1,J)=YQI(J)
c.......................................................................
      LJ=LCAT21(1,J,NQL)-1
      DO 60 I=1,N
      IM=I-1
      IF(IM.LE.1) IM=1
      QJN=0.0
      QQL=0.0
      QNET=0.0
      IF(I.EQ.1) GO TO 52
      NQLJ=NQL(J)
C LATERAL FLOW (QQL)
      IF(NQLJ.EQ.0) GO TO 65
      DO 63 L=1,NQLJ
      IR1=LQ1(L,J)
      IRN=LQN(L,J)-1
      LLJ=LTQL(L+LJ)-1
      IF(IM.LT.IR1.OR.IM.GT.IRN)GO TO 63
CC      QQL=QL(1+LLJ)*DDX(IM,J)
      QQL=QLI(L+LJ)*DDX(IM,J)
      IU=I
      GO TO 65
   63 CONTINUE
   65 CONTINUE
C TRIBUTARY FLOW (QJN)
      DO 87 J2=1,JN
      IF(MRV(J2).EQ.J.AND.IM.EQ.NJUN(J2)) THEN
        NBJ=NB(J2)
        QJN=QJN+QDI(NBJ,J2)
        GO TO 88
      ENDIF
   87 CONTINUE
C NETWORK-RIVER INFLOW (QNET)
   88 IF (NET.LE.0) GOTO 100
      DO 90 JJ=JN+1,JN+NET
      QNET1=QDI(1,JJ)
         IF (QNET1 .LE. 0.1) THEN
           WRITE(IPR,*)
           WRITE(IPR,*) 'YOU MUST INPUT QDI(1,J) FOR NETWORK-RIVER'
           STOP
           ENDIF
      JOUT=MRU(JJ)
      IOUT=NJUM(JJ)
      IF (J.EQ.JOUT .AND. I.EQ.IOUT) QNET=-QNET1
      JIN=MRV(JJ)
        IIN=NJUN(JJ)
      IF (J.EQ.JIN .AND. I.EQ.IIN) QNET=QNET1
   90 CONTINUE
c.......................................................................
  100 QDI(I,J)=QDI(IM,J)+QQL+QJN+QNET
C INTERNAL BOUNDARY
c.......................................................................
   52 KRA=IABS(KRCH(I,J))
      IF(KRA.LT.10.OR.KRA.GT.30) GO TO 60
      CALL IDDB55(I,J,PO(LONLAD),PO(LOLAD),KL,K1,K16)
      IF(KRA.EQ.26) THEN
        QOTHR(KL,J)=QGH(1,KL,J)
        QDI(I,J)=QOTHR(KL,J)
        GO TO 55
      END IF
      YDUMA=ABS(YDUMY(I,J))
      IF(YDUMA.LE.0.004) THEN
        QOTHR(KL,J)=QDI(I,J)
        GO TO 60
      END IF
      IR=I+1
      YU(I,J)=YDUMY(I,J)
      YU(IR,J)=HS(1,IR,J)+0.5
      DTIM=1.0
      IREPT=0
   54 CALL DAM55(PO,CO,Z,ST1,LTST1,T1,LTT1,POLH,LTPOLH,ITWT,LTITWT,
     . Z(LZD),Z(LZC),Z(LZQU),Z(LZQD),YU,YDUMY,PO(LOHDD),PO(LOHSPD),
     . PO(LOHGTD),PO(LOCSD),PO(LOCGD),PO(LOZBCH),PO(LOCDOD),PO(LCNFLD),
     . Z(LZBBP),Z(LZYBP),Z(LZSQS1),Z(LZSQS2),Z(LZSQO),Z(LZQBCH),
     . Z(LZQOTP),Z(LZQOTR),KRCH,PO(LOQGH),PO(LOCLL),PO(LOSPL),PO(LORHI),
     . PO(LORQI),PO(LOTIQH),QDI,QTS,DQNYU,DTIM,IFL,ITRS,IX,IX,IX,IX,IX,
     . IX,ITQ,I,J,K1,K2,K7,K8,K9,K15,K16,K19,K20,K21)

      IF(KRA.NE.28) QDI(I,J)=QTS
      IF(IREPT.EQ.1) GO TO 55
      IREPT=1
      YMN=YU(IR,J)
      DO 105 LL=1,NCS
      LST=NCS-LL+1
      IF(ABS(HS(LST,IR,J)).GT.0.0001) GO TO 106
  105 CONTINUE
  106 YMX=HS(LST,IR,J)
      YNN=-999.
      DYNX=-0.5*(HS(1,IR,J)-HS(1,IR+1,J))
      SO=SLFI(IR,J)
      IRCH=IR
      NCML=ABS(NQCM(J))
      IF(NCML.EQ.0) NCML=NCS
      CALL HNORM55(Z,JNK,NCML,NQCM,J,IR,IRCH,YNN,QTS,SO,YMN,YMX,DYNX,
     1 ITN,K1,K2,K7,K8,K9)
      YU(IR,J)=YNN
      GO TO 54
c.......................................................................
   55 ID=I
      IDMU=ID-IU
      IF(IDMU.LE.1) GO TO 59
      DQ=QDI(ID,J)-QDI(IU,J)
      IF(ABS(DQ).LE.1.) GO TO 59
      DQ=DQ/(ID-IU)
      IDD=ID-1
      IUU=IU+1
      DO 159 II=IUU,IDD
      QDI(II,J)=QDI(II-1,J)+DQ
  159 CONTINUE
   59 IU=ID+1
   60 CONTINUE
c.......................................................................
   70 CONTINUE
      IF (NET.LE.0) GOTO 200
      DO 180 J=JN+1,JN+NET
      DO 170 I=2,NB(J)
  170 QDI(I,J)=QDI(1,J)
  180 CONTINUE
  200 IF (JNK.LE.4) GOTO 999
      WRITE(IPR,211)
  211 FORMAT(/'***** COMPUTE INITIAL DISCHARGES FOR ALL RIVERS *****')
      DO 115 J=1,JN+NET
      N=NB(J)
      WRITE(IPR,3) J
    3 FORMAT(/4X,7H(QDI(I,,I1,8H),I=1,N))
      CALL WYQMET55(METRIC,1,N,QDI(1,J),0,N)
  115 CONTINUE
C  999 WRITE(IPR,11111)
11111 FORMAT(1X,'** EXIT QMAIN **')
  999 RETURN
      END
