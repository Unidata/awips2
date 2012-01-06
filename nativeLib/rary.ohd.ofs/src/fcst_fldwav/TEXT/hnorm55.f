      SUBROUTINE HNORM55(PO,JNK,NCML,NQCM,J,I,IRCH,Y1,Q1,SO,YMN,YMX
     * ,DYNX,LC,K1,K2,K7,K8,K9)
      COMMON/VS55/MUD,IWF,SHR,VIS,UW,PB,SIMUD
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/FLP55/KFLP
      COMMON/METR55/METRIC
      COMMON/PRES55/KPRES
      COMMON/PRMS55/DPRM,WPRM
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*),NQCM(1) 
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/hnorm55.f,v $
     . $',                                                             '
     .$Id: hnorm55.f,v 1.2 2000/12/19 15:51:04 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'HNORM55 ' /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      Q1A=ABS(Q1)
      E23=2./3.
      E43=4./3.
      FK=1.49*SQRT(SO)
      YTOP=YMX
      YBOT=YMN
      IF(Y1.LE.-999.) GO TO 101
      YTOP1=YTOP+0.1
      YBOT1=YBOT-0.1
C  NEWTON-RAPHSON METHOD
      F=0.
      DO 113 K=1,20
CCC      IT=K
CCC      FOLD=F
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,I,Y1,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      IF(KFLP.EQ.0) GO TO 131
      IH=0
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,I,Y1,QKI,
     1 DQKI,BETI,DBEI,IH,K1,K2)
      F=QKI*SQRT(SO)-Q1A
      FP=DQKI*SQRT(SO)
      GO TO 132
  131 P=WPRM
      DP=DPRM
      YQ=Y1-DYNX
      IF(NQCM(J).LT.0) YQ=Q1
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,IRCH,YQ,CMU,DCM,K1,K7,K8)
      R=A/B
      IF(KPRES.EQ.1) R=A/P
      DR=0.5*(1.-A*DB/B/B)
      IF(KPRES.EQ.1) DR=0.5*(1.-A*DP/P/P)
      SI=0.
      DSI=0.
      F1=FK/CMU*A*R**E23
      F=F1-Q1A
      FP=F1*(B/A+E23*DR/R-DCM/CMU)
  132 IF(MUD.NE.1) GO TO 111
      PB1=PB+1.
      PB2=PB+1.
      PA=1./PB2*(UW/VIS)**PB
      PC=0.5*(SHR/VIS)**PB
      D=A/B
      DD=0.5*D*(B/A-DB/B)
      SF=CMU*CMU*Q1*ABS(Q1)/(2.21*A*A*D**E43)
      SI=SO-SF
      IF(SI.LT.0.000001) SI=0.000001
      DSF=SF*(DCM/CMU-B/A-4./3.*DR/R)
      DSI=-DSF
      F1=PA*SI**PB*A*D**PB1
      F=F1-PC*A*D-Q1
      FP=F1*(DSI/SI+B/A+PB1/D)-PC*D*B-PC*A*DD
  111 YNW=Y1-F/FP
      PY1=Y1
      PYN=YNW
      PF=F
      IF(METRIC.EQ.1) THEN
        PY1=Y1/3.281
        PYN=YNW/3.281
        PF=PF/35.32
      ENDIF
      IF(JNK.GE.10) WRITE(IPR,112) K,I,PY1,PYN,PF
  112 FORMAT(10X,2HK=,I2,10X,2HI=,I5,10X,'YNO=',F10.2,10X,'YNN=',F10.2,
     *  20X,2HF=,F10.1)
      IF(ABS(YNW-Y1).LE.0.004) GO TO 114
  113 Y1=YNW
  114 IF(Y1.GT.YTOP1 .OR. Y1.LT.YBOT1) GO TO 101
      Y1=YNW
      RETURN
  101 CONTINUE
C  BISECTION METHOD
      IYT=0
      LMX=100
    7 LC=0
      YMX=YTOP
      YMN=YBOT
      YOLD=0.
    8 Y=0.5*(YMX+YMN)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,I,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      IF(KFLP.EQ.0) GO TO 138
      IH=0
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,I,Y,QKI,
     1 DQKI,BETI,DBEI,IH,K1,K2)
      F=QKI*SQRT(SO)-Q1A
      GO TO 139
  138 CONTINUE
      YQ=Y-DYNX
      IF(NQCM(J).LT.0) YQ=Q1
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,IRCH,YQ,CMU,DCM,K1,K7,K8)
      R=A/B
      IF(KPRES.EQ.1) R=A/WPRM
      F=FK/CMU*A*R**E23-Q1A
  139 PY=Y
      IF(MUD.NE.1) GO TO 9
      PB1=PB+1.
      PB2=PB+1.
      PA=1./PB2*(UW/VIS)**PB
      PC=0.5*(SHR/VIS)**PB
      D=A/B
      SF=CMU*CMU*Q1*ABS(Q1)/(2.21*A*A*R**E43)
      SI=SO-SF
      IF(SI.LT.0.000001) SI=0.000001
      F=PA*SI**PB*A*D**PB1-PC*A*D-Q1
    9 PF=F
      PFK=FK
      PA=A
      PR=R
      PQ1=Q1
      PY=Y
      IF(METRIC.EQ.1) THEN
        PY=Y/3.281
        PF=PF/35.32
        PFK=PFK/1.49
        PA=PA/10.765
        PR=PR/3.281
        PQ1=PQ1/35.32
      ENDIF
      IF(JNK.GE.10) WRITE(IPR,70) I,PY,PF,PFK,PA,PR,CMU,PQ1
      IF(ABS(Y-YOLD).LE.0.004) GO TO 12
      IF(F.GT.0.0) YMX=Y
      IF(F.LT.0.0) YMN=Y
      YOLD=Y
      LC=LC+1
      IF(LC.LE.LMX) GO TO 8
  116 WRITE(IPR,69) I
   69 FORMAT(//'NORMAL DEPTH COMPUTATION DID NOT CONVERGE; ',
     1' PROGRAM STOPS AT SECTION NO=',I5//)
      STOP
   12 CONTINUE
      IF(SO.LE.0.000001) GO TO 18
      IF((YTOP-Y) .GT. 0.04) GO TO 18
      IYT=IYT+1
      IF(IYT.GE.2) GO TO 116
      YBOT=Y-0.05
      YTOP=YTOP+100.0
      GO TO 7
   18 Y1=Y
   70 FORMAT(10X,2HI=,I5,10X,2HY=,F10.2,10X,2HF=,F10.1,2X,'FK=',F10.4,
     . 2X,'A=',F10.2,2X,'R=',F10.2,5X,4HCMU=,F6.4,2X,'Q1=',F10.2)
      RETURN
      END
