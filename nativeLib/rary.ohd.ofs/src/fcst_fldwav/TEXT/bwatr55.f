      SUBROUTINE BWATR55(PO,JNK,NCML,NQCM,J,IR,I,QIR,QII,YIR,YII,DX,YA,
     1 YMX,DQDT,ITB,K1,K2,K7,K8,K9)
      COMMON/VS55/MUD,IWF,SHR,VIS,UW,PB,SIMUD
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/FLP55/KFLP
      COMMON/METR55/METRIC
      COMMON/PRES55/KPRES
      COMMON/PRMS55/DPRM,WPRM
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*),NQCM(K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/bwatr55.f,v $
     . $',                                                             '
     .$Id: bwatr55.f,v 1.3 2004/02/02 21:49:31 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'BWATR55 ' /

      CALL FPRBUG(SNAME,1,55,IBUG)

      YTOP=YMX
      YBOT=YII
CC      YBOT=HS(1,I,J)+0.1
      YTOP1=YTOP+0.1
      YBOT1=YBOT-0.1
      E43=4./3.
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,IR,YIR,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AR=A
      BR=B
CCC      DBR=DB
CCC      ATR=AT
CCC      BTR=BT
      PRR=WPRM
      QA=0.5*(QII+QIR)
      BET=1.06
      DBE=0.0
      BETI=1.06
      DBEI=0.0
      IF(KFLP.EQ.1) CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),
     * J,IR,YIR,QK,DQK,BET,DBE,0,K1,K2)

      F1=BET*QIR*QIR/AR
      QI2=QII*QII
      IF(YA.LE.-999.0) GO TO 101
      F=0.
      DO 13 K=1,20
      ITB=K
CCC      FOLD=F
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     . PO(LOBSS),J,I,YA,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      IF(KFLP.EQ.0) GO TO 131
      AA=0.5*(A+AR)
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,I,YA,QKI,
     1 DQKI,BETI,DBEI,0,K1,K2)
      AQK=0.5*(QK+QKI)
      SFA=QA*QA/(AQK*AQK)
      DSFA=-SFA*DQKI/AQK
      GO TO 132
  131 P=WPRM
      DP=DPRM
      YAA=0.5*(YIR+YA)
      IF(NQCM(J).LT.0) YAA=QA
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,I,YAA,CMU,DCM,K1,K7,K8)
      AA=0.5*(A+AR)
      BA=0.5*(B+BR)
      PA=0.5*(P+PRR)
      RA=AA/BA
      IF(KPRES.EQ.1) RA=AA/PA
      DRA=0.5*(B/BA-AA*DB/BA/BA)
      IF(KPRES.EQ.1) DRA=0.5*(B/BA-AA*DP/PA/PA)
CCC      PRES=KPRES
      SFA=CMU*CMU*QA*ABS(QA)/(2.208*AA*AA*RA**E43)
      DSFA=SFA*(DCM/CMU-B/AA-4./3.*DRA/RA)
  132 SIA=0.
      DSIA=0.
      IF(MUD.NE.1) GO TO 11
      RPB=1./PB
      PB1=PB+1.
      PB2=PB+1.
      PAA=1./PB2*(UW/VIS)**PB
      PCC=0.5*(SHR/VIS)**PB
      DA=AA/BA
      DDA=0.5*DA*(B/AA-DB/BA)
      BRK=(QA+PCC*DA*AA)/(PAA*AA*DA**PB1)
      SIA=BRK**RPB
      DBRK=((PAA*AA*DA**PB1*PCC*(0.5*DA*B+AA*DDA))-((QA+PCC*DA*AA)*PAA*
     *  (0.5*B*DA*PB1+PB1*AA*DA**PB*DDA)))/(PAA*AA*DA**PB1)**2.
      DSIA=RPB*BRK**(RPB-1.)*DBRK
   11 F=F1-BETI*QI2/A+32.2*AA*(YIR-YA+DX*SFA+DX*SIA)-DQDT*DX
      FP=BETI*QI2*B/A/A+32.2*0.5*B*(YIR-YA+DX*SFA+DX*SIA)+
     &  32.2*AA*(-1.0+DX*DSFA+DX*DSIA)-DBEI*QI2/A
CCC      YAOLD=YA
      YNW=YA-F/FP
CCC      IF(YNW.LE.YBOT) YNW=YBOT
      PYA=YA
      PYN=YNW
      PF=F
      IF(METRIC.EQ.1) THEN
        PYA=YA/3.281
        PYN=YNW/3.281
        PF=PF/115.884
      ENDIF
      IF(JNK.GE.10) WRITE(IPR,12) ITB,I,PYA,PYN,PF
   12 FORMAT(15X,'ITB=',I3,5X,2HI=,I5,5X,'YBWO=',F10.2,5X,
     * 'YBWN=',F10.2,10X,2HF=,F12.3)
      IF(ABS(YNW-YA).LE.0.004) GO TO 20
   13 YA=YNW
      GO TO 101
   20 IF(YA.GT.YTOP1.OR.YA.LT.YBOT1) GO TO 101
      YII=YNW
CCC   20 YII=YNW
CC      IF(YII.LT.YIR) THEN
CC        YII=YIR
CC        YA=YIR
CC      ENDIF
CCC      IF(YII.GE.YTOP .OR. YII.LE.YBOT) GO TO 101
      RETURN
  101 CONTINUE
      IYT=0
      KMX=100
  104 ITB=0
      YMX=YTOP
      YMN=YBOT
      YOLD=0.
  108 YA=0.5*(YMN+YMX)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,I,YA,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      IF(KFLP.EQ.0) GO TO 138
      AA=0.5*(A+AR)
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,I,YA,QKI,
     1 DQKI,BETI,DBEI,0,K1,K2)
      AQK=0.5*(QK+QKI)
      SFA=QA*QA/(AQK*AQK)
      GO TO 139
  138 P=WPRM
      YAA=0.5*(YIR+YA)
      IF(NQCM(J).LT.0) YAA=QA
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,I,YAA,CMU,DCM,K1,K7,K8)
      AA=0.5*(A+AR)
      BA=0.5*(B+BR)
      PA=0.5*(P+PRR)
      RA=AA/BA
      IF(KPRES.EQ.1) RA=AA/PA
      SFA=CMU*CMU*QA*ABS(QA)/(2.208*AA*AA*RA**E43)
  139 SIA=0.
      IF(MUD.NE.1) GO TO 111
      RPB=1./PB
      PB1=PB+1.
      PB2=PB+1.
      PAA=1./PB2*(UW/VIS)**PB
      PCC=0.5*(SHR/VIS)**PB
      DA=AA/BA
      DDA=0.5*DA*(B/AA-DB/BA)
      BRK=(QA+PCC*DA*AA)/(PAA*AA*DA**PB1)
      SIA=BRK**RPB
  111 F=F1-BETI*QI2/A+32.2*AA*(YIR-YA+DX*SFA+DX*SIA)-DQDT*DX
      PYIR=YIR
      PQII=QII
      PYA=YA
      PF=F
      IF(METRIC.EQ.0) GO TO 112
      PYIR=YIR/3.281
      PQII=QII/35.32
      PYA=YA/3.281
      PF=PF/115.884
  112 IF(JNK.GE.10) WRITE(IPR,113) ITB,I,PYIR,PQII,PYA,PF
  113 FORMAT(15X,'ITB=',I3,5X,2HI=,I5,5X,4HYIR=,F10.2,
     *  5X,4HQII=,F12.3,5X,3HYA=,F10.4,10X,2HF=,F12.3)
      IF(F.LT.0.0) YMX=YA
      IF(F.GT.0.0) YMN=YA
      IF(ABS(YA-YOLD).LE.0.004) GO TO 114
      ITB=ITB+1
      YOLD=YA
      IF(ITB.LT.KMX) GO TO 108
  116 WRITE(IPR,117) I
  117 FORMAT(//'BWATER DID NOT CONVERGE; ',
     *' PROGRAM STOPS AT SECTION NO=',I5//)
      STOP
  114 IF((YTOP-YA).GT.0.04) GO TO 120
      IYT=IYT+1
      IF(IYT.GE.2) GO TO 116
      YTOP=YTOP+100.0
      YBOT=YA-0.05
      GO TO 104
CC  120 IF(YA.LT.YIR) YA=YIR
  120 YII=YA
      RETURN
      END
