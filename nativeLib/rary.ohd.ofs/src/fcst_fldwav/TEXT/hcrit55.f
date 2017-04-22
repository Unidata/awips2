      SUBROUTINE HCRIT55(PO,JNK,J,I,Y1,Q1,YMN,YMX,ITC,K1,K2,K9)
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/METR55/METRIC
      COMMON/PRES55/KPRES
      COMMON/PRMS55/DPRM,WPRM
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/hcrit55.f,v $
     . $',                                                             '
     .$Id: hcrit55.f,v 1.2 2000/12/19 15:50:40 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'HCRIT55 ' /

      CALL FPRBUG(SNAME,1,55,IBUG)

C     FIND CRITICAL ELEVATION
      YTOP=YMX
      YBOT=YMN
      IF(Y1.LE.-999.) GO TO 101
C  NEWTON-RAPHSON METHOD
      F=0.
      DO 113 K=1,20
      ITC=K
CCC      FOLD=F
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,I,Y1,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      P=WPRM
      DP=DPRM
      R=A/B
      DR=0.5*(1.-A*DB/B/B)
CCC      SI=0.
CCC      DSI=0.
      F1=A*SQRT(32.2*R)
      F=F1-Q1
      FP=F1*(1.+0.5*DR)/R
CCC      Y1OLD=Y1
      YNW=Y1-F/FP
      PY1=Y1
      PYN=YNW
      IF(METRIC.EQ.1) THEN
        PY1=Y1/3.281
        PYN=YNW/3.281
        PF=PF/115.884
      ENDIF
      IF(JNK.GE.10) WRITE(IPR,112) K,I,PY1,PYN,PF
  112 FORMAT(10X,2HK=,I2,10X,2HI=,I5,10X,'YCRO=',F10.2,10X,'YCRN=',
     *  F10.2,20X,2HF=,F10.1)
      IF(ABS(YNW-Y1).LE.0.004) GO TO 114
  113 Y1=YNW
      GO TO 101
  114 Y1=YNW
      RETURN
  101 CONTINUE
C  BISECTION METHOD
      IYT=0
      LMX=100
   14 ITC=0
      YMN=YBOT
      YMX=YTOP
      YOLD=0.
   15 Y=0.5*(YMX+YMN)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     . PO(LOBSS),J,I,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      F=A*SQRT(32.2*A/B)-Q1
      PY=Y
      PF=F
      IF(METRIC.EQ.1) THEN
        PY=PY/3.281
        PF=PF/35.32
      ENDIF
      IF(JNK.GE.13) WRITE(IPR,70) I,PY,PF
   70 FORMAT(10X,2HI=,I5,10X,2HY=,F10.2,10X,2HF=,F12.3)
      IF(ABS(Y-YOLD).LE.0.004) GO TO 17
      IF(F.GT.0.0) YMX=Y
      IF(F.LT.0.0) YMN=Y
      YOLD=Y
      ITC=ITC+1
      IF(ITC.LE.LMX) GO TO 15
  116 WRITE(IPR,69) I
   69 FORMAT(//'CRITICAL DEPTH COMPUTATION DID NOT CONVERGE; ',
     1' PROGRAM STOPS AT SECTION NO=',I5//)
      STOP
   17 CONTINUE
      IF((YTOP-Y) .GT. 0.04) GO TO 18
      IYT=IYT+1
      IF(IYT.GE.6) GO TO 116
      YBOT=Y-0.05
      YTOP=YTOP+20.0
      GO TO 14
   18 Y1=Y
      RETURN
      END
