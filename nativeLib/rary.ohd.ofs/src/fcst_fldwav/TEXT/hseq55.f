      SUBROUTINE HSEQ55(PO,JNK,J,I,QII,HSUP,YSI,YMN,YMX,ITS,K1,K2,K9)
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/METR55/METRIC
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/hseq55.f,v $
     . $',                                                             '
     .$Id: hseq55.f,v 1.2 2000/12/19 15:51:17 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'HSEQ55  '  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,I,HSUP,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A1=A
      B1=B
      CALL YCTRD55(J,I,HSUP,YCT1,A1,B1,NCS,PO(LOBS),PO(LOHS),K1,K2,K9)
      FAC=QII*QII/32.2
      F1=FAC/A1+YCT1*A1
      YTOP=YMX
      YBOT=YMN
      LMX=100
    4 ITS=0
      YMN=YBOT
      YMX=YTOP
      YOLD=0.
   10 YA=0.5*(YMN+YMX)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,I,YA,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A2=A
      B2=B
      CALL YCTRD55(J,I,YA,YCT2,A2,B2,NCS,PO(LOBS),PO(LOHS),K1,K2,K9)
      F=F1-FAC/A2-YCT2*A2
      PYA=YA
      PYCT2=YCT2
      IF(METRIC.EQ.0) GO TO 11
      PYA=YA/3.281
      PYCT2=YCT2/3.281
   11 IF(JNK.GE.12) WRITE(IPR,70) I,PYA,F,PYCT2
   70 FORMAT(10X,2HI=,I5,10X,3HYA=,F10.2,10X,2HF=,F10.2,5X,4HYCT=,
     *  F10.2)
      IF(HSUP.GT.YTOP) F=-F
      IF(F.LT.0.0) YMX=YA
      IF(F.GT.0.0) YMN=YA
      IF(ABS(YA-YOLD).LE.0.01) GO TO 20
      ITS=ITS+1
      YOLD=YA
      IF(ITS.LT.LMX) GO TO 10
      WRITE(IPR,17) I
   17 FORMAT(//78HSEQUENT DEPTH DID NOT CONVERGE IN 100 ITERATIONS. PROG
     *RAM STOPS AT SECTION NO=,I5//)
      STOP
   20 CONTINUE
      IF(HSUP.LE.YTOP) GO TO 21
      IF((YTOP-YA) .GT. 0.04) GO TO 21
      YBOT=YA-0.05
      YTOP=YTOP+100.0
      GO TO 4
   21 YSI=YA
      RETURN
      END
