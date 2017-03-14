      SUBROUTINE LOOPR55(PO,NCML,NQCM,J,IL,IR,QIL,QIR,YIL,YIR,DX,QNL,
     * QNR,K1,K2,K7,K8,K9)
C  THIS SUBROUTINE COMPUTES FLOW USING MANNING EQN AND ENERGY SLOPE
C  FOR USE IN LOOP RATING BOUNDARY IN STEADY STATE
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/FLP55/KFLP

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*),NQCM(K1)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/loopr55.f,v $
     . $',                                                             '
     .$Id: loopr55.f,v 1.2 2000/12/18 21:44:42 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'LOOPR55 ' /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      BETL=1.06
      BETR=1.06
      E23=2./3.
      IF(KFLP.EQ.0) GO TO 100
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,IL,YIL,QKL,
     * DQKL,BETL,DBEL,0,K1,K2)
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,IR,YIR,QKR,
     * DQKR,BETR,DBER,0,K1,K2)
  100 CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,IL,YIL,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AL=A
      BL=B
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,IR,YIR,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AR=A
      BR=B
      AA=0.5*(AL+AR)
      GA=1./(G*AA)
      TERM1=(YIL-YIR)/DX
      TERM2=0.0
      TERM3=GA*(BETL*QIL*QIL/AL-BETR*QIR*QIR/AR)/DX
      SF=TERM1+TERM2+TERM3
      IF(KFLP.EQ.0) GO TO 150
      QNL=QKL*SQRT(SF)
      QNR=QKR*SQRT(SF)
      GO TO 200
  150 YY=0.5*(YIL+YIR)
      YQ=YY
      IF(NQCM(J).LT.0) YQ=0.5*(QIL+QIR)
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,IL,YQ,CMN,DCM,K1,K7,K8)
      R23=(AL/BL)**E23
      QNL=1.486*AL*R23*SQRT(SF)/CMN
      R23=(AR/BR)**E23
      QNR=1.486*AR*R23*SQRT(SF)/CMN
  200 RETURN
      END
