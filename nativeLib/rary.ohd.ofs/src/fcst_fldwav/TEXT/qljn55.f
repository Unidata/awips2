      SUBROUTINE QLJN55(PO,T1,LTT1,QLJ,LTQLJ,NB,NJUN,QJ,QD,MRV,DDX,NQL,
     . LQ1,LQN,YU,YD,QDI,JM,I,QLJNU,QLJND,ATRBU,ATRBD,K1,K2,K9,K10)

C
C      THIS SUBROUTINE COMPUTES LATERAL INFLOW FROM TRIBUTARY
C
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'
C
      DIMENSION PO(*),QLJ(*),LTQLJ(*),NB(K1),NJUN(K1),QJ(K1),QD(K2,K1)
      DIMENSION MRV(K1),NQL(K1),LQ1(K10,K1),LQN(K10,K1),DDX(K2,K1)
      DIMENSION YU(K2,K1),YD(K2,K1),QDI(K2,K1),T1(*),LTT1(*)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/qljn55.f,v $
     . $',                                                             '
     .$Id: qljn55.f,v 1.2 2000/12/19 15:54:49 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/ 'QLJN55  '  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      DO 240 J=2,JN
      MJ=MRV(J)
      M=NB(J)
      IF(MJ.NE.JM) GO TO 240
C  FIND ORIGINAL CONFLUENCE REACH
      L1=NQL(MJ)+1
      L2=L1+JN-1
      IR1=0
      IRN=0
      DO 10 LL=L1,L2
        IF(LQ1(LL,MJ).EQ.NJUN(J)) THEN
          IR1=LQ1(LL,MJ)
          IRN=LQN(LL,MJ)-1
          GO TO 20
        ENDIF
   10 CONTINUE
   20 IF(I.GE.IR1.AND.I.LE.IRN) GO TO 250
  240 CONTINUE
C.........................................................................
C  NOT IN CONFLUENCE ... SET FLOW & AREA TO ZERO
      QLJNU=0.
      ATRBU=1.
      QLJND=0.
      ATRBD=1.
      GO TO 300
C.........................................................................
C  IN TRIBUTARY CONFLUENCE ... COMPUTE q
C     DXTRB=CONFLUENCE DISTANCE (ON MAIN RIVER)
C     ATRBU,ATRBD=AREA OF LAST SECTION ON TRIB AT CURRENT & PREV TIME STEP
C     QLJNU,QLJND= TRIB FLOW/UNIT WIDTH (q) AT CURRENT & PREV TIME STEP

  250 IF(I.EQ.IR1) THEN
        DXTRB=0.
        DO 30 L=IR1,IRN
          DXTRB=DXTRB+DDX(L,MJ)
   30   CONTINUE
        IF(NP.LE.-1) THEN
  260     TX=TT
          LT1=LTT1(J)
          LJ=LCAT21(L,J,NQL)
          LLJ=LTQLJ(LJ)-1
          CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
        IF(TX.GE.T1(LT1)) THEN
            QLJNU=QLJ(IT1+LLJ)+(QLJ(IT2+LLJ)-QLJ(IT1+LLJ))*TINP
        ELSE
            QLJNU=QDI(M,J)+(QLJ(1+LLJ)-QDI(M,J))*TX/T1(LT1)
        ENDIF
          TX=TT-DT/TIMF
          IF(TX.LE.0.0) TX=0.0
          CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
        IF(TX.GE.T1(LT1)) THEN
            QLJND=QLJ(IT1+LLJ)+TINP*(QLJ(IT2+LLJ)-QLJ(IT1+LLJ))
        ELSE
            QLJND=QDI(M,J)+(QLJ(1+LLJ)-QDI(M,J))*TX/T1(LT1)
        ENDIF
        ELSE
          QLJNU=QJ(J)
          QLJND=QD(M,J)
        ENDIF
        QLJNU=QLJNU/DXTRB
        QLJND=QLJND/DXTRB
        CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     .   PO(LOBSS),J,M,YU(M,J),PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
        ATRBU=A
        CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     .   PO(LOBSS),J,M,YD(M,J),PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
        ATRBD=A
      ENDIF
C.........................................................................
  300 CONTINUE
      RETURN
      END
