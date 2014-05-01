      SUBROUTINE DBDY55(PO,JNK,NCML,NQCM,HS,QD,QU,YD,YU,DDX,KD,SLFI,YJ,
     * KR,STN,LTSTN,T1,LTT1,STT,LTSTT,ST0,C,D,YQD,QYQD,NGAGE,NBJ,ISN,
     * LRT,IRTN,IFR,YUMN,YDI,QDI,NL,L3,L4,MRV,NJUN,KRCH,
     * K1,K2,K6,K7,K8,K9,K28)
C
C      THIS SUBROUTINE SETS UP DOWNSTREAM BOUNDARY CONDITIONS
C
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M655/KTIME,DTHYD,J1
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/NYQDC55/NYQD
      COMMON/DQDT55/DQDTN
      COMMON/FLP55/KFLP
      COMMON/IT55/ITER
      COMMON/PRES55/KPRES
      COMMON/PRMS55/DPRM,WPRM
      COMMON/LOOP55/LOOPKD,LOOPRN
      COMMON/VS55/MUD,IWF,SHR,VIS,UW,PB,SIMUD
      COMMON/IONUM/IN,IPR,IPU
        COMMON/NETWK55/NET

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(1),STN(*),T1(*),LTT1(*),STT(*),LTSTT(*)
      DIMENSION ST0(K28),NQCM(K1),HS(K9,K2,K1),QD(K2,K1),QU(K2,K1)
      DIMENSION YD(K2,K1),YU(K2,K1),DDX(K2,K1),NGAGE(K1)
      DIMENSION KD(K1),SLFI(K2,K1),YJ(K1),QDI(K2,K1),YDI(K2,K1)
      DIMENSION C(1),D(4,1),YQD(K6,K1),KRCH(K2,K1)
      DIMENSION QYQD(K6,K1),IFR(K2,K1),YUMN(K2,K1)
        DIMENSION MRV(K1),NJUN(K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/dbdy55.f,v $
     . $',                                                             '
     .$Id: dbdy55.f,v 1.5 2004/02/04 19:56:39 dsa Exp $
     . $' /
C    ===================================================================
C
C
      J=JJ
C NETWORK-RIVER U/S
      IF (J.GT.JN) THEN
        JIN=MRV(J)
        IIN=NJUN(J)
      YQ1=0.5*(YU(IIN,JIN)+YU(IIN+1,JIN))
      YU(ISN,J)=YQ1
      C(NL)=0.
      D(L3,NL)=1.
      D(L4,NL)=0.
      GO TO 330
        ENDIF
      L1=LRT+1
      N=ISN
      NM=N-1
      LT1=LTT1(J)
      LJ=LCAT21(L1,J,NGAGE)
        IF (MUD.EQ.1) GOTO 260
      IF(IFR(ISN,J).EQ.0 .OR. ISN.EQ.NBJ) GO TO 80
      Y=YU(ISN,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,ISN,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      QN=SQRT(32.2/B*A**3.)
      DQNY=0.5*QN*(3.*B/A-DB/B)
      C(NL)=-(QU(ISN,J)-QN)
      D(L3,NL)=-DQNY
      D(L4,NL)=1.
      GO TO 330
   80 IF(NP.GE.0) GO TO 85
      IF(ISN.NE.IRTN) GO TO 245
      IF(ISN.EQ.NBJ) GO TO 85
C  KD(J)=1 (IN DWAVAC) FOR ALL AUTOMATIC CALIBRATION RUN
      TX=TT
      CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
      L1J=LTSTT(LJ)-1
      IF(TX.GE.T1(LT1)) THEN
        YQ1=STT(IT1+L1J)+(STT(IT2+L1J)-STT(IT1+L1J))*TINP
      ELSE
        YQ1=ST0(LJ)+(STT(IT1+L1J)-ST0(LJ))*TX/T1(LT1)
      ENDIF

      GO TO 86
   85 IF(ISN.NE.IRTN) GO TO 245
      IF (IRTN.LT.NBJ) GOTO 190
      IF(KD(J).GE.3) GO TO 190
      YQ1=YJ(J)
      IF(KD(J).GE.1.OR.(J.EQ.1.AND.KD(J).EQ.0)) THEN
        TCYCL=NU*DTHYD
        IF(DTHYD.LE.0.) TCYCL=T1(NU+LT1-1)
        NCYCL=TT/TCYCL
        TX=TT-TCYCL*NCYCL
        CALL INTERP55(T1(LT1),NU,TX,IT1,IT2,TINP)
        L1J=LTSTN-1
        IF(TT.EQ.TCYCL) THEN 
            YQ1=STN(NU+L1J)
        ELSEIF(TX.GE.T1(LT1)) THEN
          YQ1=STN(IT1+L1J)+(STN(IT2+L1J)-STN(IT1+L1J))*TINP
        ELSE
          IF(KD(1).EQ.1) YQ1=YDI(N,J)+(STN(IT1+L1J)-YDI(N,J))*TX/T1(LT1)
          IF(KD(1).EQ.2) YQ1=QDI(N,J)+(STN(IT1+L1J)-QDI(N,J))*TX/T1(LT1)
        ENDIF
      END IF
      IF(KD(J).EQ.2) THEN
        QU(N,J)=YQ1
        C(NL)=0.
        D(L3,NL)=0.
        D(L4,NL)=1.
        GOTO 330
      END IF
   86 YU(N,J)=YQ1
  180 C(NL)=0.
      D(L3,NL)=1.
      D(L4,NL)=0.

      GO TO 330
  190 KDJ=KD(J)
            IF (IRTN.LT.NBJ) THEN
            KDJ=4
            LOOPKD=0
            IF(KD(J).EQ.7) LOOPKD=1
            ENDIF
      IF(KDJ-4) 200,240,250
  200 Y = YU(N,J)
      IF(Y.LE.YQD(1,J)) THEN
        QN=QYQD(1,J)
        DQNY=0.0
        DQNQ=0.0
        GO TO 300
      END IF
      DO 220 K = 1, NYQD
      KT=K
      IF(Y-YQD(K,J)) 230,230,220
  220 CONTINUE
  230 KL = KT - 1
      IF(ABS(YQD(2,J)).LT.0.01) GO TO 235
      DQNY = (QYQD(KT,J)-QYQD(KL,J))/(YQD(KT,J)-YQD(KL,J))
      QN = QYQD(KL,J)+DQNY*(Y-YQD(KL,J))
      DQNQ=0.
      GO TO 300
  235 DEP=Y-YQD(1,J)
      QN=QYQD(1,J)+QYQD(2,J)*DEP+QYQD(3,J)*DEP*DEP+
     & QYQD(4,J)*DEP*DEP*DEP
      DQNY=QYQD(2,J)+2.*QYQD(3,J)*DEP+3.*QYQD(4,J)*DEP*DEP
      DQNQ=0.
C------------------  D/S  LOOP RATING  (KD=4)  ------------------------
C       NEW METHOD IS USED WHEN LOOPKD=1  (input KD=7)
C----------------------------------------------------------------------
  240 YL=YD(NM,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,NM,YL,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AL=A
      BL=B
      QL=QD(NM,J)
      YR=YD(N,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,N,YR,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AR=A
      BR=B
      QR=QD(N,J)
      AA=0.5*(AL+AR)
      BA=0.5*(BL+BR)
      RA=AA/BA
      IF (KFLP.EQ.0) GOTO 241
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,NM,YL,QKL,
     * DQKL,BETL,DBEL,0,K1,K2)
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,N,YR,QKR,
     * DQKR,BETR,DBER,0,K1,K2)
      QA=0.5*(QL+QR)
      QKA=0.5*(QKL+QKR)
      SF=(QA/QKA)**2
      IF (SF.LE.0.0000001) SF=0.0000001
      Y=YU(N,J)
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,N,Y,QK,DQK,
     * BET,DBE,0,K1,K2)
      QN=QK*SF**0.5
      DQNQ=0.0
      DQNY=DQK*SF**0.5
      GOTO 300
C------------------------------------------------------------------------
241   IF (LOOPKD.EQ.0) GOTO 242
      QA=0.5*(QR+QL)
      HA=0.5*(YR+YL)
      IF (NQCM(J).LT.0) YQ=QA
      IF (NQCM(J).GE.0) YQ=HA
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,NM,YQ,CMD,DCMU,K1,K7,K8)
      SF=(CMD*QA/1.487/AA/RA**0.666667)**2
      IF(KRCH(N,J).EQ.7) SF=SLFI(N,J)
      IF (SF.LE.0.0000001) SF=0.0000001
      Y=YU(N,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,N,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AN=A
      RN=A/B
      IF (NQCM(J).LT.0) YQ=QU(N,J)
      IF (NQCM(J).GT.0) YQ=YU(N,J)
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,N,YQ,CMN,DCMU,K1,K7,K8)
      QN=(2.21*SF)**0.5*AN*RN**0.666667/CMN
      DQNQ=0.0
      DQNY=QU(N,J)/RN
      GOTO 300
C------------------------------------------------------------------------
242   TERM1=(YD(NM,J)-YD(N,J))/DDX(NM,J)
      TERM2=DQDTN/(G*AA)
      BETL=1.06
      BETR=1.06
  244 TERM3=(BETL*QD(NM,J)**2./AL-BETR*QD(N,J)**2./AR)/
     &  (G*AA*DDX(NM,J))
      SF=TERM1+TERM2+TERM3
  245 IF(N.LT.NBJ) SF=(YD(N,J)-YD(N+1,J))/DDX(N,J)
      IF(KR.EQ.1) SF=TERM1
      SDUM=(YUMN(NM,J)-YUMN(N,J))/DDX(NM,J)
      FN=SQRT(SDUM)*100.
      SMIN=SDUM/FN
      IF(SMIN.LE.0.0000001) SMIN=0.0000001
      IF(SF.LE.SMIN) SF=SMIN
      IF(JNK.GT.9.AND.ITER.LE.1) WRITE(IPR,9102) TERM1,TERM2,TERM3,SF,
     . SMIN
 9102 FORMAT(/1X,5E13.5,5X,'(TERM1,TERM2,TERM3,SF,SMIN)')
      PSFY = 0.
      PSFQ=0.
      GO TO 260
  250 IF (MUD.EQ.1) GOTO 260
      SF=SLFI(N,J)
      PSFY=0.
      PSFQ=0.
  260 Y=YU(N,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     * PO(LOBSS),J,N,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      R=(A/B)**(2./3.)
      DR=2./3.*R*(B/A-DB/B)
          IF (KPRES.EQ.1) THEN
          R=(A/WPRM)**(2./3.)
          DR=2./3.*R*(B/A-DPRM/WPRM)
          END IF
      NM=N-1
      DYNXN=0.5*(HS(1,NM,J)-HS(1,N,J))
      DYNX=-DYNXN
      YQ=Y-DYNX
      IF(NQCM(J).LT.0) YQ=QD(N,J)
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,N,YQ,CMU,DCMU,K1,K7,K8)
      PNQ=0.
      IF(NQCM(J).LT.0) PNQ=-DCMU/CMU
      PNH=0.
      IF(NQCM(J).GE.0) PNH=-DCMU/CMU
      QK=CMU
C---------------------  MUD FLOW D/S -----------------------------
      IF (MUD.NE.1) GOTO 290
      HD=A/B
      SI=SIMUD
      SL1=SLFI(NM,J)
      SL2=SLFI(N,J)
      SLN=0.5*(SL1+SL2)
      IF (SL2.GE.1.5*SL1 .OR. SL2.LE.0.5*SL1) SI=SLN
      IF (SI.LT.0.000001) SI=0.000001
      DMIN=SHR/UW/SI
      DDHH=HD/DMIN
         IF (DDHH.LE.1.001) THEN
         C(NL)=0.0
         D(3,NL)=1.0
         D(4,NL)=0.0
         QU(N,J)=0.1
         QD(N,J)=0.1
         GOTO 330
         ENDIF
      PA=(0.74+0.656*PB)*(SHR/VIS)**PB/(PB+1)/(PB+2)
      QN=A*HD*PA*(DDHH-1)**(PB+0.15)
      DQNQ=0.0
      DQNY=QN*(2.0/HD+(PB+0.15)/(DDHH-1.0)/DMIN)
      GOTO 300
C-----------------------------------------------------------------------
  290 QN=1.486/CMU*A*R*SQRT(SF)
      DQNQ=QN*(0.5/SF*PSFQ+PNQ)
      DQNY=QN*(DR/R+B/A+0.5/SF*PSFY+PNH)
  300 C(NL)=-(QU(ISN,J)-QN)
      D(L3,NL)=-DQNY
      D(L4,NL)=1.-DQNQ
  330 RETURN
      END

