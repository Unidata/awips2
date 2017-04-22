C-----------------------------------------------------------------------
C  DYNAMIC ROUTING BY UPWIND EXPLICIT SCHEME DEVELOPED BY DR.MING JIN
C-----------------------------------------------------------------------
      SUBROUTINE EXPT55(PO,CO,Z,QL,LTQL,T1,LTT1,ST1,LTST1,STN,LTSTN,
     . POLH,LTPOLH,ITWT,LTITWT,J,QU,QD,YU,YD,IRT1,IRTN,U,F,S,G1,G2,PJ,
     . RJ,HS,BS,BSS,AS,ASS,DDX,NCML,NQH,KU,KD,NQL,FKEC,KRCH,QDI,YDI,
     . YUMN,SLOPE,K1,K2,K3,K6,K7,K8,K9,K10,K15,K16,K19,K20,K21,KN,X,
     . XLOS,QLOS,ALOS,QBASE)

      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/SS55/ NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/M3455/KXP,ICD,ITMAX,KWARM
      COMMON/EXP55/DTEXP
      COMMON/FLP55/KFLP
      COMMON/KREV55/KREVRS

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'
      INCLUDE 'common/ionum'

      DIMENSION PO(*),CO(*),Z(*),T1(*),LTT1(*),QL(*),LTQL(*),ST1(*)
      DIMENSION LTST1(*),STN(*),LTSTN(*),QU(K2,K1)
      DIMENSION QD(K2,K1),YU(K2,K1),YD(K2,K1),U(K2,KN),F(K2,KN)
      DIMENSION S(K2,KN),G1(KN,KN,K2),G2(KN,KN,K2),PJ(K9,K2,K1)
      DIMENSION RJ(K9,K2,K1),HS(K9,K2,K1),BS(K9,K2,K1),BSS(K9,K2,K1)
      DIMENSION AS(K9,K2,K1),ASS(K9,K2,K1),DDX(K2,K1),NQL(K1)
      DIMENSION FKEC(K2,K1),KRCH(K2,K1),QDI(K2,K1),YDI(K2,K1)
      DIMENSION YUMN(K2,K1),SLOPE(K2,K1),X(K2,K1),XLOS(2,K1)
      DIMENSION QLOS(K1),ALOS(K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/expt55.f,v $
     . $',                                                             '
     .$Id: expt55.f,v 1.3 2002/02/11 13:21:20 michaelo Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'EXPT55  '  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

C--------------------  CHECK COURANT CONDITION  ------------------------
         IF (DTEXP.GT.0.0) THEN
         DTE=DTEXP*3600.0
         GOTO 50
         ENDIF
      CRANT=ABS(DTEXP)
      TMIN=100000.0
      DO 40 L=IRT1,IRTN-1
      HL=YD(L,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $          PO(LOBSS),J,L,HL,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      TEMP=DDX(L,J)/(QD(L,J)/A+(G*A/B)**0.5)
40    IF (TEMP.LE.TMIN) TMIN=TEMP
      DTE=CRANT*TMIN
C-----------------------------------------------------------------------
50    BCU1=QD(IRT1,J)
      BCU2=QU(IRT1,J)
      TE=TT-DT/3600.0
      NEXP=INT(DT/DTE)
      IF (NEXP.LE.1) NEXP=1
      DTE=DT/NEXP
      DDTE=DTE/3600.0
      DO 100 L=IRT1,IRTN
      YU(L,J)=YD(L,J)
100   QU(L,J)=QD(L,J)
C-------------------    p(j),  r(j),   SLOPE    -------------------------
      IF (KIT.GT.2) GOTO 180
      DO 110 L=IRT1,IRTN-1
      G1(1,1,L)=(HS(1,L,J)-HS(1,L+1,J))/ABS(DDX(L,J))
      KRA=KRCH(L,J)
      IF (KRA.GE.10 .AND. KRA.LE.35) G1(1,1,L)=G1(1,1,L-1)
110   CONTINUE
      G1(1,1,IRTN)=G1(1,1,IRTN-1)
      DO 120 L=IRT1,IRTN
      PJ(1,L,J)=0.0
      RJ(1,L,J)=0.0
      DO 120 I=1,NCS-1
      DH=HS(I+1,L,J)-HS(I,L,J)
      PJ(I+1,L,J)=PJ(I,L,J)+AS(I,L,J)*DH+DH*DH*(BS(I+1,L,J)+2.0*
     $            BS(I,L,J))/6.0
120   CONTINUE
      DO 150 L=IRT1+1,IRTN-1
      SLOPE(L,J)=0.5*(G1(1,1,L-1)+G1(1,1,L))
      DO 150 I=2,NCS
      YI=HS(I,L,J)-HS(1,L,J)
      NCOM=NINT(YI)*5
      IF (YI.LE.1.5) NCOM=5
      DY=YI/NCOM
      TEMP=0.0
      DO 140 K=1,NCOM
      YK=K*DY
      H=HS(1,L,J)+YK
      H1=HS(1,L-1,J)+YK
      H2=HS(1,L+1,J)+YK
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $        PO(LOBSS),J,L-1,H1,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      B1=B
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $        PO(LOBSS),J,L+1,H2,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      B2=B
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $        PO(LOBSS),J,L,H,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      DBDX=0.5*((B-B1)/DDX(L-1,J)+(B2-B)/DDX(L,J))
      FY=(YI-YK)*DBDX
140   TEMP=TEMP+FY*DY
      RJ(I,L,J)=TEMP
150   CONTINUE               
      SLOPE(IRT1,J)=G1(1,1,IRT1)
      SLOPE(IRTN,J)=G1(1,1,IRTN)
      DO 160 I=1,NCS
      RJ(I,IRT1,J)=RJ(I,IRT1+1,J)
      RJ(I,IRTN,J)=RJ(I,IRTN-1,J)
160   CONTINUE 
180   DO 500 NNN=1,NEXP
      TE=TE+DDTE
      IF (TE.LE.0.0 .OR. KIT.LE.(KWARM+1)) TE=0.0
      BCU=BCU1+(BCU2-BCU1)*NNN/NEXP
      DO 190 L=IRT1,IRTN
      YD(L,J)=YU(L,J)
190   QD(L,J)=QU(L,J)
C----------   TEMPARARY STOGAGE A,B,AT,BT,SF  AT TIME t(D)  ------------
      DO 200 L=IRT1,IRTN
      HL=YD(L,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $       PO(LOBSS),J,L,HL,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      G2(1,1,L)=A
      G2(1,2,L)=AT
      IF (L.EQ.IRT1) AU1=AT
      IF (L.EQ.(IRT1+1)) AU2=AT
      IF (L.EQ.(IRTN-1)) AD1=AT
      IF (L.EQ.IRTN) AD2=AT
      YU(L,J)=B 
      IF (KFLP.EQ.0) GOTO 200
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,L,HL,
     $          QK,DQK,BET,DBE,0,K1,K2) 
      G2(2,1,L)=QK
200   CONTINUE

      DO 220 L=IRT1,IRTN-1
      IF (KFLP.EQ.0) GOTO 210
      QKA=0.5*(G2(2,1,L)+G2(2,1,L+1))
      QA=0.5*(QD(L,J)+QD(L+1,J))
      SF=(QA/QKA)**2
      GOTO 220
210   AA=0.5*(G2(1,1,L)+G2(1,1,L+1))
      BA=0.5*(YU(L,J)+YU(L+1,J))
      RA=AA/BA
      QA=0.5*(QD(L,J)+QD(L+1,J))
      HA=0.5*(YD(L,J)+YD(L+1,J))
        IF(NQH.LT.0) YQ=QA
        IF(NQH.GE.0) YQ=HA
      CALL FRICT55(NCML,PO(LOCM),PO(LOYQCM),J,L,YQ,CMD,DCMU,K1,K7,K8)
      IF (L.EQ.(IRTN-1)) CMANG=CMD
      SF=(CMD*QA/1.487/AA/RA**0.666667)**2
220   QU(L,J)=SF                
      QU(IRTN,J)=QU(IRTN-1,J)
      SFN=QU(IRTN,J)
C-------------------    U    F(U)     S(U)     --------------------------
      DO 280 L=IRT1,IRTN
      U(L,1)=G2(1,2,L)
      U(L,2)=QD(L,J)

      JJ1=1
      DO 260 I=1,NCS-1
      HL=YD(L,J)
260   IF (HL.GE.HS(I,L,J)) JJ1=I
      DHH=HL-HS(JJ1,L,J)
      DH1=HS(JJ1+1,L,J)-HL
      DH2=HS(JJ1+1,L,J)-HS(JJ1,L,J)
      B1=BS(JJ1,L,J)
      B2=BS(JJ1+1,L,J)
      A1=AS(JJ1,L,J)
      P=PJ(JJ1,L,J)+A1*DHH+(B2*DHH**3+B1*DH1**3+
     $  3.0*B1*DH2**2*DHH-B1*DH2**3)/6.0/DH2
      R=RJ(JJ1,L,J)+(RJ(JJ1+1,L,J)-RJ(JJ1,L,J))*DHH/DH2
      F(L,1)=U(L,2)
      F(L,2)=U(L,2)**2/G2(1,1,L)+P*G
      QLU=0.0
      IF(NQL(J).LE.0) GO TO 270
      TEMP=TT
      TT=TE
      CALL QLTS55(T1,LTT1,QL,LTQL,CO(LXQLI),L,NQL,PO(LOLQ1),PO(LCLQN),
     . QLTSU,QLTSD,K1,K10)
      TT=TEMP
      QLU=QLTSU
270   QLOSS=0.0
      IF (ABS(QLOS(J)).LE.0.0001) GOTO 275
      AN=ALOS(J)
      XL=(X(L,J)-XLOS(1,J))/(XLOS(2,J)-XLOS(1,J))
      IF (XL.LE.0.01 .OR. XL.GT.1.0) GOTO 275
      BTAX=AN*0.01*QLOS(J)*XL**(AN-1.0)/
     $ (1.0+0.01*QLOS(J)*XL**AN)/(XLOS(2,J)-XLOS(1,J))/5279.0
      QLOSS=(QD(L,J)-QBASE)*BTAX
      IF (QD(L,J).LE.QBASE) QLOSS=0.0
275   SE=0.0
       IF (L.LT.IRTN) THEN
       SE=FKEC(L,J)*((QD(L+1,J)/G2(1,1,L+1))**2-(QD(L,J)/G2(1,1,L))**2)/
     $    (2.0*G*DDX(L,J))
       ENDIF
      SF=QU(L,J)
      S0=SLOPE(L,J)
          IF (L.GT.IRT1 .AND. L.LT.IRTN) THEN
          SF=0.5*(QU(L-1,J)+QU(L,J))
          S0=0.5*SLOPE(L,J)+0.2*SLOPE(L-1,J)+0.3*SLOPE(L+1,J)
          ENDIF
      S(L,1)=-QLU-QLOSS
      S(L,2)=G*G2(1,1,L)*(SF+SE-S0)-R*G
280   CONTINUE
C-----------------------   G1=G-(L+1/2)   ------------------------------
      DO 300 L=IRT1,IRTN-1
      YA=G2(1,1,L)/YU(L,J)
      YB=G2(1,1,L+1)/YU(L+1,J)
      VA=U(L,2)/G2(1,1,L)
      VB=U(L+1,2)/G2(1,1,L+1)
      CA=(G*YA)**0.5
      CB=(G*YB)**0.5
      ATEMP=0.5
      V=ATEMP*VA+(1.0-ATEMP)*VB
      C=(YB**0.5*CA+YA**0.5*CB)/(YA**0.5+YB**0.5)
      CHA1=0.5*(1.0-SIGN(1.0,V+C))
      CHA2=0.5*(1.0-SIGN(1.0,V-C))
      TA=0.5*(CHA1-CHA2)
      TB=0.5*(CHA1+CHA2)
      G1(1,1,L)=TB-TA*V/C
      G1(1,2,L)=TA/C
      G1(2,1,L)=TA*(C*C-V*V)/C
      G1(2,2,L)=TA*V/C+TB
300   CONTINUE                                          
      DO 320 I1=1,2
      DO 320 I2=1,2
320   G1(I1,I2,IRTN)=G1(I1,I2,IRTN-1)
C-----------------    G2=G+(L-1/2)    ----------------------------------
      DO 350 L=IRTN,IRT1+1,-1
      YA=G2(1,1,L)/YU(L,J)
      YB=G2(1,1,L-1)/YU(L-1,J)
      VA=U(L,2)/G2(1,1,L)
      VB=U(L-1,2)/G2(1,1,L-1)
      CA=(G*YA)**0.5
      CB=(G*YB)**0.5
      ATEMP=0.5
      V=ATEMP*VA+(1.0-ATEMP)*VB
      C=(YA**0.5*CB+YB**0.5*CA)/(YA**0.5+YB**0.5)
      CHA1=0.5*(1.0+SIGN(1.0,V+C))
      CHA2=0.5*(1.0+SIGN(1.0,V-C))
      TA=0.5*(CHA1-CHA2)
      TB=0.5*(CHA1+CHA2)
      G2(1,1,L)=TB-TA*V/C
      G2(1,2,L)=TA/C
      G2(2,1,L)=TA*(C*C-V*V)/C
      G2(2,2,L)=TA*V/C+TB
350   CONTINUE
      DO 380 I1=1,2
      DO 380 I2=1,2
380   G2(I1,I2,IRT1)=G2(I1,I2,IRT1+1)
C-----------------------------------------------------------------------
      DO 400 L=IRT1+1,IRTN-1
      DTX1=DTE/DDX(L-1,J)
      DTX2=DTE/DDX(L,J)
      DO 400 I=1,2
      EA=G2(I,1,L)*(F(L,1)-F(L-1,1))+G2(I,2,L)*(F(L,2)-F(L-1,2))
      EB=G1(I,1,L)*(F(L+1,1)-F(L,1))+G1(I,2,L)*(F(L+1,2)-F(L,2))
      U(L,I)=U(L,I)-DTX1*EA-DTX2*EB-DTE*S(L,I)
400   CONTINUE                           
C---------------   TRANSFERING U TO QU AND YU   ------------------------
      DO 420 L=IRT1+1,IRTN-1
      QU(L,J)=U(L,2)
      CALL EXAH55(J,NCS,L,U(L,1),HL,HS,BS,BSS,AS,ASS,K1,K2,K9)
420   YU(L,J)=HL
C---------------   BOUNDARY CONDITION  ----------------------------------
      CALL EXBC55(PO,NNN,J,TE,DTE,IRT1,IRTN,QU,QD,YU,YD,KU,KD,BCU,
     . ST1,LTST1,STN,LTSTN,T1,LTT1,DDX,AU1,AU2,AD1,AD2,U,SFN,CMANG,
     . PO(LOYQD),PO(LOQYQD),K1,K2,K6,K9,2)
C----------------  INTERNAL BOUNDARIES (DAM OR BRIDGE)  -----------------
      DO 460 L=IRT1+1,IRTN-1
      KRA=KRCH(L,J)
      IF (KRA.LT.10 .OR. KRA.GT.35) GOTO 460
      INTYP=1
      IF (KRA.EQ.35) INTYP=2
      CALL EXIN55(PO,CO,Z,ST1,LTST1,T1,LTT1,POLH,LTPOLH,ITWT,LTITWT,J,L,
     . TE,DTE,INTYP,YU,YD,QU,QD,DDX,NCML,K1,K2,K7,K8,K9,K15,K16,K19,K20,
     . K21,PO(LOTFH),PO(LOHFDD),PO(LCNFLD),Z(LZQBCH),Z(LZQOTP),
     . Z(LZQOTR),QDI,YDI)
460   CONTINUE
C    *******************  BLOW UP CHECK  *************************
      QMAX=10.0**12   
470   DO 485 L=IRT1,IRTN
      HMIN=HS(1,L,J)
        IF (QU(L,J).GE.QMAX .OR. YU(L,J).LE.HMIN) THEN
        WRITE(IPR,*)
        WRITE(IPR,*) ' DT OR COURANT NUMBER FOR EXPLICIT IS TOO BIG FOR'
        WRITE(IPR,*) ' TT=',TT
        WRITE(IPR,*) ' L =',L
        WRITE(IPR,*) ' X =',X(L,J)
        WRITE(IPR,*) ' PROGRAM STOPPED'
        STOP
        END IF
485   CONTINUE      
C    LOW FLOW FILTER 
      IF (KREVRS.EQ.1) GOTO 500
      DO 490 L=IRT1,IRTN
      IF (QU(L,J).LT.QDI(L,J)) QU(L,J)=QDI(L,J)
490   IF (YU(L,J).LT.YUMN(L,J)) YU(L,J)=YUMN(L,J)
C-----------------------------------------------------------------------      
500   CONTINUE
      RETURN
      END
