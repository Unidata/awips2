C----------------------------------------------------------------------
C  THIS SUBPROGRAME HANDELS BOUNDARY CONDITIONS FOR EXPLICIT ROUTING
C----------------------------------------------------------------------
      SUBROUTINE EXBC55(PO,NNN,J,TE,DTE,IRT1,IRTN,QU,QD,YU,YD,KU,KD,BCU,
     . ST1,LTST1,STN,LTSTN,T1,LTT1,DDX,AU1,AU2,AD1,AD2,U,SFN,CMANG,
     . YQD,QYQD,K1,K2,K6,K9,KN)

      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/FLP55/KFLP

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*),ST1(*),LTST1(*),STN(*),LTSTN(*),T1(*),LTT1(*)
      DIMENSION QU(K2,K1),QD(K2,K1),YU(K2,K1),YD(K2,K1),DDX(K2,K1)
      DIMENSION U(K2,KN),YQD(K6,K1),QYQD(K6,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/exbc55.f,v $
     . $',                                                             '
     .$Id: exbc55.f,v 1.2 2000/12/19 15:48:49 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'EXBC55  '  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

C---------------------   UPSTREAM   -----------------------------------
      DTX=DTE/DDX(IRT1,J)
      DXT=1.0/DTX

      LT1=LTT1(J)-1
      LST1=LTST1(J)-1
      LSTN=LTSTN(J)-1
      
      IF (IRT1.LE.1) GO TO 115
100   QU(IRT1,J)=BCU
      A2=AU1+AU2-U(IRT1+1,1)+DTX*(BCU+QD(IRT1,J)-QD(IRT1+1,J)-
     $     U(IRT1+1,2))
      CALL EXAH55(J,NCS,IRT1,A2,HL,PO(LOHS),PO(LOBS),PO(LOBSS),PO(LOAS),
     $         PO(LOASS),K1,K2,K9)
      YU(IRT1,J)=HL
      GOTO 500

115   JJ1=1
      DO 120 I=1,NU-1
120   IF (T1(I+LT1).LE.TE) JJ1=I
      JJ2=JJ1+1
      TEMP=(TE-T1(JJ1+LT1))/(T1(JJ2+LT1)-T1(JJ1+LT1))
      BCU=ST1(JJ1+LST1)+TEMP*(ST1(JJ2+LST1)-ST1(JJ1+LST1))
      IF (KU.EQ.2) GOTO 100

      IF (KU.EQ.1) THEN
      YU(IRT1,J)=BCU
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     . PO(LOBSS),J,1,BCU,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A2=AT
      QU(IRT1,J)=U(IRT1+1,2)+QD(IRT1+1,J)-QD(IRT1,J)+DXT*
     $           (A2+U(IRT1+1,1)-AU1-AU2) 
      ENDIF
C---------------------   DOWNSTREAM  ----------------------------------
500   CONTINUE 

      IF(KD.EQ.3 .AND. KIT.EQ.2 .AND. NNN.EQ.1) THEN    
      L=IRTN
      DO 520 I=1,K6
      Y=YQD(I,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     . PO(LOBSS),J,L,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
520   YQD(I,J)=AT
      ENDIF

      DTX=DTE/DDX(IRTN-1,J)
      DXT=1.0/DTX      
      
      IF (KD.LE.2) THEN
      JJ1=1
      DO 540 I=1,NU-1
540   IF (T1(I+LT1).LE.TE) JJ1=I
      JJ2=JJ1+1
      TEMP=(TE-T1(JJ1+LT1))/(T1(JJ2+LT1)-T1(JJ1+LT1))
      BCD=STN(JJ1+LSTN)+TEMP*(STN(JJ2+LSTN)-STN(JJ1+LSTN))
      ENDIF

      IF (KD.EQ.1) THEN
      YU(IRTN,J)=BCD                                         
      L=IRTN
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     . PO(LOBSS),J,L,BCD,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A2=AT
      QU(IRTN,J)=QD(IRTN-1,J)+U(IRTN-1,2)-QD(IRTN,J)-DXT*
     $           (U(IRTN-1,1)+A2-AD1-AD2)     

      ELSE IF (KD.EQ.2) THEN
      QU(IRTN,J)=BCD
      A2=AD1+AD2-U(IRTN-1,1)-DTX*(QD(IRTN,J)+BCD-QD(IRTN-1,J)-
     $                            QU(IRTN-1,J))
      CALL EXAH55(J,NCS,IRTN,A2,HL,PO(LOHS),PO(LOBS),PO(LOBSS),
     . PO(LOAS),PO(LOASS),K1,K2,K9)
      YU(IRTN,J)=HL
      
      ELSE IF (KD.EQ.4) THEN
      DDYY=0.25
      H1=YD(IRTN,J)-DDYY
      H2=YD(IRTN,J)+DDYY
      L=IRTN
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     . PO(LOBSS),J,L,H1,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A1=A 
      R1=A/B
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     . PO(LOBSS),J,L,H2,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A2=A
      R2=A/B
      IF (KFLP.EQ.0) GOTO 560
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,L,H1,
     $          QK1,DQK,BET,DBE,0,K1,K2) 
      CALL CONV55(PO(LOQKC),PO(LOHKC),PO(LCBEV),PO(LCNKC),J,L,H2,
     $          QK2,DQK,BET,DBE,0,K1,K2) 
      Q1=QK1*SFN**0.5
      Q2=QK2*SFN**0.5 
      GOTO 580
560   Q1=(2.21*SFN)**0.5*A1*R1**0.666667/CMANG
      Q2=(2.21*SFN)**0.5*A2*R2**0.666667/CMANG
580   AK=(Q2-Q1)/(A2-A1)
      AB=0.5*(-AK*(A1+A2)+(Q1+Q2))
      AC=1.0+DTX*AK
      A2=(AD1+AD2-U(IRTN-1,1)+DTX*(QD(IRTN-1,J)-QD(IRTN,J)+U(IRTN,2)-
     $ AB))/AC
      CALL EXAH55(J,NCS,IRTN,A2,HL,PO(LOHS),PO(LOBS),PO(LOBSS),PO(LOAS),
     $         PO(LOASS),K1,K2,K9)
      YU(IRTN,J)=HL
      QU(IRTN,J)=AK*A2+AB

      ELSE IF (KD.EQ.3) THEN
      JJ1=1
      DO 620 I=1,K6-1
620   IF (QYQD(I,J).LE.QD(IRTN,J)) JJ1=I
      JJ2=JJ1+1
      AK=(QYQD(JJ2,J)-QYQD(JJ1,J))/(YQD(JJ2,J)-YQD(JJ1,J))
      AB=-AK*YQD(JJ1,J)+QYQD(JJ1,J)
      AC=1.0+DTX*AK
      A2=(AD1+AD2-U(IRTN-1,1)+DTX*(QD(IRTN-1,J)-QD(IRTN,J)+U(IRTN,2)-
     $ AB))/AC
      CALL EXAH55(J,NCS,IRTN,A2,HL,PO(LOHS),PO(LOBS),PO(LOBSS),PO(LOAS),
     $         PO(LOASS),K1,K2,K9)
      YU(IRTN,J)=HL
      QU(IRTN,J)=AK*A2+AB

      ELSE
      YU(IRTN,J)=2.0*YU(IRTN-1,J)-YU(IRTN-2,J)
      QU(IRTN,J)=2.0*QU(IRTN-1,J)-QU(IRTN-2,J)
      ENDIF

999   RETURN
      END
