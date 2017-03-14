C------   INTERNAL BOUNDARY HANDLING FOR EXPLICIT ROUTING   ------------
C         FOR A DAM OR BRIDGE BETWEEN L AND L+1, REDEFINEING
C         QU(L,J),QU(L+1,J),YU(L,J),YU(L+1,J)
C-----------------------------------------------------------------------
      SUBROUTINE EXIN55(PO,CO,Z,ST1,LTST1,T1,LTT1,POLH,LTPOLH,ITWT,
     . LTITWT,J,L,TE,DTE,INTYP,YU,YD,QU,QD,DDX,NCML,K1,K2,K7,K8,K9,K15,
     . K16,K19,K20,K21,TFH,HFDD,NFAILD,QBRCH,QOVTP,QOTHR,QDI,YDI)

      COMMON/SS55/ NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M3455/KXP,ICD,ITMAX,KWARM

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*),Z(*),ST1(*),LTST1(*),T1(*),LTT1(*),POLH(*)
      DIMENSION LTPOLH(*),ITWT(*),LTITWT(*),YU(K2,K1)
      DIMENSION YD(K2,K1),QU(K2,K1),QD(K2,K1),DDX(K2,K1)
      DIMENSION TFH(K16,K1),HFDD(K16,K1),NFAILD(K16,K1),QBRCH(K16,K1)
      DIMENSION QOVTP(K16,K1),QOTHR(K16,K1),QDI(K2,K1),YDI(K2,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/exin55.f,v $
     . $',                                                             '
     .$Id: exin55.f,v 1.3 2000/12/19 15:49:16 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'EXIN55  '  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      CALL IDDB55(L,J,PO(LONLAD),PO(LOLAD),KL,K1,K16)
CC      IF ((YD(L,J)+0.0001).GE.HFDD(KL,J)) NFAILD(KL,J)=3
CC      IF (KIT.LE.(KWARM+1)) NFAILD(KL,J)=1
      TF=ABS(TFH(KL,J))
      MF=NFAILD(KL,J)
      IF ((YD(L,J)+0.0001).GE.HFDD(KL,J)) NFAILD(KL,J)=3
              IF (KIT.LE.(KWARM+1)) THEN
              NFAILD(KL,J)=1
              Q2=QBRCH(KL,J)+QOVTP(KL,J)+QOTHR(KL,J)
              QU(L,J)=QDI(L,J)
              QU(L+1,J)=QDI(L+1,J)
              YU(L,J)=YDI(L,J)
              YU(L+1,J)=YDI(L+1,J)
              GOTO 998
              ENDIF
      IF(TF.LE.0.0000001.AND.MF.EQ.3) THEN
        QBRCH(KL,J)=0.0
        QOVTP(KL,J)=0.0
        QOTHR(KL,J)=0.0
        GOTO 999
      ENDIF
      TTEMP=TT
      TT=TE
      DY=0.1
C------------------  UPSTREAM SECTION L  -------------------------------
      DTX=DTE/DDX(L-1,J)
      L1=L-1
      Y=YD(L-1,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $           PO(LOBSS),J,L1,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AD1=AT
      L1=L
      Y=YD(L,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $          PO(LOBSS),J,L1,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AD2=AT
      L1=L-1
      Y=YU(L-1,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $          PO(LOBSS),J,L1,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AU1=AT
      Y=YD(L,J)
      Y1=Y-DY
      Y2=Y+DY
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $          PO(LOBSS),J,L,Y1,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A1=AT
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $          PO(LOBSS),J,L,Y2,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      A2=AT
      QQ=QD(L,J)
      IFL=0 
      ITRS=0      
      II=2*L
      II2=2*L+1
      L1=2
      L2=1
      L3=4
      L4=4      
      YU(L+1,J)=YD(L+1,J) 
C DAM
      IF (INTYP.EQ.1) THEN
      YU(L,J)=Y1                         
      QU(L,J)=QD(L,J)
      NCALL=0
1111  CALL DAM55(PO,CO,Z,ST1,LTST1,T1,LTT1,POLH,LTPOLH,ITWT,LTITWT,
     . Z(LZD),Z(LZC),Z(LZQU),Z(LZQD),Z(LZYU),Z(LZYD),PO(LOHDD),
     . PO(LOHSPD),PO(LOHGTD),PO(LOCSD),PO(LOCGD),PO(LOZBCH),PO(LOCDOD),
     . PO(LCNFLD),Z(LZBBP),Z(LZYBP),Z(LZSQS1),Z(LZSQS2),Z(LZSQO),
     . Z(LZQBCH),Z(LZQOTP),Z(LZQOTR),PO(LOKRCH),PO(LOQGH),PO(LOCLL),
     . PO(LOSPL),PO(LORHI),PO(LORQI),PO(LOTIQH),QDI,QN,DQNYU,DT,IFL,
     . ITRS,II,II2,L1,L2,L3,L4,ITQ,L,J,K1,K2,K7,K8,K9,K15,K16,K19,
     . K20,K21)
      NCALL=NCALL+1
             IF (NCALL.EQ.1) THEN 
             Q1=QN
             YU(L,J)=Y2
             GOTO 1111
             ENDIF
      Q2=QN       
C  BRIDGE 
      ELSE
      DH=YD(L,J)-YD(L+1,J)
      YUL=Y1       
      YUR=YUL-DH
      YDR=YD(L+1,J)
      NCALL=0
2222  CALL BRIDGE55(PO,Z,NCML,PO(LONQCM),L,J,II,II2,L1,L2,L3,L4,YUL,YUR,
     . YDR,PO(LCDDX),PO(LOEBE1),PO(LOEBE2),PO(LOBRGW),PO(LOCDBR),Z(LZD),
     . Z(LZC),Z(LZQU),PO(LCNFLD),Z(LZBBP),Z(LZYBP),Z(LZSQW),Z(LZSQS1),
     . Z(LZSQO),Z(LZQBCH),Z(LZQOTP),Z(LZQOTR),PO(LOHS),QTS,TT,QQ,
     . PO(LOZBCH),PO(LCTOPN),PO(LOEBW2),PO(LOEBW1),K1,K2,K7,K8,K9,K15,
     . K16)
      NCALL=NCALL+1
             IF (NCALL.EQ.1) THEN
             Q1=QTS 
             YUL=Y2 
             YUR=YUL-DH
             GOTO 2222
             ENDIF
      Q2=QTS       
      ENDIF

      AK=(Q2-Q1)/(A2-A1)
      AB=0.5*(-AK*(A1+A2)+(Q1+Q2))
      AC=1.0+DTX*AK
      AU2=(AD1+AD2-AU1+DTX*(QD(L-1,J)+QU(L-1,J)-QD(L,J)-AB))/AC
      QU(L,J)=AK*AU2+AB
      CALL EXAH55(J,NCS,L,AU2,HL,PO(LOHS),PO(LOBS),PO(LOBSS),PO(LOAS),
     $         PO(LOASS),K1,K2,K9)
      YU(L,J)=HL
C------------------  DOWNSTREAM SECTION L+1  ---------------------------
100   QU(L+1,J)=QU(L,J)
      DTX=DTE/DDX(L+1,J)
      L1=L+1
      Y=YD(L+1,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $          PO(LOBSS),J,L1,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AD1=AT
      L1=L+2
      Y=YD(L+2,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $           PO(LOBSS),J,L1,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AD2=AT
      L1=L+2
      Y=YU(L+2,J)
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $          PO(LOBSS),J,L1,Y,PO(LCHCAV),PO(LCIFCV),K1,K2,K9)
      AU2=AT
      AU1=AD1+AD2-AU2+DTX*(QD(L+1,J)+QU(L+1,J)-QD(L+2,J)-QU(L+2,J))
      CALL EXAH55(J,NCS,L+1,AU1,HL,PO(LOHS),PO(LOBS),PO(LOBSS),PO(LOAS),
     $         PO(LOASS),K1,K2,K9)
      YU(L+1,J)=HL 
C-----------------------------------------------------------------------
 998  IF (ABS(Q2).LE.1.0) GOTO 999
      QFACT=QU(L,J)/Q2
      QBRCH(KL,J)=QFACT*QBRCH(KL,J)
      QOVTP(KL,J)=QFACT*QOVTP(KL,J)
      QOTHR(KL,J)=QFACT*QOTHR(KL,J)
      TT=TTEMP
999   RETURN
      END
