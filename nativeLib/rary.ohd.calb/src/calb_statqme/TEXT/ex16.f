C MEMBER EX16
C  (from old member MCEX16)
C
      SUBROUTINE EX16(PO,QS,QO,W,ORD)
C.......................................................................
C     THIS IS THE EXECUTION SUBROUTINE FOR THE QME STATISTICS OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   APRIL 1980 VERSION 1
C.......................................................................
      DIMENSION PO(1),QS(1),QO(1),W(1),MO(12),ORD(1)
C     COMMON BLOCKS.
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fwyds'
      INCLUDE 'common/fwydat'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/ex16.f,v $
     . $',                                                             '
     .$Id: ex16.f,v 1.2 1996/07/11 19:21:22 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** EX16 ENTERED)
C.......................................................................
C     CHECK TO SEE IF DEBUG OUTPUT IS NEEDED FOR THIS OPERATION.
      IBUG=0
      IF(IDBALL.GT.0) GO TO 11
      IF(NDEBUG.EQ.0) GO TO 100
      DO 10 I=1,NDEBUG
      IF(IDEBUG(I).EQ.16) GO TO 11
   10 CONTINUE
      GO TO 100
   11 IBUG=1
  100 CONTINUE
C.......................................................................
C     DEBUG OUTPUT - PRINT PO()
      IF(IBUG.EQ.0) GO TO 102
      NPO=PO(28)
      WRITE(IODBUG,910) NPO
  910 FORMAT(1H0,21HCONTENTS OF PO ARRAY.,5X,
     117HNUMBER OF VALUES=,I3)
      WRITE(IODBUG,911) (PO(I),I=1,NPO)
  911 FORMAT(1H0,15F8.3)
  102 CONTINUE
C.......................................................................
C     CHECK TIMING TO DETERMINE CURRENT MONTH.
      CALL MDYH1(IDA,IHR,MONTH,ID,IYEAR,IHOUR,NLSTZ,NOUTDS,IZONE)
      IXWY=PO(21)
      L=10
      DO 110 I=1,12
      MO(I)=L
      L=L+1
  110 IF(L.GT.12) L=1
  112 DO 120 JJ=1,12
      IF(MONTH.EQ.MO(JJ)) GO TO 130
  120 CONTINUE
C
C     STORE DISCHARGE DATA ON SCRATCH FILE.
C
  130 IF(JJ.LE.6) GO TO 135
      MX=7
      IXWY=IXWY+1
      GO TO 136
  135 MX=1
  136 MN=(JJ-MX)*62
  138 READ(IRWY,REC=IXWY) (WY(I),I=1,372)
      IDAY=IDA+1-IDADAT
      LDAY=LDA+1-IDADAT
      DO 140 K=IDAY,LDAY
      ILOC=MN+K
      WY(ILOC)=QS(K)
      LOC=ILOC+31
  140 WY(LOC)=QO(K)
      IF(LDAY.EQ.31) GO TO 145
      NN=LDAY+1
      DO 142 LL=NN,31
      WY(MN+LL)=-999.
  142 WY(MN+LL+31)=-999.
  145 WRITE(IRWY,REC=IXWY) (WY(I),I=1,372)
C
C.......................................................................
C     DEBUG OUTPUT - PRINT DISCHARGES
      IF(IBUG.EQ.0) GO TO 103
      WRITE(IODBUG,912)
  912 FORMAT(1H0,20HSIMULATED DISCHARGES)
      WRITE(IODBUG,911) (QS(I),I=1,31)
      WRITE(IODBUG,913)
  913 FORMAT(1H0,19HOBSERVED DISCHARGES)
      WRITE(IODBUG,911) (QO(I),I=1,31)
  103 CONTINUE
C.......................................................................
C
C     CHECK TO SEE IF THIS MONTH IS END OF YEAR OR END OF RUN.
C
      IF(MONTH.NE.9.AND.LDARUN.NE.LDA) GO TO 800
C
C     IF END OF YEAR OR RUN, READ CARRYOVER DATA FROM SCRATCH FILE.
      IWS=PO(20)
      IREC=IWS/372
      IF(IWS/372*372.NE.IWS) IREC=IREC+1
      IXWY=IFIX(PO(21))+1
      IW=372
      DO 200 I=1,IREC
      IX=IXWY+I
      IF(I.EQ.IREC) IW=IWS-((IREC-1)*372)
      READ(IRWY,REC=IX) (WY(L),L=1,IW)
      DO 190 J=1,IW
      K=(I-1)*372+J
  190 W(K)=WY(J)
  200 CONTINUE
C
C     DETERMINE FIRST AND LAST MONTHS AND YEARS OF RUN.
C
      IH1=IHRRUN
      IF(IH1.EQ.0) IH1=1
      CALL MDYH1(IDARUN,IH1,IMON,IDY,IYEA,IHOU,NLSTZ,NOUTDS,IZONE)
      IH2=LHRRUN
      IF(IH2.EQ.0) IH2=1
      CALL MDYH1(LDARUN,IH2,LMON,LDY,LYEA,LHOU,NLSTZ,NOUTDS,IZONE)
C
C     DETERMINE WORK SPACE LOCATIONS FOR OPTIONAL OUTPUT.
      IQR=PO(17)
      IFR=PO(18)
      EXCED=PO(19)
      IQR1=1
      IQR2=1
      IQR3=1
      IFR1=1
      IQEX=1
      ISIM=1
      IOBS=1
      INUM=381
      IF(IQR.EQ.0) GO TO 210
      IQR1=382
      INUMYR=LYEA-IYEA
      IF(IMON.LE.9) INUMYR=INUMYR+1
      IF(LMON.GT.9) INUMYR=INUMYR+1
      IQR2=INUMYR*4+IQR1
      IQR3=INUMYR*4+IQR2
      INUM=IQR3
  210 IF(IFR.EQ.0) GO TO 220
      IFR1=INUM+1
      INUM=INUM+51
  220 IF(EXCED.LT.0.01) GO TO 230
      IQEX=INUM+1
      ISIM=INUM+21
      IOBS=INUM+41
  230 CONTINUE
C.......................................................................
C     DEBUG OUTPUT - PRINT CARRYOVER
      IF(IBUG.EQ.0) GO TO 104
      WRITE(IODBUG,914)
  914 FORMAT(1H0,65HCARRYOVER VALUES PRIOR TO END OF YEAR COMPUTATIONS (
     *REAL FORMAT):)
      CALL PRTR16(W,IWS)
      WRITE(IODBUG,916)
  916 FORMAT(1H0,15HINTEGER FORMAT:)
      CALL PRTI16(W,IWS)
  104 CONTINUE
C.......................................................................
C
      XKKL=W(IQR3)
C
C     CALL STATISTICS SUBROUTINE
      CALL STAT16(PO,QS,QO,W(1),W(13),W(25),W(37),W(49),W(61),W(73),
     1W(85),W(97),W(109),W(121),W(133),W(145),W(157),W(169),W(181),
     2W(193),W(205),W(212),W(219),W(226),W(233),W(240),W(247),W(254),
     3W(255),W(256),W(257),W(282),W(307),W(332),W(357),W(IQR1),W(IQR2),
     4XKKL,W(IFR1),W(IQEX),W(ISIM),W(IOBS),ORD,IMON,IYEA,LMON,LYEA)
C
C     STORE CARRYOVER ON SCRATCH FILE.
C
      IF(IQR.GT.0) W(IQR3)=XKKL
      JW=1
      IW=372
      DO 400 I=1,IREC
      IX=IXWY+I
      IF(I.EQ.IREC) IW=IWS
      WRITE(IRWY,REC=IX) (W(L),L=JW,IW)
      JW=JW+372
      IW=IW+372
  400 CONTINUE
C.......................................................................
C     DEBUG OUTPUT - PRINT CARRYOVER
      IF(IBUG.EQ.0) GO TO 105
      WRITE(IODBUG,915)
  915 FORMAT(1H0,62HCARRYOVER VALUES AFTER END OF YEAR COMPUTATIONS (REA
     *L FORMAT):)
      CALL PRTR16(W,IWS)
      WRITE(IODBUG,916)
      CALL PRTI16(W,IWS)
  105 CONTINUE
C.......................................................................
C
  800 CONTINUE
      RETURN
      END
