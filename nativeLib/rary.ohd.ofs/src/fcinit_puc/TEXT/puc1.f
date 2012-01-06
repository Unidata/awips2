C MEMBER PUC1
C  (from old member FCPUC1)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/31/95.11:00:49 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE PUC1(PL,CL)
C.......................................
C     THIS SUBROUTINE PUNCHES PARAMETERS AND CARRYOVER VALUES FOR
C        THE SACRAMENTO SOIL MOISTURE ACCOUNTING OPERATION IN THE
C        FORMAT REQUIRED BY THE INPUT SUBROUTINE PIN1.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL   MAY 1979   VERSION 1
C.......................................
      DIMENSION PL(*),CL(*)
      DIMENSION CHAR(10),RSUM(7)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/PUDFLT/IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc1.f,v $
     . $',                                                             '
     .$Id: puc1.f,v 1.4 2004/05/13 14:25:06 gzhou Exp $
     . $' /
C    ===================================================================
C
C
      DATA BLANK,SUMS,PRST/4H    ,4HSUMS,4HPRST/
      DATA FRZES/4HFRZE/
C.......................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,15H** PUC1 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE.
C.......................................
C     PUNCH CARD NO. 1
      IDT=PL(2)
      WRITE (IPU,901) (PL(I),I=3,7),IDT,(PL(I),I=8,13)
  901 FORMAT (5A4,8X,I2,2X,2A4,1X,A4,7X,2A4,1X,A4)
C.......................................
C     PUNCH CARD NO. 2
      L=PL(15)
      IF (L.EQ.0) GO TO 101
      ISC=L
      CHAR(1)=PL(L)
      CHAR(2)=PL(L+1)
      CHAR(3)=PL(L+2)
      ITSC=PL(L+3)
      ISCO=PL(L+4)
      GO TO 102
  101 DO 103 I=1,3
  103 CHAR(I)=BLANK
      ISC=0
      ITSC=0
  102 L=PL(16)
      IF (L.EQ.0) GO TO 104
      IROC=L
      CHAR(4)=PL(L)
      CHAR(5)=PL(L+1)
      ITROC=PL(L+3)
      IROCO=PL(L+4)
      GO TO 105
  104 CHAR(4)=BLANK
      CHAR(5)=BLANK
      IROC=0
      ITROC=0
  105 L=PL(17)
      IF (L.EQ.0) GO TO 106
      ISM=L
      CHAR(6)=PL(L)
      CHAR(7)=PL(L+1)
      ITSM=PL(L+3)
      GO TO 107
  106 CHAR(6)=BLANK
      CHAR(7)=BLANK
      ISM=0
      ITSM=0
  107 L=PL(18)
      IF (L.EQ.0) GO TO 108
      CHAR(8)=SUMS
      GO TO 109
  108 CHAR(8)=BLANK
  109 L=PL(19)
      IF (L.EQ.0) GO TO 110
      CHAR(9)=PRST
      GO TO 111
  110 CHAR(9)=BLANK
  111 L=PL(24)
      IF(L.EQ.0) GO TO 114
      IFRZE=L
      CHAR(10)=FRZES
      GO TO 115
  114 CHAR(10)=BLANK
      IFRZE=0
  115 WRITE(IPU,902) (CHAR(I),I=1,3),ITSC,(CHAR(I),I=4,9),ITROC,ITSM,
     1CHAR(10)
  902 FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,2X,2A4,1X,A4,1X,A4,3X,I2,3X,I2,
     11X,A4)
C.......................................
C     PUNCH CARD NO. 3
      L=PL(20)
      L=L-1
      IOPTET=PL(L+19)
      WRITE (IPU,903) (PL(L+I),I=1,8),IOPTET,PL(L+20)
  903 FORMAT (20X,2F5.3,2F5.1,4F5.3,I5,F5.3)
C.......................................
C     PUNCH CARD NO. 4
      WRITE (IPU,904) (PL(L+I),I=9,18)
  904 FORMAT (20X,F5.1,F5.2,F5.0,F5.1,F5.0,F5.3,F5.4,2F5.3,F5.2)
C.......................................
C     PUNCH CARD NO. 5
      IET=PL(14)
      L=PL(21)
      L=L-1
      IF (IET.EQ.0) GO TO 112
C
C     PE DATA ARE USED.
      CHAR(1)=PL(IET)
      CHAR(2)=PL(IET+1)
      CHAR(3)=PL(IET+2)
      IETCO=PL(IET+3)
      WRITE (IPU,905) (CHAR(I),I=1,3),(PL(L+I),I=1,12)
  905 FORMAT (2X,2A4,1X,A4,5X,12F4.2)
      GO TO 113
C
C     SEASONEL ET-DEMAND USED.
C
CGZHOU FOR BUG R25-3 04-15-2004
CBASED ON THE DOCUMENT, THE PUNCHCARD LIMITATION
CFOR THE ET-DEMAND CARVE IS F4.1
  112 WRITE (IPU,906) (PL(L+I),I=1,12)
C 906 FORMAT (20X,12F4.2)
  906 FORMAT (20X,12F4.1)
C.......................................
C     PUNCH CARD NO. 6
  113 IRXC=0
      NXCO=PL(23)
      IF(IPDFLT.EQ.1) GO TO 116
      IF(NXCO.GT.0) IRXC=1
  116 WRITE(IPU,907) (CL(I),I=1,6),IRXC
  907 FORMAT(20X,2F5.1,F5.0,F5.1,2F5.0,4X,I1)
      IF(IRXC.EQ.0) GO TO 125
C     PUNCH CARD 6A.
      PSC=0.0
      IF(ISC.GT.0) PSC=CL(ISCO+6)
      PPE=0.0
      IF(IET.GT.0) PPE=CL(IETCO+6)
      DO 117 I=1,7
  117 RSUM(I)=0.0
      IF(IROC.EQ.0) GO TO 120
      J=IROCO+6-1
      DO 118 I=1,7
  118 RSUM(I)=CL(J+I)
  120 L=4
      DO 119 I=1,7
         NDEC=3
         IF (RSUM(I).GT.-100..AND.RSUM(I).LT.-10.) NDEC=0
         IF (RSUM(I).GE.-10..AND.RSUM(I).LT.0.) NDEC=2
         IF (RSUM(I).GT.0..AND.RSUM(I).LT.10.) NDEC=2
         IF (RSUM(I).GT.10..AND.RSUM(I).LT.100.) NDEC=1
         IF (RSUM(I).GE.100.) NDEC=0
         CALL UFF2A(RSUM(I),CHAR(I),1,L,NDEC,0,6,IER)
  119 CONTINUE
      WRITE(IPU,908) (CHAR(I),I=1,7),PPE,PSC
  908 FORMAT(7(1X,A4),5X,2F5.2)
  125 IF(IFRZE.EQ.0) RETURN
C     PUNCH FROZEN GROUND CARDS.
      J=NXCO+7
      CALL FGPUC1(PL(IFRZE),CL(J))
C.......................................
      RETURN
      END
