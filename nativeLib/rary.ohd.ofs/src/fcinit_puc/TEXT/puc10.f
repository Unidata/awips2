C MEMBER PUC10
C  (from old member FCPUC10)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/31/95.09:31:15 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE PUC10(PO,CO)
C
C.......................................
C     THIS IS THE PUNCHED CARD SUBROUTINE FOR THE ADD/SUBTRACT
C        OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION PO(*),CO(*)
      DIMENSION CHAR(3)
C
C     COMMON BLOCKS.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/PUDFLT/IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc10.f,v $
     . $',                                                             '
     .$Id: puc10.f,v 1.2 1996/01/16 23:01:54 page Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS.
      DATA SUB,YES,B3/3HSUB,3HYES,3H   /
C.......................................
C     TRACE LEVEL=1. - NO DEBUG OUTPUT.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PUC10 ENTERED)
C.......................................
C     GET CONTROL VARIABLES.
      IADD=PO(2)
      ITB=PO(6)
      ITA=PO(10)
      ICO=PO(11)
      NEG=PO(12)
C.......................................
C     SET VALUES TO BE PUNCHED.
      ADDSUB=B3
      IF(IADD.EQ.0) ADDSUB=SUB
      ANEG=B3
      IF(NEG.EQ.1) ANEG=YES
C.......................................
C     PUNCH CARD INPUT
      IF(ICO.EQ.0) GO TO 100
      IF(IPDFLT.EQ.0) GO TO 101
C
C     NO CARRYOVER OR DEFAULT VALUE USED.
  100 WRITE(IPU,901) (PO(I),I=3,5),ITB,(PO(I),I=7,9),ITA,ADDSUB,ANEG
  901 FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,2X,A3,2X,A3,7X,A3,
     12A4,A2)
      RETURN
C
C     ACTUAL CARRYOVER VALUE IS TO BE PUNCHED.
  101 NDEC=9
      IF (CO(1).GT.-10000000..AND.CO(1).LE.-1000000.) NDEC=1
      IF (CO(1).GT.-1000000..AND.CO(1).LE.-100000.) NDEC=2
      IF (CO(1).GT.-100000..AND.CO(1).LE.-10000.) NDEC=3
      IF (CO(1).GT.-10000..AND.CO(1).LE.-1000.) NDEC=4
      IF (CO(1).GT.-1000..AND.CO(1).LE.-100.) NDEC=5
      IF (CO(1).GT.-100..AND.CO(1).LE.-10.) NDEC=6
      IF (CO(1).GT.-10..AND.CO(1).LT.0.) NDEC=7
      IF (CO(1).GE.1..AND.CO(1).LT.10.) NDEC=8
      IF (CO(1).GE.10..AND.CO(1).LT.100.) NDEC=7
      IF (CO(1).GE.100..AND.CO(1).LT.1000.) NDEC=6
      IF (CO(1).GE.1000..AND.CO(1).LT.10000.) NDEC=5
      IF (CO(1).GE.10000..AND.CO(1).LT.100000.) NDEC=4
      IF (CO(1).GE.100000..AND.CO(1).LT.1000000.) NDEC=3
      IF (CO(1).GE.1000000..AND.CO(1).LT.10000000.) NDEC=2
      IF (CO(1).GE.10000000..AND.CO(1).LT.100000000.) NDEC=1
      CALL UFF2A(CO(1),CHAR,1,10,NDEC,0,6,IERR)
      WRITE(IPU,901) (PO(I),I=3,5),ITB,(PO(I),I=7,9),ITA,ADDSUB,ANEG,
     1YES,CHAR
C.......................................
      RETURN
      END
