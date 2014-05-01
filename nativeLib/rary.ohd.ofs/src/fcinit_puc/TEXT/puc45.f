C MEMBER PUC45
C  (from old member FCPUC45)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/31/95.09:32:24 BY $WC21DT
C
C @PROCESS LVL(77)
C
C
      SUBROUTINE PUC45(PO,CO)
C.......................................
C     THIS IS THE PUNCHED CARD SUBROUTINE FOR
C     THE DELTA-TS OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        KUANG HSU  -  HRL   DEC. 1993
C.......................................
      DIMENSION PO(*),CO(*)
      DIMENSION SNAME(2)
      DIMENSION CHAR(3)
C
C     COMMON BLOCKS.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/PUDFLT/IPDFLT
      COMMON /FENGMT/ METRIC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc45.f,v $
     . $',                                                             '
     .$Id: puc45.f,v 1.3 1997/04/06 12:46:12 page Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HPUC4,4H5   /
      DATA YES/3HYES/,XUNT/4HXUNT/,BLANK/4H    /
C.......................................
C     TRACE LEVEL FOR SUBROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,45,IBUG)
C.......................................
C     GET CONTROL VARIABLES.
      ITH=PO(5)
C.......................................
C     PUNCH CARD INPUT
      IUNIT=PO(10)
      AUNT=BLANK
      IF(IUNIT.EQ.1) AUNT=XUNT
      IF(IPDFLT.EQ.0) GO TO 101
C
C     DEFAULT CARRYOVER USED

 100  WRITE(IPU,901) (PO(I),I=2,4),ITH,(PO(I),I=6,8),AUNT
 901  FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,15X,1X,A4)
      RETURN
C
C     ACTUAL CARRYOVER VALUE IS TO BE PUNCHED.
 101  IF ((CO(1).LT.-988.99).AND.(CO(1).GT.-989.01)) GO TO 100
      TYPE=PO(4)
      CALL FDCODE(TYPE,UNIT,DIM,MSG,NPDT,TSCALE,NADD,IERR)
      PCO=CO(1)
      PUNIT=UNIT
      IF(METRIC.EQ.0) THEN
        CALL FCONVT(UNIT,DIM,PUNIT,CMULT,CADD,IER)
        PCO=CO(1)*CMULT+CADD
      END IF
      NDEC=9
      IF (PCO.GT.-10000000..AND.PCO.LE.-1000000.) NDEC=1
      IF (PCO.GT.-1000000..AND.PCO.LE.-100000.) NDEC=2
      IF (PCO.GT.-100000..AND.PCO.LE.-10000.) NDEC=3
      IF (PCO.GT.-10000..AND.PCO.LE.-1000.) NDEC=4
      IF (PCO.GT.-1000..AND.PCO.LE.-100.) NDEC=5
      IF (PCO.GT.-100..AND.PCO.LE.-10.) NDEC=6
      IF (PCO.GT.-10..AND.PCO.LT.0.) NDEC=7
      IF (PCO.GE.1..AND.PCO.LT.10.) NDEC=8
      IF (PCO.GE.10..AND.PCO.LT.100.) NDEC=7
      IF (PCO.GE.100..AND.PCO.LT.1000.) NDEC=6
      IF (PCO.GE.1000..AND.PCO.LT.10000.) NDEC=5
      IF (PCO.GE.10000..AND.PCO.LT.100000.) NDEC=4
      IF (PCO.GE.100000..AND.PCO.LT.1000000.) NDEC=3
      IF (PCO.GE.1000000..AND.PCO.LT.10000000.) NDEC=2
      IF (PCO.GE.10000000..AND.PCO.LT.100000000.) NDEC=1
      CALL UFF2A(PCO,CHAR,1,10,NDEC,0,6,IERR)
      WRITE(IPU,902) (PO(I),I=2,4),ITH,(PO(I),I=6,8),
     1YES,CHAR,AUNT
 902  FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,2X,A3,2A4,A2,1X,A4)
C.......................................
      RETURN
      END
