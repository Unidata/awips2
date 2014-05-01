C MEMBER PRC45
C  (from old member FCPRC45)
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 02/09/94.13:03:53 BY $WC30KH
C
      SUBROUTINE PRC45(PO,CO)
C.......................................
C     THIS IS THE PRINT CARRYOVER SUBROUTINE FOR
C     THE DELTA-TS OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        KUANG HSU  - HRL   DEC. 1993
C.......................................
      DIMENSION PO(*),CO(*)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCONIT/IVALUE
      COMMON /FENGMT/ METRIC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc45.f,v $
     . $',                                                             '
     .$Id: prc45.f,v 1.2 1997/04/04 15:41:07 page Exp $
     . $' /
C    ===================================================================
C
C
C
C     DATA STATEMENTS
      DATA SNAME/4HPRC4,4H5   /
      DATA XUNT/4HXUNT/
C.......................................
C     TRACE LEVEL FOR SUBROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,45,IBUG)
C.......................................
C     PRINT CARRYOVER VALUE.
C     IVALUE=1, INITIAL VALUE; IVALUE=0, NOT INITIAL VALUE
      IF(IVALUE.NE.1) GO TO 100
      ICO=PO(9)
      IF(ICO.EQ.1) GO TO 102
 103  WRITE(IPR,901)
  901 FORMAT(1H0,10X,'CARRYOVER SET TO DEFAULT VALUE (-989.) ',
     1'SO THAT INITIAL RATE OF CHANGE VALUE= 0.0')
      GO TO 101
 100  IF ((CO(1).LT.-988.99).AND.(CO(1).GT.-989.01)) GO TO 103
 102  TYPE=PO(4)
      CALL FDCODE(TYPE,UNIT,DIM,MSG,NPDT,TSCALE,NADD,IERR)
      ITH=PO(5)
      PCO=CO(1)
      IXUNIT=PO(10)
      IF(IXUNIT.EQ.1) THEN
         PUNIT=XUNT
         GO TO 900
      ENDIF
      PUNIT=UNIT
      IF(METRIC.EQ.0) THEN
        CALL FCONVT(UNIT,DIM,PUNIT,CMULT,CADD,IER)
        PCO=CO(1)*CMULT+CADD
      END IF
 900  WRITE(IPR,902) PCO,PUNIT,(PO(I),I=2,4),ITH
  902 FORMAT(1H0,10X,'DELTA-TS CARRYOVER=',F10.2,1X,A4/16X,
     148HTHIS IS THE PREVIOUS VALUE OF TIME SERIES (I.D.=,2A4,3X,
     25HTYPE=,A4,3X,3HDT=,I2,1X,7HHOURS).)
  101 CONTINUE
C.......................................
      RETURN
      END
