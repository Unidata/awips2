C MEMBER TAB12
C  (from old member FCTAB12)
C
      SUBROUTINE TAB12(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,NWORK,NDD,LWORK,
C                             LAST UPDATE: 01/03/95.10:04:25 BY $WC21DT
C
     1   IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'INSQPLOT'
C     OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   DECEMBER 1979
C.......................................
      DIMENSION TS(MTS),PO(1)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab12.f,v $
     . $',                                                             '
     .$Id: tab12.f,v 1.1 1995/09/17 18:49:03 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB1,4H2   /
C.......................................
C     TRACE LEVEL =1 -- DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,12,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES.
      NPLOT=PO(7)
      LENGTH=NPLOT+7
      IOPT=PO(8)
      LRC=PO(11)
      IDT=0
C.......................................
C     CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  100 TO(1)=12
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      LRS=0
C.......................................
C     FIND LOCATION OF RAIN+MELT TIME SERIES.
      LD=0
      IF(IOPT.EQ.0) GO TO 101
      IF(IOPT.EQ.2) GO TO 101
      ITD=PO(15)
      CALL CKINPT(PO(12),PO(14),ITD,LD,TS,MTS,IERR)
      CALL FCLCD(IDT,ITD)
      LRS=LRS+7
  101 TO(4)=LD
C.......................................
C     FIND LOCATION OF RUNOFF TIME SERIES.
      LD=0
      IF(IOPT.LT.2) GO TO 102
      ITD=PO(15+LRS)
      CALL CKINPT(PO(12+LRS),PO(14+LRS),ITD,LD,TS,MTS,IERR)
      CALL FCLCD(IDT,ITD)
      LRS=LRS+7
  102 TO(5)=LD
C.......................................
C     FIND LOCATION OF EACH TIME SERIES TO BE PLOTTED.
      IF(NPLOT.GT.0) GO TO 105
      IUSET=0
      LWORK=0
      IDT=0
      RETURN
  105 DO 106 N=1,NPLOT
      ITD=PO(15+LRS)
      CALL CKINPT(PO(12+LRS),PO(14+LRS),ITD,LD,TS,MTS,IERR)
      CALL FCLCD(IDT,ITD)
      LRS=LRS+8
      TO(7+N)=LD
  106 CONTINUE
C.......................................
C     STORE LOCATION OF WORKING SPACE AND COMPUTE LENGTH.
      IF (LRC.GT.0) GO TO 107
      TO(6)=0
      GO TO 110
  107 TO(6)=LPO+LRC-1
  110 TO(7)=NWORK
      LWORK=101+4*NPLOT
      I=PO(9)
      IF (I.GT.0) LWORK=LWORK+NDD
      IUSET=LENGTH
C     ALL ENTRIES INTO TO() HAVE BEEN MADE.
C.......................................
C     CHECK FOR DEBUG OUTPUT.
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) IDT,LWORK
  900 FORMAT(1H0,33HINSQPLOT DEBUG--COMPUTATIONAL DT=,I3,5X,6HLWORK=,I6,
     1/6X,20HCONTENTS OF TO ARRAY)
      WRITE(IODBUG,901)(TO(I),I=1,IUSET)
  901 FORMAT(1H ,20I6)
C     CONTENTS OF THE TO ARRAY
C     POSITION               CONTENTS
C     1.      OPERATION NUMBER=12
C     2.      LOCATION OF NEXT OPERATION IN T ARRAY
C     3.      LOCATION OF PARAMETERS IN P ARRAY-LPO
C     4.      LOCATION OF RAIN+MELT DATA IN D ARRAY
C     5.      LOCATION OF RUNOFF DATA IN D ARRAY
C     6.      LOCATION OF RATING CURVE ID IN THE P ARRAY.
C     7.      LOCATION OF START OF WORKING SPACE
C     8 TO    LOCATION OF EACH TIME SERIES TO BE
C     NPLOT+7    PLOTTED IN THE D ARRAY.
C.......................................
  199 CONTINUE
      RETURN
      END
