C MEMBER TAB45
C  (from old member FCTAB45)
C
C @PROCESS LVL(77)
C
      SUBROUTINE TAB45(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,
C                             LAST UPDATE: 02/14/94.13:33:53 BY $WC30KH
C
     1   LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C        DELTA-TS OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        KUANG HSU  - HRL   JAN. 1993
C.......................................
      DIMENSION PO(*),TS(MTS)
      INTEGER TO(*)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab45.f,v $
     . $',                                                             '
     .$Id: tab45.f,v 1.1 1995/09/17 18:49:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB4,4H5   /
C.......................................
C     TRACE LEVEL =1 - DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,45,IBUG)
C.......................................
C     INITIAL VALUES
      LWORK=0
      IDT=PO(5)
      LENGTH=6
C.......................................
C     CHECK TO SEE IF SPACE AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE - MAKE ENTRIES INTO TO().
  100 TO(1)=45
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
C
C     STORE LOCATION OF INPUT TIME SERIES.
      CALL CKINPT(PO(2),PO(4),IDT,LD,TS,MTS,IERR)
      TO(5)=LD
C
C     STORE LOCATION OF OUTPUT TIME SERIES.
      CALL FINDTS(PO(6),PO(8),IDT,LD,LTS,DIM)
      TO(6)=LD
C
C     PUT FLAG=1 TO INDICATE THE TIME SERIES CONTAINS DATA VALUES
      IF(LTS.GT.0) TS(LTS+8)=1.01
      IUSET=LENGTH
C
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) IDT,(TO(I),I=1,IUSET)
  900 FORMAT(1H0,'DELTA-TS DEBUG--TAB ROUTINE--COMPUTATIONAL DT=',I2,
     1 /16X,'CONTENTS OF TO= ',6I6)
C.......................................
C     THE TO ARRAY ENTRIES ARE AS FOLLOWS.
C     POSITION                   CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=45
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF THE PARAMETERS IN THE P ARRAY=LPO
C        4.        LOCATION OF THE CARRYOVER IN THE C ARRAY=LCO
C        5.        LOCATION OF INPUT TIME SERIES IN THE D ARRAY
C        6.        LOCATION OF OUTPUT TIME SERIES IN THE D ARRAY
C.......................................
  199 CONTINUE
      RETURN
      END
