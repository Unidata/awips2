C MEMBER TAB20
C  (from old member FCTAB20)
C
      SUBROUTINE TAB20(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,NWORK,NDD,
     1    LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE CHANGE TIME
C           INTERVAL OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY ...
C         ERIC ANDERSON - HRL  APRIL 1981
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C      COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab20.f,v $
     . $',                                                             '
     .$Id: tab20.f,v 1.1 1995/09/17 18:49:09 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB2,4H0   /
C.......................................
C     TRACE LEVEL=1 - DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,20,IBUG)
C.......................................
C     INITIAL VALUES.
      LT=8
      ICO=PO(10)
C.......................................
C     CHECK THAT LT SPACES ARE AVAILABLE IN T( ).
      CALL CHECKT(LT,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      LWORK=0
      IDT=0
      RETURN
C.......................................
C     SPACE AVAILABLE - MAKE ENTRIES INTO TO( ).
  100 TO(1)=20
      TO(2)=NXT+LT
      TO(3)=LPO
      IF(ICO.GT.0) GO TO 105
      TO(4)=0
      GO TO 110
  105 TO(4)=LCO
C.......................................
C     STORE LOCATION OF INPUT TIME SERIES.
  110 IDT=PO(5)
      CALL CKINPT(PO(2),PO(4),IDT,LD,TS,MTS,IERR)
      TO(5)=LD
C.......................................
C     STORE LOCATION OF OUTPUT TIME SERIES.
      I=PO(9)
      CALL FINDTS(PO(6),PO(8),I,LD,LTS,DIM)
      IF(LTS.GT.0) TS(LTS+8)=1.01
      TO(6)=LD
C.......................................
C     WORKING SPACE IF NEEDED.
      IMDQ=PO(12)
      IF (IMDQ.EQ.7) GO TO 115
      TO(7)=0
      TO(8)=0
      LWORK=0
      GO TO 120
  115 TO(7)=NWORK
      LWORK=(24/I)*NDD
      TO(8)=NWORK+LWORK
      LWORK=LWORK+(24/IDT)*NDD
  120 IUSET=LT
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(I),I=1,LT),IDT
  900 FORMAT(1H0,30HCHANGE-T DEBUG-CONTENTS OF TO=,8I6,/16X,
     117HCOMPUTATIONAL DT=,I3)
C.......................................
C      THE TO ARRAY ENTRIES ARE AS FOLLOWS.
C     POSITION                        CONTENTS
C        1.         I.D. NUMBER FOR THE OPERATION=20
C        2.         LOCATION OF NEXT OPERATION IN THE T ARRAY.
C        3.         LOCATION OF PARAMETERS IN THE P ARRAY.
C        4.         LOCATION OF CARRYOVER IN THE C ARRAY, =0 IF NONE.
C        5.         LOCATION OF INPUT T.S. IN THE D ARRAY.
C        6.         LOCATION OF OUTPUT T.S. IN THE D ARRAY.
C        7.         LOCATION OF OUTPUT WORKING SPACE, =0 IF NONE.
C        8.         LOCATION OF INPUT  WORKING SPACE, =0 IF NONE.
C.......................................
  199 CONTINUE
      RETURN
      END
