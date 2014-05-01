C MEMBER TAB23
C  (from old member FCTAB23)
C
      SUBROUTINE TAB23(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,NWORK,NDD,
     1LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C     STAGE-DISCHARGE CONVERSION OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY ...
C          ERIC ANDERSON - HRL  APRIL 1981
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab23.f,v $
     . $',                                                             '
     .$Id: tab23.f,v 1.1 1995/09/17 18:49:10 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB2,4H3   /
C.......................................
C     TRACE LEVEL=1 - DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,23,IBUG)
C.......................................
C     INITIAL VALUES
      LT=9
      ICNVRT=PO(7)
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
  100 TO(1)=23
      TO(2)=NXT+LT
      TO(3)=LPO
      TO(4)=LCO
      IDT=PO(11)
C.......................................
C     STORE LOCATION OF STAGE DATA.
      IF(ICNVRT.EQ.2) GO TO 105
      CALL CKINPT(PO(8),PO(10),IDT,LD,TS,MTS,IERR)
      GO TO 110
  105 CALL FINDTS(PO(8),PO(10),IDT,LD,LTS,DIM)
      IF(LTS.GT.0) TS(LTS+8)=1.01
  110 TO(5)=LD
C.......................................
C     STORE LOCATION OF DISCHARGE DATA.
      IF(ICNVRT.EQ.1) GO TO 115
      CALL CKINPT(PO(12),PO(14),IDT,LD,TS,MTS,IERR)
      GO TO 120
  115 CALL FINDTS(PO(12),PO(14),IDT,LD,LTS,DIM)
      IF(LTS.GT.0) TS(LTS+8)=1.01
  120 TO(6)=LD
C.......................................
C     STORE RATING CURVE AND WORKING SPACE INFORMATION.
      TO(7)=LPO+15
      TO(8)=NWORK
      LWORK=(24/IDT)*NDD
      TO(9)=NWORK+LWORK
      LWORK=LWORK+LWORK
      IUSET=LT
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(I),I=1,LT),IDT,LWORK
  900 FORMAT(1H0,29HSTAGE-Q DEBUG-CONTENTS OF TO=,9I6,/16X,
     117HCOMPUTATIONAL DT=,I3,5X,6HLWORK=,I6)
C.......................................
C     THE TO ARRAY ENTRIES ARE AS FOLLOWS.
C      POSITION                       CONTENTS
C        1.         I.D.NUMBER FOR THE OPERATION=23
C        2.         LOCATION OF NEXT OPERATION IN THE T ARRAY.
C        3.         LOCATION OF PARAMETERS IN THE P ARRARY.
C        49         LOCATION OF CARRYOVER IN THE C ARRAY.
C        5.         LOCATION OF STAGE DATA IN THE D ARRAY.
C        6.         LOCATION OF DISCHARGE DATA IN THE D ARRAY.
C        7.         LOCATION OF RATING CURVE I.D. IN THE P ARRAY.
C        8.         LOCATION OF WORKING SPACE FOR POINTERS.
C        9.         LOCATION OF WORKING SPACE FOR TIMES.
C.......................................
  199 CONTINUE
      RETURN
      END
