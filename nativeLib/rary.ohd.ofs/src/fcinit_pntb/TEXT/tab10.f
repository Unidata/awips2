C MEMBER TAB10
C  (from old member FCTAB10)
C
      SUBROUTINE TAB10(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,NWORK,NDD,
C                             LAST UPDATE: 01/03/95.10:03:31 BY $WC21DT
C
     1   LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C        ADD/SUBTRACT OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab10.f,v $
     . $',                                                             '
     .$Id: tab10.f,v 1.1 1995/09/17 18:49:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB1,4H0   /
C.......................................
C     TRACE LEVEL =1 - DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,10,IBUG)
C.......................................
C     INITIAL VALUES
      ISKIPT=0
C.......................................
C     CHECK TO SEE IF TIME SERIES B NEEDS TO BE CLEARED.
      ITB=PO(6)
      CALL FINDTS(PO(3),PO(5),ITB,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF(ICK.GT.0) GO TO 101
C
C     TIME SERIES B NEEDS TO BE CLEARED.
      CALL CLEAR(PO(3),PO(5),ITB,LD,LTS,TO,LEFT,ISKIPT,NXT,TS,MTS)
C.......................................
C     CHECK TO SEE IF 7 SPACES ARE AVAILABLE IN T().
  101 CALL CHECKT(7,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=ISKIPT
      LWORK=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE - MAKE ENTRIES INTO TO().
  100 TO(ISKIPT+1)=10
      TO(ISKIPT+2)=NXT+ISKIPT+7
      TO(ISKIPT+3)=LPO
      ICO=PO(11)
      IF(ICO.EQ.1) GO TO 102
      TO(ISKIPT+4)=0
      GO TO 103
  102 TO(ISKIPT+4)=LCO
C
C     STORE LOCATION OF TIME SERIES B.
  103 TO(ISKIPT+5)=LD
C
C     STORE LOCATION OF TIME SERIES A.
      ITA=PO(10)
      CALL CKINPT(PO(7),PO(9),ITA,LD,TS,MTS,IERR)
      TO(ISKIPT+6)=LD
C
C     DETERMINE COMPUTATIONAL TIME INTERVAL FOR THE OPERATION.
      IDT=ITB
      CALL FCLCD(IDT,ITA)
C
C     DETERMINE IF WORKING SPACE NEEDED.
      IF(ITA.NE.ITB) GO TO 105
C
C     WORKING SPACE NOT NEEDED.
      LWORK=0
      TO(ISKIPT+7)=0
      GO TO 110
C
C     WORKING SPACE NEEDED.
  105 TO(ISKIPT+7)=NWORK
      LWORK=(24/ITB)*NDD
  110 IUSET=ISKIPT+7
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(ISKIPT+I),I=1,7),IDT,LWORK
  900 FORMAT(1H0,29HADD/SUB DEBUG-CONTENTS OF TO=,7I6,/16X,
     117HCOMPUTATIONAL DT=,I3,5X,6HLWORK=,I6)
C.......................................
C     THE TO ARRAY ENTRIES ARE AS FOLLOWS.
C     POSITION                   CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=10
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF THE PARAMETERS IN THE P ARRAY=LPO
C        4.        LOCATION OF THE CARRYOVER IN THE C ARRAY=LCO
C        5.        LOCATION OF TIME SERIES B IN THE D ARRAY
C        6.        LOCATION OF TIME SERIES A IN THE D ARRAY
C        7.        LOCATION OF WORKING SPACE IN THE D ARRAY
C.......................................
  199 CONTINUE
      RETURN
      END
