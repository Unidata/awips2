C MEMBER TAB9
C  (from old member FCTAB9)
C
      SUBROUTINE TAB9(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,NWORK,NDD,
C                             LAST UPDATE: 01/03/95.10:05:52 BY $WC21DT
C
     1   LWORK,IDT)
C.......................................
C     THIS IS THE OPERATION TABLE ENTRY SUBROUTINE FOR THE
C        MUSKINGUM ROUTING 'MUSKROUT' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab9.f,v $
     . $',                                                             '
     .$Id: tab9.f,v 1.1 1995/09/17 18:49:30 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB9,4H    /
C.......................................
C     TRACE LEVEL=1--DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,9,IBUG)
C.......................................
C     INITIAL VALUES
      ISKIPT=0
C.......................................
C     CHECK TO SEE IF OUTFLOW TIME SERIES NEEDS TO BE CLEARED.
      ITB=PO(14)
      IF(ITB.EQ.0) GO TO 101
      CALL FINDTS(PO(11),PO(13),ITB,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF(ICK.GT.0) GO TO 101
C
C     CLEAR OUTFLOW TIME SERIES.
      CALL CLEAR(PO(11),PO(13),ITB,LD,LTS,TO,LEFT,ISKIPT,NXT,TS,MTS)
C.......................................
C     CHECK TO SEE IF 8 SPACES ARE AVAILABLE IN T().
  101 CALL CHECKT(8,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=ISKIPT
      LWORK=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  100 TO(ISKIPT+1)=9
      TO(ISKIPT+2)=NXT+ISKIPT+8
      TO(ISKIPT+3)=LPO
      TO(ISKIPT+4)=LCO
C
C     STORE LOCATION OF OUTFLOW TIME SERIES.
      IF(ITB.EQ.0) LD=0
      TO(ISKIPT+6)=LD
C
C     STORE LOCATION OF INFLOW TIME SERIES.
      ITA=PO(10)
      CALL CKINPT(PO(7),PO(9),ITA,LD,TS,MTS,IERR)
      TO(ISKIPT+5)=LD
C
C     DETERMINE COMPUTATIONAL TIME INTERVAL FOR THE OPERATION.
      IDT=ITB
      CALL FCLCD(IDT,ITA)
C
C     LOCATION AND LENGTH OF WORKING SPACE FOR ROUTED INFLOW.
      TO(ISKIPT+7)=NWORK
      LWORK=(24/ITA)*NDD
C
C     WORKING SPACE NEEDED FOR CHANGING TIME INTERVAL.
      IF((ITB.GT.0).AND.(ITA.NE.ITB)) GO TO 105
      TO(ISKIPT+8)=0
      GO TO 110
  105 TO(ISKIPT+8)=NWORK+LWORK
      LWORK=LWORK+(24/ITB)*NDD
  110 IUSET=ISKIPT+8
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     CHECK FOR DEBUG OUTPUT.
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(ISKIPT+I),I=1,8),IDT,LWORK
  900 FORMAT(1H0,31HMUSKROUT DEBUG--CONTENTS OF TO=,8I6,/16X,17HCOMPUTAT
     1IONAL DT=,I3,5X,6HLWORK=,I6)
C.......................................
C     THE TO ARRAY ENTRIES ARE AS FOLLOWS.
C     POSITION            CONTENTS
C     1.      I.D. NUMBER OF THE OPERATION=9
C     2.      LOCATION OF NEXT OPERATION IN T ARRAY
C     3.      LOCATION OF PARAMETERS IN P ARRAY=LPO
C     4.      LOCATION OF CARRYOVER IN C ARRAY=LCO
C     5.      LOCATION OF INFLOW IN D ARRAY
C     6.      LOCATION OF OUTFLOW IN D ARRAY
C                 =0 IF ROUTING AT A POINT.
C     7.      LOCATION OF WORKING SPACE FOR ROUTED INFLOW
C     8.      LOCATION OF WORKING SPACE FOR FCHGDT
C                 =0 UNLESS ITB.GT.0 AND ITA.NE.ITB
C.......................................
  199 CONTINUE
      RETURN
      END
