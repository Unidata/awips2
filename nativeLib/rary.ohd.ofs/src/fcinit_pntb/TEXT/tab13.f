C MEMBER TAB13
C  (from old member FCTAB13)
C
      SUBROUTINE TAB13(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,NWORK,NDD,
C                             LAST UPDATE: 01/03/95.10:04:44 BY $WC21DT
C
     1   LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE TATUM
C        ROUTING 'TATUM   ' OPERATION.
C.......................................
C      SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   DEC. 1979
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab13.f,v $
     . $',                                                             '
     .$Id: tab13.f,v 1.1 1995/09/17 18:49:04 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB1,4H3   /
C.......................................
C     TRACE LEVEL=1--DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,13,IBUG)
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
C     CHECK TO SEE IF 9 SPACES ARE AVAILABLE IN T().
  101 CALL CHECKT(9,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=ISKIPT
      LWORK=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  100 TO(ISKIPT+1)=13
      TO(ISKIPT+2)=NXT+ISKIPT+9
      TO(ISKIPT+3)=LPO
C
C     CHECK IF CARRYOVER NEEDED.
      NL=PO(16)
      DO 103 I=1,NL
      NCL=PO(16+I)
      IF (NCL.EQ.1) GO TO 103
      GO TO 104
  103 CONTINUE
      TO(ISKIPT+4)=0
      GO TO 106
C
C     CARRYOVER IS NEEDED.
  104 TO(ISKIPT+4)=LCO
C
C     STORE LOCATION OF OUTFLOW TIME SERIES.
  106 IF(ITB.EQ.0) LD=0
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
C     LOCATION AND LENGTH OF WORKING SPACE FOR TEMPORARY RESIDUALS.
      TO(ISKIPT+9)=NWORK+LWORK
      NL=PO(16)
      NCO=0
      DO 102 N=1,NL
      NC=PO(16+N)
      NCO=NCO+(NC-1)
  102 CONTINUE
      LWORK=LWORK+NCO
C
C     WORKING SPACE NEEDED FOR CHANGING TIME INTERVALS.
      IF((ITB.GT.0).AND.(ITA.NE.ITB)) GO TO 105
      TO(ISKIPT+8)=0
      GO TO 110
  105 TO(ISKIPT+8)=NWORK+LWORK
      LWORK=LWORK+(24/ITB)*NDD
  110 IUSET=ISKIPT+9
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     CHECK FOR DEBUG OUTPUT.
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(ISKIPT+I),I=1,9),IDT,LWORK
  900 FORMAT(1H0,28HTATUM DEBUG--CONTENTS OF TO=,9I6,/16X,17HCOMPUTATION
     1AL DT=,I3,5X,6HLWORK=,I6)
C.......................................
C     THE TO ARRAY ENTRIES ARE AS FOLLOWS.
C     POSITION               CONTENTS
C        1.     I.D. NUMBER OF THE OPERATION=13
C        2.     LOCATION OF NEXT OPERATION IN T ARRAY.
C        3.     LOCATION OF PARAMETERS IN P ARRAY=LPO
C        4.     LOCATION OF CARRYOVER IN C ARRAY=LCO
C                    =0 IF NO CARRYOVER
C        5.     LOCATION OF INFLOW IN D ARRAY.
C        6.     LOCATION OF OUTFLOW IN D ARRAY.
C                   =0 IF ROUTING AT A POINT.
C        7.     LOCATION OF WORKING SPACE FOR ROUTED INFLOW.
C        8.     LOCATION OF WORKING SPACE FOR FCHGDT
C                   =0 UNLESS ITB.GT.0 AND ITA.NE.ITB
C        9.     LOCATION OF WORKING SPACE FOR RESIDUALS
C.......................................
  199 CONTINUE
      RETURN
      END
