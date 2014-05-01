C MODULE TAB7
C-----------------------------------------------------------------------
C
      SUBROUTINE TAB7 (TO,LEFT,IUSET,NXT,LPO,PO,LCO,CO,TS,MTS,NWORK,NDD,
     1   LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE LAG AND
C        K ROUTING 'LAG/K   ' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION PO(1),CO(1),TS(MTS)
      INTEGER TO(1)
      CHARACTER*8 SNAME/'TAB7'/
      LOGICAL FOP7
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab7.f,v $
     . $',                                                             '
     .$Id: tab7.f,v 1.2 2000/03/13 20:49:50 page Exp $
     . $' /
C    ===================================================================
C
C
C.......................................
C     TRACE LEVEL = 1--DEBUG SWITCH = IBUG
      CALL FPRBUG(SNAME,1,7,IBUG)
C.......................................
C     INITIAL VALUES
      ISKIPT=0
C.......................................
C     CHECK TO SEE IF OUTFLOW TIME SERIES NEEDS TO BE CLEARED.
      ITB=PO(9)
      IF(ITB.EQ.0) GO TO 101
      CALL FINDTS(PO(6),PO(8),ITB,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF(ICK.GT.0) GO TO 101
C
C     CLEAR OUTFLOW TIME SERIES.
      CALL CLEAR(PO(6),PO(8),ITB,LD,LTS,TO,LEFT,ISKIPT,NXT,TS,MTS)
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
  100 TO(ISKIPT+1)=7
      TO(ISKIPT+2)=NXT+ISKIPT+8
      TO(ISKIPT+3)=LPO
      TO(ISKIPT+4)=LCO
C
C     STORE LOCATION OF OUTFLOW TIME SERIES.
      IF(ITB.EQ.0) LD=0
      TO(ISKIPT+6)=LD
C
C     STORE LOCATION OF INFLOW TIME SERIES.
      ITA=PO(5)
      CALL CKINPT(PO(2),PO(4),ITA,LD,TS,MTS,IERR)
      TO(ISKIPT+5)=LD
C
C     DETERMINE COMPUTATIONAL TIME INTERVAL FOR THE OPERATION.
      IDT=ITB
      CALL FCLCD(IDT,ITA)
C
C     LOCATION AND LENGTH OF WORKING SPACE FOR ROUTED INFLOW.
      TO(ISKIPT+7)=NWORK
      L1=(24/ITA)*NDD
C
C     WORKING SPACE NEEDED FOR CHANGING TIME INTERVAL.
      IF((ITB.GT.0).AND.(ITA.NE.ITB)) GO TO 105
      TO(ISKIPT+8)=0
      LDT=0
      GO TO 110
  105 TO(ISKIPT+8)=NWORK+L1
      LDT=(24/ITB)*NDD
C.......................................
C     COMPUTE TOTAL LENGTH OF WORKING SPACE NEEDED.
  110 LC=CO(1)
      IF (FOP7(PO(19),PO(20))) GO TO 115
C
C     ONLY K PART USED.
      L2=LC
      IF(LDT.GT.L2) L2=LDT
      LWORK=L1+L2
      GO TO 120
C
C     BOTH LAG AND K OR ONLY LAG USED.
  115 LCLAG=CO(5)
      L2=LC+2*(LCLAG+L1)+3
      LWORK=L1+L2
  120 IUSET=ISKIPT+8
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     CHECK FOR DEBUG OUTPUT.
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(ISKIPT+I),I=1,8),IDT,LWORK
  900 FORMAT (' TAB7 DEBUG: CONTENTS OF TO=',8I6,' IDT=',I3,
     1   ' LWORK=',I6)
C.......................................
C     THE TO ARRAY ENTRIES ARE AS FOLLOWS.
C     POSITION            CONTENTS
C     1.      I.D. NUMBER OF THE OPERATION = 7
C     2.      LOCATION OF NEXT OPERATION IN T ARRAY.
C     3.      LOCATION OF PARAMETERS IN P ARRAY=LPO
C     4.      LOCATION OF CARRYOVER IN C ARRAY=LCO
C     5.      LOCATION OF INFLOW IN D ARRAY
C     6.      LOCATION OF OUTFLOW IN D ARRAY
C                 0 = IF ROUTING AT A POINT.
C     7.      LOCATION OF WORKING SPACE FOR ROUTED INFLOW
C                 AND INTERMEDIATE CALCULATIONS.
C     8.      LOCATION IF WORKING SPACE FOR FCHGDT
C                 0 = UNLESS ITB.GT.0 AND ITA.NE.ITB
C.......................................
C
  199 RETURN
C
      END
