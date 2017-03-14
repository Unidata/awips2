C MEMBER TAB32
C  (from old member FCTAB32)
C
C                             LAST UPDATE: 02/04/94.12:21:23 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE TAB32(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,P,MP,NWORK,LWORK)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'FFG      '
C     OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY . . .
C            ERIC ANDERSON - HRL     APRIL 1992
C.......................................
      DIMENSION TS(MTS),PO(*),P(MP)
      INTEGER TO(*)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab32.f,v $
     . $',                                                             '
     .$Id: tab32.f,v 1.1 1995/09/17 18:49:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB3,4H2   /
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1,DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,32,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES.
      LENGTH=11
C.......................................
C     CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 105
      IUSET=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  105 TO(1)=32
      TO(2)=NXT+LENGTH
      TO(3)=LPO
C
C     SNOW AND RAINFALL/RUNOFF OPERATIONS
      CALL FOPCDE(PO(13),NUM)
      IF (NUM.EQ.0) GO TO 110
      LP=0
      CALL FSERCH(NUM,PO(15),LP,P,MP)
      IF (LP.EQ.0) GO TO 110
      TO(4)=LP
      LC=P(LP-1)
      TO(5)=LC
      GO TO 115
  110 TO(4)=0
      TO(5)=0
  115 CALL FOPCDE(PO(9),NUM)
      IF (NUM.EQ.0) GO TO 120
      LP=0
      CALL FSERCH(NUM,PO(11),LP,P,MP)
      IF (LP.EQ.0) GO TO 120
      TO(6)=LP
      LC=P(LP-1)
      TO(7)=LC
      GO TO 121
  120 TO(6)=0
      TO(7)=0
C
C     SNOW MODEL TEMPERATURE TIME SERIES
  121 IF (TO(4).EQ.0) GO TO 125
      LP=TO(4)
      IDT=P(LP+13)
      CALL CKINPT(P(LP+10),P(LP+12),IDT,LD,TS,MTS,IERR)
      TO(8)=LD
      GO TO 130
  125 TO(8)=0
C
C     RAINFALL-RUNOFF MODEL TEMPERATURE TIME SERIES
 130  NUM=PO(21)
      IF (NUM.NE.1) GO TO 135
C     SAC-SMA
      LP=TO(6)
      LFRZE=P(LP+23)
      IF (LFRZE.EQ.0) GO TO 145
      IDT=P(LP+LFRZE+2)
      CALL CKINPT(P(LP+LFRZE-1),P(LP+LFRZE+1),IDT,LD,TS,MTS,IERR)
      TO(9)=LD
      GO TO 150
 135  IF (NUM.NE.24) GO TO 140
C     API-CONT
      LP=TO(6)
      LTA=P(LP+17)
      IF (LTA.EQ.0) GO TO 145
      IDT=P(LP+LTA+2)
      CALL CKINPT(P(LP+LTA-1),P(LP+LTA+1),IDT,LD,TS,MTS,IERR)
      TO(9)=LD
      GO TO 150
 140  IF (NUM.NE.33) GO TO 145
C     API-CIN
      LP=TO(6)
      IDT=P(LP+12)
      CALL CKINPT(P(LP+22),P(LP+24),IDT,LD,TS,MTS,IERR)
      TO(9)=LD
      GO TO 150
C     NO TEMPERATURE FOR RAINFALL-RUNOFF MODEL
 145  TO(9)=0
C
C      RAIN-SNOW ELEVATION TIME SERIES
  150 IF(TO(4).EQ.0) GO TO 155
      LP=TO(4)
      LRS=P(LP+29)
      IF(LRS.EQ.0) GO TO 155
      IDT=P(LP+13)
      CALL CKINPT(P(LP+LRS+1),P(LP+LRS+3),IDT,LD,TS,MTS,IERR)
      TO(10)=LD
      GO TO 160
  155 TO(10)=0
C
C     LOCATION AND LENGTH OF WORKING SPACE
  160 TO(11)=NWORK
      LWORK=PO(23)
      IUSET=LENGTH
C     ALL ENTRIES INTO TO() HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,903)
  903 FORMAT (1H0,34HTAB32 DEBUG--CONTENTS OF TO ARRAY.)
      WRITE (IODBUG,904) (TO(I),I=1,IUSET)
  904 FORMAT (1H ,20I6)
C.......................................
C     THE CONTENTS OF THE TO ARRAY ARE AS FOLLOWS.
C     POSITION                     CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=32
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF PARAMETERS IN THE P ARRAY
C        4.        LOCATION OF SNOW MODEL PARAMETERS IN P ARRAY
C                       =0  IF NO SNOW MODEL
C        5.        LOCATION OF SNOW MODEL CARRYOVER IN C ARRAY
C                       =0  IF NO SNOW MODEL
C        6.        LOCATION OF RAINFALL-RUNOFF PARAMETERS IN P ARRAY
C        7.        LOCATION OF RAINFALL-RUNOFF CARRYOVER IN C ARRAY
C        8.        LOCATION OF SNOW MODEL TEMPERATURE IN D ARRAY
C                       =0  IF NO SNOW MODEL
C        9.        LOCATION OF RAINFALL-RUNOFF TEMPERATURE IN D ARRAY
C                       =0  IF NONE USED
C       10.        LOCATION OF RAIN-SNOW ELEVATION IN D ARRAY
C                       =0  IF NONE USED
C       11.        LOCATION OF WORKING SPACE
C.......................................
  199 CONTINUE
      RETURN
      END
