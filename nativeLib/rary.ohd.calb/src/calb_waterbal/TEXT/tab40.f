C MEMBER TAB40
C  (from old member MCTAB40)
C
      SUBROUTINE TAB40(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,P,MP,NWORK,
     1  LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'WATERBAL '
C     OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY . . .
C            ERIC ANDERSON - HRL     MAY 1991
C.......................................
      DIMENSION TS(MTS),PO(1),P(MP)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_waterbal/RCS/tab40.f,v $
     . $',                                                             '
     .$Id: tab40.f,v 1.2 1996/07/11 19:46:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB4,4H0   /
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1,DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,40,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES.
      NAREA=PO(14)
      LENGTH=7+(4*NAREA)
C.......................................
C     CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 105
      IUSET=0
      IDT=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  105 TO(1)=40
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
      IDT=24
C
C     OBSERVED DISCHARGE TIME SERIES
      CALL CKINPT(PO(7),PO(9),IDT,LD,TS,MTS,IERR)
      TO(5)=LD
C
C     SIMULATED DISCHARGE TIME SERIES
      CALL CKINPT(PO(10),PO(12),IDT,LD,TS,MTS,IERR)
      TO(6)=LD
C
C     LOCATION AND LENGTH OF WORKING SPACE
      TO(7)=NWORK
      LWORK=360
C
C     SNOW AND RAINFALL/RUNOFF OPERATIONS
      DO 110 N=1,NAREA
      LOC=22+(N-1)*17
      CALL FOPCDE(PO(LOC+7),NUM)
      IF (NUM.EQ.0) GO TO 111
      LP=0
      CALL FSERCH(NUM,PO(LOC+9),LP,P,MP)
      IF (LP.EQ.0) GO TO 111
      TO(7+N)=LP
      LC=P(LP-1)
      TO(7+NAREA+N)=LC
      GO TO 115
  111 TO(7+N)=0
      TO(7+NAREA+N)=0
  115 CALL FOPCDE(PO(LOC+12),NUM )
      IF (NUM.EQ.0) GO TO 116
      LP=0
      CALL FSERCH(NUM,PO(LOC+14),LP,P,MP)
      IF (LP.EQ.0) GO TO 116
      TO(7+2*NAREA+N)=LP
      LC=P(LP-1)
      TO(7+3*NAREA+N)=LC
      GO TO 110
  116 TO(7+2*NAREA+N)=0
      TO(7+3*NAREA+N)=0
  110 CONTINUE
      IUSET=LENGTH
C     ALL ENTRIES INTO TO() HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,903)
  903 FORMAT (1H0,34HTAB40 DEBUG--CONTENTS OF TO ARRAY.)
      WRITE (IODBUG,904) (TO(I),I=1,IUSET)
  904 FORMAT (1H ,20I6)
C.......................................
C     THE CONTENTS OF THE TO ARRAY ARE AS FOLLOWS.
C     POSITION                     CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=40
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF PARAMETERS IN THE P ARRAY
C        4.        LOCATION OF CARRYOVER IN THE C ARRAY.
C        5.        LOCATION OF OBSERVED DISCHARGE IN D ARRAY
C        6.        LOCATION OF SIMULATED DISCHARGE IN D ARRAY
C        7.        LOCATION OF WORKING SPACE
C        8.        LOCATIONS OF THE FOLLOWING POINTERS:
C                   A. SNOW MODEL PARAMETERS FOR EACH SUBAREA
C                   B. SNOW MODEL CARRYOVER FOR EACH SUBAREA
C                   C. RAINFALL/RO PARAMETERS R EACH SUBAREA
C                   D. RAINFALL/RO CARRYOVER FOR EACH SUBAREA
C                      =0 IF THE MODEL DOESN'T EXIST
C.......................................
  199 CONTINUE
      RETURN
      END
