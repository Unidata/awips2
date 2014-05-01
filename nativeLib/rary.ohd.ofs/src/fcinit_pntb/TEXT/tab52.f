C MEMBER TAB52
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE TAB52 (TO, LEFT, IUSET, NXT, LPO, PO, LCO, TS, MTS,
     1                  LWORK, IDT)
C-----------------------------------------------------------------------
C   THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE
C   SSARR (STREAMFLOW SIMULATION AND RESERVOIR REGULATION)
C   TIME SERIES SUMMING POINT 'SUMPOINT' OPERATION.
C
C   ROUTINE INITIALLY WRITTEN BY RUSS ERB - HRL  FEB 1996
C-----------------------------------------------------------------------
C   PASSED ARGUMENTS
C    NAME  I/O
C   ------ ---
C   TO      O  INT  ARRAY USED TO STORE OPERATIONS TABLE ENTRIES
C   LEFT    I  INT  INDICATING HOW MUCH SPACE IS LEFT IN T()
C   IUSET   O  INT  HOW MUCH SPACE IN THE T() IS USED BY THIS OPER
C   NXT     I  INT  STARTING LOCATION OF THE TO() FOR THIS OPERATION
C   LPO     I  INT  STARTING LOCATION OF THE PO() FOR THIS OPERATION
C   PO      I  REAL ARRAY TO STORE ALL PARAMETERS FOR THIS OPERATION
C   LCO     I  INT  STARTING LOCATION OF THE CO() FOR THIS OPERATION
C   TS      I  REAL ARRAY OF INFORMATION ON ALL TIME SERIES IN SEGMENT
C   MTS     I  INT  MAXIMUM DIMENSION OF THE TS()
C   LWORK   O  INT  HOW MUCH WORKING SPACE IS NEEDED FOR THIS OPERATION
C   IDT     O  INT  DELTA T, MINIMUM TIME INTERVAL THIS OPER CAN BE RUN
C
      INTEGER TO(*), LEFT, IUSET, NXT, LPO, LCO, MTS, LWORK, IDT
      REAL    PO(*), TS(MTS)
C
C   LOCAL VARIABLES
C    NAME
C   ------
C   IBUG    DEBUG SWITCH
C   INEED   NUMBER OF SPACES NEEDED IN THE T()
C   IDELT   TIME INTERVAL FOR ALL TIME SERIES
C   LD      LOCATION OF THE TIME SERIES DATA IN THE D()
C   LTS     LOCATION OF INFORMATION ABOUT THE TIME SERIES IN THE TS()
C   CKNONE  STRING TO CHECK IF BEGIN INTERVAL OUTPUT TS TYPE IS "NONE"
C   NUMITS  NUMBER OF INPUT TIME SERIES
C
      INTEGER IBUG, INEED, IERR, IDELT, LD, LTS, NUMITS
      REAL    CKNONE, DIM
C.......................................................................
C   COMMON BLOCKS FOR DEBUG UNIT NUMBERS
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab52.f,v $
     . $',                                                             '
     .$Id: tab52.f,v 1.1 1996/03/21 14:40:14 page Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C   DATA STATEMENT FOR "NONE" STRING
      DATA CKNONE/4HNONE/
C.......................................................................
C   DEBUG INITIALIZATION WITH TRACE LEVEL = 1
      CALL FPRBUG ('TAB52   ', 1, 52, IBUG)
C.......................................................................
C   SET VARIABLE TO HOLD NUMBER OF INPUT TIME SERIES
      NUMITS = PO(20)
C
C   NUMBER OF SPACES I NEED IN T().
      INEED = 6 + NUMITS
C
C   CHECK TO SEE IF SPACE IS AVAILABLE IN T().
      CALL CHECKT (INEED, LEFT, IERR)
      IF(IERR .NE. 0) THEN
         IUSET = 0
         LWORK = 0
         IDT = 0
         RETURN
      END IF
C.......................................................................
C   SPACE IS AVAILABLE SO MAKE ENTRIES INTO TO().
      TO(1) = 52
      TO(2) = NXT + INEED
      TO(3) = LPO
      TO(4) = LCO
C
C   TIME INTERVAL OF ALL INPUT AND OUPUT TIME SERIES.
      IDELT = PO(24)
C
C   CHECK IF BEGIN INTERVAL OUTPUT TYPE IS "NONE".  IF SO THEN SET
C   LOCATION OF BEGIN INTERVAL OUTPUT TIME SERIES TO ZERO ELSE CALL
C   FINDTS AND GET THE LOCATION OF TIME SERIES IN THE D() AND SET THE
C   FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      IF(PO(23) .EQ. CKNONE) THEN
         TO(5) = 0
      ELSE
         CALL FINDTS (PO(21), PO(23), IDELT, LD, LTS, DIM)
         TO(5) = LD
         IF(LTS .GT. 0) TS(LTS+8) = 1.01
      ENDIF
C
C   GET THE LOCATION OF END INTERVAL OUTPUT TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(25), PO(27), IDELT, LD, LTS, DIM)
      TO(6) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   CALL CKINPT AND GET THE LOCATIONS OF INPUT TIME SERIES IN THE D().
C   NEED TO LOOP THROUGH NUMBER OF INPUT TIME SERIES
      DO 10 I=1, NUMITS
         CALL CKINPT (PO(24 + 5*I), PO(26 + 5*I), IDELT,LD,TS,MTS,IERR)
         TO(6+I) = LD
   10 CONTINUE
C
C.......................................................................
C   SET HOW MUCH SPACE IN THE T() IS USED BY THIS OPERATION.
      IUSET = INEED
C.......................................................................
C   NO WORKING SPACE IS USED IN THIS OPERATION.
      LWORK = 0
C.......................................................................
C   SET COMPUTATIONAL TIME INTERVAL FOR THE OPERATION.
      IDT = IDELT
C.......................................................................
C  CHECK IF DEBUG OUTPUT IS TO BE PRINTED.
      IF(IBUG .GT. 0) THEN
         WRITE(IODBUG,1000) (TO(I),I=1,INEED)
 1000    FORMAT(1H0,'SARROUTE DEBUG - CONTENTS OF TO()=',8I6)
      END IF
C.......................................................................
C   THE TO ARRAY ENTRIES ARE AS FOLLOWS:
C   POS
C   ---
C    1      I.D. NUMBER OF THE OPERATION (52)
C    2      LOCATION OF NEXT OPERATION IN T ARRAY (NXT + INEED)
C    3      LOCATION OF PARAMETERS IN P ARRAY (LPO)
C    4      LOCATION OF CARRYOVER IN C ARRAY (LCO)
C    5      LOCATION OF BEGIN TIME INTERVAL OUTPUT TS IN D ARRAY
C              SET TO 0 IF BEGIN INTERVAL OUTPUT DATA TYPE IS "NONE"
C    6      LOCATION OF END TIME INTERVAL OUTPUT TS IN D ARRAY
C    6 + NUMITS      LOCATION(S) OF INPUT TS IN D ARRAY
C.......................................................................
      RETURN
      END
