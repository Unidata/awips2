C MEMBER TAB59
C-----------------------------------------------------------------------
C
      SUBROUTINE TAB59 (TO, LEFT, IUSET, NXT, LPO, PO, LCO, TS, MTS,
     1                  LWORK, IDT)
C-----------------------------------------------------------------------
C   THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE
C   TIDE BALANCE REVIEW 'TIDEREV' OPERATION.
C
C   ROUTINE INITIALLY WRITTEN BY RUSS ERB - HRL  FEB 1998
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
C
      INTEGER IBUG, INEED, IERR, IDELT, LD, LTS
      REAL    DIM
C.......................................................................
C   COMMON BLOCKS FOR DEBUG UNIT NUMBERS
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab59.f,v $
     . $',                                                             '
     .$Id: tab59.f,v 1.1 1998/04/07 11:58:37 page Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C   DEBUG INITIALIZATION WITH TRACE LEVEL = 1
      CALL FPRBUG ('TAB59   ', 1, 59, IBUG)
C.......................................................................
C   NUMBER OF SPACES I NEED IN T().
      INEED = 10
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
      TO(1) = 59
      TO(2) = NXT + INEED
      TO(3) = LPO
      TO(4) = LCO
C
C   TIME INTERVAL OF ALL INPUT TIME SERIES IS ALWAYS 1 AS PER NWRFC
      IDELT = 1
C
C   CALL CKINPT AND GET THE LOCATION OF THE DWOPER OBSERVED STAGE
C   TIME SERIES IN THE D().
      CALL CKINPT (PO(20), PO(22), IDELT, LD, TS, MTS, IERR)
      TO(5) = LD
C
C   CALL CKINPT AND GET THE LOCATION OF THE NOS STAGE
C   TIME SERIES IN THE D().
      CALL CKINPT (PO(23), PO(25), IDELT, LD, TS, MTS, IERR)
      TO(6) = LD
C
C   TIME INTERVAL OF ALL OUTPUT TIME SERIES IS ALWAYS 24 AS PER NWRFC
      IDELT = 24
C
C   GET THE LOCATION OF THE TIDE1 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(26), PO(28), IDELT, LD, LTS, DIM)
      TO(7) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   GET THE LOCATION OF THE TIDE2 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(29), PO(31), IDELT, LD, LTS, DIM)
      TO(8) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   GET THE LOCATION OF THE TIDE3 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(32), PO(34), IDELT, LD, LTS, DIM)
      TO(9) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   GET THE LOCATION OF THE TIDE4 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(35), PO(37), IDELT, LD, LTS, DIM)
      TO(10) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C.......................................................................
C   SET HOW MUCH SPACE IN THE T() IS USED BY THIS OPERATION.
      IUSET = INEED
C.......................................................................
C   NO WORKING SPACE IS USED IN THIS OPERATION.
      LWORK = 0
C.......................................................................
C   SET COMPUTATIONAL TIME INTERVAL FOR THE OPERATION TO 1 HOUR.
      IDT = 1
C.......................................................................
C  CHECK IF DEBUG OUTPUT IS TO BE PRINTED.
      IF(IBUG .GT. 0) THEN
         WRITE(IODBUG,1000) (TO(I),I=1,INEED)
 1000    FORMAT(1H0,'TIDEREV DEBUG - CONTENTS OF TO()=',10I6)
      END IF
C.......................................................................
C   THE TO ARRAY ENTRIES ARE AS FOLLOWS:
C   POS
C   ---
C    1      I.D. NUMBER OF THE OPERATION (59)
C    2      LOCATION OF NEXT OPERATION IN T ARRAY (NXT + INEED)
C    3      LOCATION OF PARAMETERS     IN P ARRAY (LPO)
C    4      LOCATION OF CARRYOVER      IN C ARRAY (LCO)
C    5      LOCATION OF DWOPER OBSERVED STAGE TS IN D ARRAY
C    6      LOCATION OF NOS OBSERVED STAGE    TS IN D ARRAY
C    7      LOCATION OF TIDE1 BALANCE         TS IN D ARRAY
C    8      LOCATION OF TIDE2 BALANCE         TS IN D ARRAY
C    9      LOCATION OF TIDE3 BALANCE         TS IN D ARRAY
C   10      LOCATION OF TIDE4 BALANCE         TS IN D ARRAY
C.......................................................................
      RETURN
      END
