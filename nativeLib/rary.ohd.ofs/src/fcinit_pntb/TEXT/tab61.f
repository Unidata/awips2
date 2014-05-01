C MEMBER TAB61
C-----------------------------------------------------------------------
C
      SUBROUTINE TAB61 (TO, LEFT, IUSET, NXT, LPO, PO, TS, MTS, 
     1                  LWORK, IDT)
C-----------------------------------------------------------------------
C   THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE
C   STAGE BALANCE REVIEW 'STAGEREV' OPERATION.
C
C   ROUTINE INITIALLY WRITTEN BY Tim Sweeney - HRL  APR 1998
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
C   TS      I  REAL ARRAY OF INFORMATION ON ALL TIME SERIES IN SEGMENT
C   MTS     I  INT  MAXIMUM DIMENSION OF THE TS()
C   LWORK   O  INT  HOW MUCH WORKING SPACE IS NEEDED FOR THIS OPERATION
C
      INTEGER TO(*), LEFT, IUSET, NXT, LPO, MTS, LWORK, IDT
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab61.f,v $
     . $',                                                             '
     .$Id: tab61.f,v 1.1 1998/07/02 17:31:39 page Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C   DEBUG INITIALIZATION WITH TRACE LEVEL = 1
      CALL FPRBUG ('TAB61   ', 1, 61, IBUG)
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
      TO(1) = 61
      TO(2) = NXT + INEED
      TO(3) = LPO
C
C   TIME INTERVAL OF OPERATION AND ALL INPUT TIME SERIES 
      IDT = IFIX(PO(20))
C
C   CALL CKINPT AND GET THE LOCATION OF THE DWOPER OBSERVED STAGE
C   TIME SERIES IN THE D().
      CALL CKINPT (PO(21), PO(23), IDT, LD, TS, MTS, IERR)
      TO(4) = LD
C
C   CALL CKINPT AND GET THE LOCATION OF THE OBS/FORECAST STAGE
C   TIME SERIES IN THE D().
      CALL CKINPT (PO(24), PO(26), IDT, LD, TS, MTS, IERR)
      TO(5) = LD
C
C   TIME INTERVAL OF ALL OUTPUT TIME SERIES 
      IDELT = IFIX(PO(27))
C
C   GET THE LOCATION OF THE RANGE LIMIT TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(28), PO(30), IDELT, LD, LTS, DIM)
      TO(6) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   GET THE LOCATION OF THE TIDE1 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(31), PO(30), IDELT, LD, LTS, DIM)
      TO(7) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   GET THE LOCATION OF THE TIDE2 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(33), PO(30), IDELT, LD, LTS, DIM)
      TO(8) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   GET THE LOCATION OF THE TIDE3 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(35), PO(30), IDELT, LD, LTS, DIM)
      TO(9) = LD
      IF(LTS .GT. 0) TS(LTS+8) = 1.01
C
C   GET THE LOCATION OF THE TIDE4 BALANCE TIME SERIES IN THE D()
C   AND SET FLAG INDICATING THAT THIS TIME SERIES CONTAINS DATA VALUES.
      CALL FINDTS (PO(37), PO(30), IDELT, LD, LTS, DIM)
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
C  CHECK IF DEBUG OUTPUT IS TO BE PRINTED.
      IF(IBUG .GT. 0) THEN
         WRITE(IODBUG,1000) (TO(I),I=1,INEED)
 1000    FORMAT(1H0,'STAGEREV DEBUG - CONTENTS OF TO()=',11I6)
      END IF
C.......................................................................
C   THE TO ARRAY ENTRIES ARE AS FOLLOWS:
C   POS
C   ---
C    1      I.D. NUMBER OF THE OPERATION (61)
C    2      LOCATION OF NEXT OPERATION IN T ARRAY (NXT + INEED)
C    3      LOCATION OF PARAMETERS     IN P ARRAY (LPO)
C    4      LOCATION OF OBSERVED STAGE        TS IN D ARRAY
C    5      LOCATION OF OBS/FORECAST STAGE    TS IN D ARRAY
C    6      LOCATION OF RANGE LIMIT           TS IN D ARRAY
C    7      LOCATION OF TIDE1 BALANCE         TS IN D ARRAY
C    8      LOCATION OF TIDE2 BALANCE         TS IN D ARRAY
C    9      LOCATION OF TIDE3 BALANCE         TS IN D ARRAY
C   10      LOCATION OF TIDE4 BALANCE         TS IN D ARRAY
C.......................................................................
      RETURN
      END
