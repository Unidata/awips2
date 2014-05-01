C MEMBER TAB63
C-----------------------------------------------------------------------
C
      SUBROUTINE TAB63 (TO, LEFT, IUSET, NXT, LPO, PO, TS, MTS, 
     1                  LWORK, IDT)
C-----------------------------------------------------------------------
C   THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE
C   SET TIME SERIES 'SET-TS' OPERATION.
C
C   ROUTINE INITIALLY WRITTEN BY Tim Sweeney - HRL  DEC 2000
C
C   CORRECTED USE OF P() INDICES AND IMPLEMENTED USE OF COMPUTED LENGTH 
C   OF TS IN D()
C                                JASON SPERFSLAGE - HRC  DECEMBER 2000
C
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
      INTEGER TO(1), LEFT, IUSET, NXT, LPO, MTS, LWORK, IDT
      DIMENSION    PO(1), TS(MTS)
C
C   LOCAL VARIABLES
C    NAME
C   ------
C   IBUG    DEBUG SWITCH
C   INEED   NUMBER OF SPACES NEEDED IN THE T()
C   LD      LOCATION OF THE TIME SERIES DATA IN THE D()
C   LTS     LOCATION OF INFORMATION ABOUT THE TIME SERIES IN THE TS()
C
      INTEGER IBUG, INEED, IERR, LD, LTS
      DIMENSION SNAME(2)
C.......................................................................
C   COMMON BLOCKS FOR DEBUG UNIT NUMBERS
C      INCLUDE 'common/fdbug'
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C.......................................................................
C   USE FPROG COMMON BLOCK TO GET NDD IN COMPUTATION OF TS LENGTH IN D()
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C.......................................................................
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab63.f,v $
     . $',                                                             '
     .$Id: tab63.f,v 1.1 2002/05/15 13:41:31 hank Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/4HTAB6,4H3   /
C
C.......................................................................
C   DEBUG INITIALIZATION WITH TRACE LEVEL = 1
      CALL FPRBUG (SNAME, 1, 63, IBUG)
C.......................................................................
C   NUMBER OF SPACES I NEED IN T().
      INEED = 5
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
C  CHECK IF THE SPECIFIED TIME SERIES EXISTS, GET LTS TO COMPUTE LENGTH
      IDT = PO(5)
      CALL FINDTS(PO(2),PO(4),IDT,LD,LTS,DIM)
C  COMPUTE LENGTH OF TIME SERIES
      NPDT=TS(LTS+6)
      LENGTH=NDD*(24/IDT)*NPDT
C.......................................................................
C  STORE PARMS IN LOCAL T().
      TO(1) = 63
      TO(2) = NXT + INEED
      TO(3) = LPO
      TO(4) = LD
      TO(5) = LENGTH
C
C  IF TIME SERIES IS FOUND - MARK TS ARRAY TO INDICATE THAT IT IS 
C  WRITTEN TO
C
      IF (LTS.GT.0) TS(LTS+8)=1.01
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
 1000    FORMAT(' SET-TS DEBUG - CONTENTS OF TO()=',5I4)
      END IF
C.......................................................................
C   THE TO ARRAY ENTRIES ARE AS FOLLOWS:
C   POS
C   ---
C    1      I.D. NUMBER OF THE OPERATION (63)
C    2      LOCATION OF NEXT OPERATION IN T ARRAY (NXT + INEED)
C    3      LOCATION OF PARAMETERS IN P ARRAY (LPO)
C    4      LOCATION OF TIME SERIES TS IN D ARRAY
C    5      LENTGH OF TIME SERIES IN D ARRAY
C.......................................................................
      RETURN
      END
