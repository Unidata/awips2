C.......................................
C File: tab64.f
C Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
C Date Created: 5/5/06
C Development group: OHD HSEB
C Purpose: This is the operations table entry routine for the
c          distributed hydrological modelling (dhm-op)
c          9/27/07 add one space in PO array to contain value (1 or 0)
c                  indicating whether we need to use RAIN+ MELT grids to ingest
c                  into SAC-SMA
C Module(s): TAB64
C.......................................
C   PASSED ARGUMENTS
C    NAME  I/O
C   ------ ---
C   OpArray O  INT  ARRAY USED TO STORE OPERATIONS TABLE ENTRIES
C   LEFT    I  INT  INDICATING HOW MUCH SPACE IS LEFT IN T()
C   IUSET   O  INT  HOW MUCH SPACE IN THE T() IS USED BY THIS OPER
C   NXT     I  INT  STARTING LOCATION OF THE OpArray FOR THIS OPERATION
C   LPO     I  INT  STARTING LOCATION OF THE PO() FOR THIS OPERATION
C   PO      I  REAL ARRAY TO STORE ALL PARAMETERS FOR THIS OPERATION
C   LCO     I  INT  STARTING LOCATION OF THE CO() FOR THIS OPERATION
C   TS      I  REAL ARRAY OF INFORMATION ON ALL TIME SERIES IN SEGMENT
C   MTS     I  INT  MAXIMUM DIMENSION OF THE TS()
C   LWORK   O  INT  HOW MUCH WORKING SPACE IS NEEDED FOR THIS OPERATION
C   IDT     O  INT  TIME SERIES TIME STEP
C.......................................
C
C   LOCAL VARIABLES
C    NAME
C   ------
C   IBUG    DEBUG SWITCH
C   INEED   NUMBER OF SPACES NEEDED IN THE T()
C   LD      LOCATION OF THE TIME SERIES DATA IN THE D()
C   LTS     LOCATION OF INFORMATION ABOUT THE TIME SERIES IN THE TS()
C.......................................
C
      SUBROUTINE TAB64 (OPARRAY, LEFT, IUSET, NXT, LPO, PO, LCO, TS, 
     1                  MTS,LWORK, IDT)

      INTEGER OPARRAY(1), LEFT, IUSET, NXT, LPO, MTS, LWORK, IDT
      INTEGER IBUG, INEED, IERR, LD, LTS, NINFLOWS
      REAL    PO(1), TS(MTS)
      REAL SNAME(2)
      INTEGER*2  MAXSIZE,MAXINFLOWS
      CHARACTER*4 TIMESERIESTYPE,TSTYPETMP
C.......................................................................
C   COMMON BLOCKS FOR DEBUG UNIT NUMBERS
C      INCLUDE 'COMMON/FDBUG'
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C.......................................................................
C   USE FPROG COMMON BLOCK TO GET NDD IN COMPUTATION OF TS LENGTH IN D()
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source$
     . $',                                                             '
     .$Id$
     . $' /
C    ===================================================================
C
C.......................................................................
C
      DATA SNAME/4HTAB6,4H4   /
      DATA MAXINFLOWS/5/
C
C.......................................................................
C   DEBUG INITIALIZATION WITH TRACE LEVEL = 1
      CALL FPRBUG (SNAME, 1, 64, IBUG)
C.......................................................................
C  IS AN INPUT TS DEFINED
      MAXSIZE = (6*MAXINFLOWS+9)
      NINFLOWS = PO(MAXSIZE-1)
      
C.......................................................................
C   NUMBER OF SPACES I NEED IN T().
      INEED = 5 + NINFLOWS
      
      DO 20 I = 1,INEED           
          OPARRAY(I)= -1
   20 CONTINUE 
      
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
C  GET LOCATION OF INPUT AND/OR OUTPUT TS  
      IDT = PO(5)
      CALL FINDTS(PO(2),PO(4),IDT,LDOUTPUT,LTS,DIMN)
c  Storing inflow data
c  po(8) = tsid (1) - next index after outlet data
c  po(9) = tsid (2)
c  po(10) = tstype
c  po(11) = idt 
c  Where K is index of NINFLOWS  PO(4*K+4)=TSID(1)
c                               PO(4*K+5)=TSID(2)  
c                               PO(4*K+6)=TSTYPE
c                               PO(4*K+7)= idt
c  
      IF (NINFLOWS > 0) THEN 
      DO 10 K = 1, NINFLOWS
         IDT = PO(4*K+7)
         CALL CKINPT(PO(4*K+4),PO(4*K+6),IDT,LDINPUT,TS,MTS,IERR)	 
         OPARRAY(5+K) = LDINPUT
 10   CONTINUE
      ENDIF

C.......................................................................
C  STORE PARMS IN LOCAL T().
      OPARRAY(1) = 64
      OPARRAY(2) = NXT + INEED
      OPARRAY(3) = LPO
      OPARRAY(4) = LCO
      OPARRAY(5) = LDOUTPUT
      
C
C  IF TIME SERIES IS FOUND - MARK TS ARRAY TO INDICATE THAT IT IS 
C  WRITTEN TO
C
      IF (LTS.GT.0) THEN
         TS(LTS+8) = 1.01
      ENDIF
C.......................................................................
C   SET HOW MUCH SPACE IN THE T() IS USED BY THIS OPERATION.
      IUSET = INEED
C.......................................................................
C   NO WORKING SPACE IS USED IN THIS OPERATION.
      LWORK = 0
C.......................................................................
C  CHECK IF DEBUG OUTPUT IS TO BE PRINTED.
      IF(IBUG .GT. 0) THEN
         WRITE(IODBUG,1000) (OPARRAY(I),I=1,INEED)
      END IF
C.......................................................................
C   THE OPARRAY ARRAY ENTRIES ARE AS FOLLOWS:
C   POS
C   ---
C    1      I.D. NUMBER OF THE OPERATION (64)
C    2      LOCATION OF NEXT OPERATION IN T ARRAY (NXT + INEED)
C    3      LOCATION OF PARAMETERS IN P ARRAY (LPO)
C    4      LOCATION OF CARRYOVER IN C ARRAY (LCO)
C    5      LOCATION OF OUTPUT TIME SERIES TS IN D ARRAY
C    6      LOCATION OF INPUT TIME SERIES TS IN D ARRAY
C.......................................................................
1000  FORMAT(' DHM-OP DEBUG - CONTENTS OF OPARRAY()=',6I4)
      RETURN
      END
