C MEMBER COXDR3
C  (from old member FCCOXDR3)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/24/97 BY ERB
C
C @PROCESS LVL(77)
C
      SUBROUTINE COXDR3 (POLD, COLD, PONEW, CONEW, D, MD, NUMOP, IERR)
C
C  THIS ROUTINE CALLS THE CARRYOVER TRANSFER ROUTINES FOR
C  OPERATIONS 41 THRU 60.
C
C  ADDED OPERATION 55
C        JANICE LEWIS, HRL                                MAR 1999
C
C  ROUTINE INITIALLY WRITTEN BY ERIC ANDERSON - HRL  DECEMBER 1991
C-----------------------------------------------------------------------
C  PASSED ARGUMENTS
C   NAME  I/O
C  ------ ---
C  POLD    I  REAL PORTION OF OLD P ARRAY FOR CURRENT OPERATION
C  COLD    I  REAL PORTION OF OLD C ARRAY FOR CURRENT OPERATION
C  PONEW   I  REAL PORTION OF NEW P ARRAY FOR CURRENT OPERATION
C  CONEW  I/O REAL PORTION OF NEW C ARRAY FOR CURRENT OPERATION
C  D       I  REAL THE ENTIRE D ARRAY FOR USE AS WORKING SPACE
C  MD      I  INT  DIMENSION OF THE D()
C  NUMOP   I  INT  NUMBER OF THE OPERATION FOR CARRYOVER TRANSFER
C  IERR    O  INT  = 1 IF NO CARRYOVER TRANSFER ROUTINE EXISTS
C
      INTEGER MD, NUMOP, IERR
      REAL    POLD(1), COLD(1), PONEW(1), CONEW(1), D(MD)
C
C  LOCAL VARIABLES
C   NAME
C  ------
C  NUM     NUMBER USED IN COMPUTED GO TO STATEMENT
C
      INTEGER NUM
C.......................................................................
C  COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/coxdr3.f,v $
     . $',                                                             '
     .$Id: coxdr3.f,v 1.7 1999/07/27 14:49:34 page Exp $
     . $' /
C  =====================================================================
C
C.......................................................................
C  TRACE LEVEL=1
C
      IF(ITRACE .GE. 1) WRITE(IODBUG,1000)
 1000 FORMAT(1H0,17H** COXDR3 ENTERED)
C.......................................................................
C  GO TO THE SECTION FOR THE CURRENT OPERATION.
C
      NUM = NUMOP - 40
      GO TO (41, 42, 43, 44, 45, 46, 99, 99, 99, 99,
     1       51, 52, 99, 54, 55, 56, 57 ,58, 99, 99,
     2       99, 99),NUM
      GO TO 99
C.......................................................................
C  API-HAR2 OPERATION #41 - 2ND VERSION OF THE HARRISBURG RFC API
C
   41 CALL COX41 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  RSNWELEV OPERATION #42 - RAIN-SNOW ELEVATION
C
   42 CALL COX42 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  API-HFD  OPERATION #43 - HARTFORD API MODEL
C
   43 CALL COX43 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  SARROUTE OPERATION #44 - SSARR ROUTING
C
   44 CALL COX44 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  DELTA-TS OPERATION #45 - RATE OF CHANGE OF TIME SERIES
C
   45 CALL COX45 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  NOMSNG   OPERATION #46 - NO MISSING VALUES TIME SERIES      
C      
   46 CALL COX46 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  SSARRESV OPERATION #51 - SSARR RESERVOIR SIMULATION OPERATION
C
   51 CALL COX51 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  SUMPOINT OPERATION #52 - SSARR SUMMING POINT OPERATION
C
   52 CALL COX52 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  SWB-NILE OPERATION #54 - NILE SIMPLE WATER BALANCE OPERATION
C
   54 CALL COX54 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  FLDWAV   OPERATION #55 - GENERALIZED FLOOD WAVE ROUTING OPERATION
C
   55 CALL COX55 (POLD, POLD, COLD, PONEW, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  GLACIER  OPERATION #56 - AKRFC GLACIER OPERATION
C
   56 CALL COX56 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  CONS_USE OPERATION #57 - CONSUMPTIVE USE OPERATION
C
   57 CALL COX57 (COLD, CONEW)
      GO TO 100
C.......................................................................
C  RES-J OPERATION #58 - JOINT RESERVOIR OPERATION
C
   58 CALL COX58 (POLD, COLD, PONEW, CONEW)
      GO TO 100
C.......................................................................
C  NO CARRYOVER TRANSFER ROUTINE FOR THIS OPERATION SO FLAG AS AN ERROR
C
   99 IERR = 1
C.......................................................................
  100 CONTINUE
      RETURN
      END
