C MEMBER PUOPT3
C  (from old member FCPUOPT3)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/01/2000 BY TLS
C
C @PROCESS LVL(77)
C
      SUBROUTINE PUOPT3 (P, MP, C, MC, T, MT, TS, MTS, NUMOP, IERR)
C
C  THIS ROUTINE CALLS THE PUNCH CARD ROUTINES FOR OPERATIONS 41 THRU 62.
C
C  ROUTINE INITIALLY WRITTEN BY ERIC ANDERSON - HRL  DECEMBER 1991
C
C  ADDED OPERATIONS 61 AND 62.
C        TIM SWEENEY, HRL                                 APR 1998
C
C  ADDED OPERATION 55
C        JANICE LEWIS, HRL                                MAR 1999
C
C  ADDED OPERATION 63
C        TIM SWEENEY, HL                                  DEC 2000
C
C  ADDED OPERATION 64 DHM-OP
C                   , HL                                  SEPT 2005
C-----------------------------------------------------------------------
C  PASSED ARGUMENTS
C   NAME  I/O
C  ------ ---
C  P       I  REAL THE ENTIRE P ARRAY
C  MP      I  INT  DIMENSION OF THE P()
C  C       I  REAL THE ENTIRE C ARRAY
C  MC      I  INT  DIMENSION OF THE C()
C  T       I  INT  THE ENTIRE T ARRAY
C  MT      I  INT  DIMENSION OF THE T()
C  TS      I  REAL THE ENTIRE TS ARRAY
C  MTS     I  INT  DIMENSION OF THE TS()
C  NUMOP   I  INT  NUMBER OF THE OPERATION TO BE PUNCHED
C  IERR    O  INT  = 1 IF PUNCH ROUTINE FOR THIS OPERATION NOT INCLUDED
C
      INTEGER T(MT), MP, MC, MTS, NUMOP, IERR
      REAL    P(MP), C(MC), TS(MTS)
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
      COMMON/FCOPPT/LOCT,LPM,LCO
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/puopt3.f,v $
     . $',                                                             '
     .$Id: puopt3.f,v 1.12 2006/05/09 16:34:19 aivo Exp $
     . $' /
C  =====================================================================
C
C.......................................................................
C  TRACE LEVEL=1
C
      IF (ITRACE .GE. 1) WRITE(IODBUG,1000)
C.......................................................................
C  GO TO THE SECTION FOR THE CURRENT OPERATION.
C
      NUM = NUMOP - 40
      GO TO (41, 42, 43, 44, 45, 46, 99, 48, 49, 50,
     1       51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
     2       61, 62, 63, 64),NUM
      GO TO 99
C.......................................................................
C  API-HAR2 OPERATION #41 - 2ND VERSION OF THE HARRISBURG RFC API
C
   41 CALL PUC41 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  RSNWELEV OPERATION #42 - RAIN-SNOW ELEVATION
C
   42 CALL PUC42 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  API-HFD  OPERATION #43 - HARTFORD API MODEL
C
   43 CALL PUC43 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  SARROUTE OPERATION #44 - SSARR ROUTING
C
   44 CALL PUC44 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  DELTA-TS OPERATION #45 - RATE OF CHANGE OF TIME SERIES
C
   45 CALL PUC45 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  NOMSNG   OPERATION #46 - NO MISSING VALUES TIME SERIES
   46 CALL PUC46 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  MULT/DIV OPERATION #48 - MULTIPLY AND DIVIDE TIME SERIES
C
   48 CALL PUC48 (P(LPM))
      GO TO 100
C.......................................................................
C  BEGASSIM OPERATION #49 - MARK BEGINNING OF ASSIMILATOR LOOP
C
   49 CONTINUE
      GO TO 100
C.......................................................................
C  ASSIM    OPERATION #50 -  ASSIMILATOR
C
   50 CALL PUC50 (P(LPM))
      GO TO 100
C.......................................................................
C  SSARRESV OPERATION #51 - SSARR RESERVOIR SIMULATION
C
   51 CALL PUC51 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  SUMPOINT OPERATION #52 - SSARR SUMMING POINT
C
   52 CALL PUC52 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  LOOKUP3  OPERATION #53 - 3 VARIABLE TABLE LOOKUP
C
   53 CALL PUC53 (P(LPM))
      GO TO 100
C.......................................................................
C  SWB-NILE OPERATION #54 - NILE SIMPLE WATER BALANCE
C
   54 CALL PUC54 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  FLDWAV   OPERATION #55 - GENERALIZED FLOOD WAVE ROUTING
C
   55 CALL PUC55 (P(LPM), P(LPM), C(LCO), C(LCO))
      GO TO 100
C.......................................................................
C  GLACIER  OPERATION #56 - AKRFC GLACIER
C
   56 CALL PUC56 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  CONS_USE OPERATION #57 - CONSUMPTIVE USE
C
   57 CALL PUC57 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  RES-J OPERATION #58 - JOINT RESERVOIR OPERATIONS
C  EJM - RTi  12/02/97
   58 CALL PUC58 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  TIDEREV  OPERATION #59 - TIDE BALANCE REVIEW
C
   59 CALL PUC59 (P(LPM))
      GO TO 100
C.......................................................................
C  ADJUST-T OPERATION #60 - TIDE ADJUSTMENT
C
   60 CALL PUC60 (P(LPM))
      GO TO 100
C.......................................................................
C  STAGEREV  OPERATION #61 - STAGE BALANCE REVIEW
C
   61 CALL PUC61 (P(LPM))
      GO TO 100
C.......................................................................
C  ADJUST-H OPERATION #62 - STAGE ADJUSTMENT
C
   62 CALL PUC62 (P(LPM))
      GO TO 100
C.......................................................................
C  SET-TS OPERATION #63 - SET TIME SERIES TO SPECIFIED VALUE
C
   63 CALL PUC63 (P(LPM))
      GO TO 100
C.......................................................................
C  DHM-OP OPERATION #64 - SET DHM-OP
C
   64 CALL PUC64 (P(LPM))
      GO TO 100
C.......................................................................
C  NO PUNCH ROUTINE FOR THIS OPERATION SO FLAG AS AN ERROR
C
   99 IERR = 1
C.......................................................................
  100 CONTINUE
1000  FORMAT('0','** PUOPT3 ENTERED')
      RETURN
      END
