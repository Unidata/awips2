C MEMBER PROPT3
C  (from old member FCPROPT3)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/01/2000 BY TLS
C
C @PROCESS LVL(77)
C
      SUBROUTINE PROPT3 (P, MP, C, MC, T, MT, TS, MTS, IOPT, NUMOP,IERR)
C
C  THIS ROUTINE CALLS THE PRINT PARAMETER AND PRINT CARRYOVER
C  ROUTINES FOR OPERATIONS 41-64.
C
C  ROUTINE INITIALLY WRITTEN BY ERIC ANDERSON - HRL   DECEMBER 1991
C
C  ADDED OPERATIONS 61 AND 62
C        TIM SWEENEY, HRL                                 APR 1998
C
C  ADDED OPERATION 55
C        JANICE LEWIS, HRL                                MAR 1999
C
C  ADDED OPERATION 63
C        TIM SWEENEY, HL                                  DEC 2000
C
C  ADDED OPERATION 64 - DISTRIBUTED HYDROLOGICAL MODELLING
C        LEE CAJNA,   HL                                  SEP 2005
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
C  IOPT    I  INT  PRINT OPTION INDICATOR
C  NUMOP   I  INT  NUMBER OF THE OPERATION TO BE PRINTED
C  IERR    O  INT  = 1 IF PRINT ROUTINE FOR THIS OPERATION NOT INCLUDED
C
      INTEGER T(MT), MP, MC, MT, MTS, IOPT, NUMOP, IERR
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/propt3.f,v $
     . $',                                                             '
     .$Id: propt3.f,v 1.11 2006/03/28 15:16:51 aivo Exp $
     . $' /
C  =====================================================================
C
C.......................................................................
C  TRACE LEVEL FOR THIS ROUTINE=1
C
      IF(ITRACE .GE. 1) WRITE(IODBUG,1000)
 1000 FORMAT('0','** PROPT3 ENTERED')
C.......................................................................
C  GO TO THE SECTION FOR THE CURRENT OPERATION AND CALL PRINT
C  PARAMETERS ROUTINE IF PRINT OPTION INDICATOR IS 1 OR 2 AND CALL
C  PRINT CARRYOVER ROUTINE IF PRINT OPTION INDICATOR IS 0 OR 1
C
      NUM = NUMOP - 40
      GO TO (41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
     1       51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
     2       61, 62, 63, 64),NUM
      GO TO 99
C.......................................................................
C  API-HAR2 OPERATION #41 - 2ND VERSION OF THE HARRISBURG RFC API
C
   41 IF (IOPT .GT. 0) CALL PRP41 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC41 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  RSNWELEV OPERATION #42 - RAIN-SNOW ELEVATION
C
   42 IF (IOPT .GT. 0) CALL PRP42 (P(LPM))
      IF (IOPT .LT. 2) THEN
         NEEDC = P(LPM+11)
         IF (NEEDC .EQ. 1) CALL PRC42 (C(LCO))
      ENDIF
      GO TO 100
C.......................................................................
C  API-HFD  OPERATION #43 - HARTFORD API MODEL
C
   43 IF (IOPT .GT. 0) CALL PRP43 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC43 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  SARROUTE OPERATION #44 - SSARR ROUTING
C
   44 IF (IOPT .GT. 0) CALL PRP44 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC44 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  DELTA-TS OPERATION #45 - RATE OF CHANGE OF TIME SERIES
C
   45 IF (IOPT .GT. 0) CALL PRP45 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC45 (P(LPM), C(LCO))
      GO TO 100
C
C.......................................................................
C  NOMSNG   OPERATION #46 - NO MISSING TIME SERIES
   46 IF (IOPT .GT. 0) CALL PRP46 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC46 (P(LPM), C(LCO))
      GO TO 100     
C.......................................................................
C  PEAKFLOW OPERATION #47
C
   47 IF (IOPT .GT. 0) CALL PRP47 (P(LPM))
      GO TO 100
C.......................................................................
C  MULT/DIV OPERATION #48 - MULTIPLY AND DIVIDE TIME SERIES
C
   48 IF (IOPT .GT. 0) CALL PRP48 (P(LPM))
      GO TO 100
C.......................................................................
C  BEGASSIM OPERATION #49 - MARK BEGINNING OF ASSIMILATOR LOOP
C
   49 CONTINUE
      GO TO 100
C.......................................................................
C  ASSIM    OPERATION #50 - ASSIMILATOR
C
   50 IF (IOPT .GT. 0) CALL PRP50 (P(LPM))
      GO TO 100
C.......................................................................
C  SSARRESV OPERATION #51 - SSARR RESERVOIR SIMULATION
C
   51 IF (IOPT .GT. 0) CALL PRP51 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC51 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  SUMPOINT OPERATION #52 - SSARR SUMMING POINT
C
   52 IF (IOPT .GT. 0) CALL PRP52 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC52 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  LOOKUP3  OPERATION #53 - 3 VARIABLE TABLE LOOKUP
C
   53 IF (IOPT .GT. 0) CALL PRP53 (P(LPM))
      GO TO 100
C.......................................................................
C  SWB-NILE OPERATION #54 - NILE SIMPLE WATER BALANCE
C
   54 IF (IOPT .GT. 0) CALL PRP54 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC54 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  FLDWAV   OPERATION #55 - GENERALIZED FLOOD WAVE ROUTING
C
   55 LPMM1=LPM-1
CC      LNB=P(LPMM1+126)
CC      LNQL=P(LPMM1+69)
CC      LNLAD=P(LPMM1+130)
CC      LITWT=P(LPMM1+364)
      LMSG=P(LPMM1+10)

      IF (IOPT .GT. 0) CALL PRP55 (P(LPM),P(LPM),P(LMSG))
      IF (IOPT .LT. 2) CALL PRC55 (P(LPM),P(LPM),C(LCO),C(LCO))
CC      IF (IOPT .LT. 2) CALL PRC55 (P(LPM),C(LCO),P(LNB),P(LNQL),
CC     .  P(LNLAD),C(LITWT))
      GO TO 100
C.......................................................................
C  GLACIER  OPERATION #56 - AKRFC GLACIER
C
   56 IF (IOPT .GT. 0) CALL PRP56 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC56 (C(LCO))
      GO TO 100
C.......................................................................
C  CONS_USE OPERATION #57 - CONSUMPTIVE USE
C
   57 IF (IOPT .GT. 0) CALL PRP57 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC57 (C(LCO))
      GO TO 100
C.......................................................................
C  RES-J OPERATION #58 - JOINT RESERVOIR OPERATIONS
C  EJM - RTi 12/02/97
   58 IF (IOPT .GT. 0) CALL PRP58 (P(LPM))
      IF (IOPT .LT. 2) CALL PRC58 (P(LPM), C(LCO))
      GO TO 100
C.......................................................................
C  TIDEREV  OPERATION #59 - TIDE BALANCE REVIEW
C
   59 IF (IOPT .GT. 0) CALL PRP59 (P(LPM))
      GO TO 100
C.......................................................................
C  ADJUST-T OPERATION #60 - TIDE ADJUSTMENT
C
   60 IF (IOPT .GT. 0) CALL PRP60 (P(LPM))
      GO TO 100
C.......................................................................
C  STAGEREV  OPERATION #61 - STAGE BALANCE REVIEW
C
   61 IF (IOPT .GT. 0) CALL PRP61 (P(LPM))
      GO TO 100
C.......................................................................
C  ADJUST-H OPERATION #62 - STAGE ADJUSTMENT
C
   62 IF (IOPT .GT. 0) CALL PRP62 (P(LPM))
      GO TO 100
C.......................................................................
C  SET-TS OPERATION #63 - SET TIME SERIES TO SPECIFIED VALUE
C
   63 IF (IOPT .GT. 0) CALL PRP63 (P(LPM))
      GO TO 100
C.......................................................................
C  SET-TS OPERATION #64 - DISTRIBUTED HYDROLOGICAL MODELLING
C
   64 IF (IOPT .GT. 0) CALL PRP64 (P(LPM))
      GO TO 100      
C.......................................................................
C  NO PRINT ROUTINE FOR THIS OPERATION SO FLAG AS AN ERROR
C
   99 IERR = 1
C.......................................................................
  100 CONTINUE
      RETURN
      END
