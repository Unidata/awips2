C MEMBER INFC51
C-------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE INFC51(WK,IUSEW,LEFTW,NCINF)
C
C DESC READ AND STORE INFLOW CARRYOVER VALUES FOR OPERATION 51
C
C-------------------------------------------------------------------
C  ARGS:
C     WK - ARRAY TO HOLD ENCODED CARRYOVER INFO
C    IUSEW - NUMBER OF WORDS ALREADY USED IN WORK ARRAY
C    LEFTW - NUMBER OF WORDS LEFT IN WORK ARRAY
C     NCINF - NUMBER OF WORDS NEEDED TO STORE CO INFO
C-------------------------------------------------------------------
C
C  KUANG HSU - HRL - APRIL 1994
C----------------------------------------------------------------
      INCLUDE 'common/read51'
      INCLUDE 'common/fld51'
      INCLUDE 'common/comn51'
C
      DIMENSION KEYWDS(2,4),LKEYWD(4),WK(1)
      LOGICAL FOUND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/infc51.f,v $
     . $',                                                             '
     .$Id: infc51.f,v 1.1 1996/03/21 14:28:17 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA KEYWDS/
     .   4HQ-IN,4HST  ,4HENDC,4H    ,4HENDC,4HO   ,
     .   4HENDI,4HNFLW/
C
      DATA LKEYWD/2,1,2,2/
      DATA NKEYWD/4/
      DATA NDKEY/2/
C
C  INITIALIZE LOCAL VARIABLES
C
      NCINF = 0
      USEDUP = .FALSE.
      FOUND = .FALSE.
      VAL = -999.0
C
C---------------------------------------------------------------------
C  LOOK FOR VALID KEYWORD
C
   10 CONTINUE
      NUMFLD = 0
      CALL UFLD51(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      NUMWD = (LEN-1)/4 + 1
      IDEST = IKEY26(CHAR,NUMWD,KEYWDS,LKEYWD,NKEYWD,NDKEY) + 1
C
      GO TO (100,200,300,300,400), IDEST
C
C------------------------------------------------------------------
C  NO VALID KEYWORD FOUND
C
  100 CONTINUE
      CALL STER51(1,1)
      GO TO 10
C
C------------------------------------------------------------------
C  VALID CARRYOVER KEYWORD FOUND. LOOK FOR REAL VALUE IN NEXT FIELD
C
  200 CONTINUE
C
      IF (.NOT.FOUND) GO TO 205
      CALL STER51(39,1)
      GO TO 10
C
C
  205 CONTINUE
      FOUND = .TRUE.
      NUMFLD = -2
      CALL UFLD51(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      IF (ITYPE.LE.1) GO TO 210
      CALL STER51(4,1)
      GO TO 10
C
C  HAVE REAL VALUE FOR CARRYOVER QUANTITY
C
  210 CONTINUE
      VAL = REAL
      GO TO 10
C----------------------------------------------------------------------
C  'ENDC' FOUND. CHECK TO MAKE SURE REQUIRED HAS BEEN ENTERED AND WAS
C   SUCCESSFULLY DEFINED.
C
  300 CONTINUE
C
C  THE INFLOW CARRYOVER IS REQUIRED.
C
      IF (FOUND) GO TO 310
      CALL STER51(49,1)
      GO TO 9999
  310 CONTINUE
      IF(VAL.EQ.-999.0) GO TO 9999
C
      CALL FLWK51(WK,IUSEW,LEFTW,VAL,501)
C
      NCINF = NCINF + 1
      GO TO 9999
C
C-----------------------------------------------------------------------
C  ENDINFLW FOUND WHERE IT SHOULDN'T BE. SIGNAL ERROR AND RETURN
C
  400 CONTINUE
      USEDUP = .TRUE.
      CALL STER51(43,1)
      GO TO 9999
C
C
C---------------------------------------------------------------------
C  ERROR IN UFLD51
C
 9000 CONTINUE
      IF (IERF.EQ.1) CALL STER51(19,1)
      IF (IERF.EQ.2) CALL STER51(20,1)
      IF (IERF.EQ.3) CALL STER51(21,1)
      IF (IERF.EQ.4) CALL STER51(1,1)
C
      IF (IERF.NE.3) GO TO 10
      USEDUP = .TRUE.
C----------------------------------------------------------------
C
 9999 CONTINUE
      RETURN
      END
