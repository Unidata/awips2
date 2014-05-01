C MEMBER SABC51
C-------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE SABC51(WK,IUSEW,LEFTW,NCSAB)
C
C DESC READ AND STORE CARRYOVER VALUES
C FOR SAB SUB-SECTIONS OF OPERATION 51
C-------------------------------------------------------------------
C  ARGS:
C     WK - ARRAY TO HOLD ENCODED CARRYOVER INFO
C    IUSEW - NUMBER OF WORDS ALREADY USED IN WORK ARRAY
C    LEFTW - NUMBER OF WORDS LEFT IN WORK ARRAY
C     NCSAB - NUMBER OF WORDS NEEDED TO STORE CO INFO
C-------------------------------------------------------------------
C
C  KUANG HSU - HRL - APRIL 1994
C----------------------------------------------------------------
      INCLUDE 'common/read51'
      INCLUDE 'common/fld51'
      INCLUDE 'common/comn51'
C
      DIMENSION KEYWDS(2,7),LKEYWD(7),VAL(4),WK(*)
      LOGICAL FOUND(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/sabc51.f,v $
     . $',                                                             '
     .$Id: sabc51.f,v 1.1 1996/03/21 14:35:28 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA KEYWDS/
     &   4HQ-IN,4HST  ,4HPOOL,4H    ,4HSTOR,4HAGE ,4HTRIB,4HQL  ,
     &   4HENDC,4H    ,4HENDC,4HO   ,4HENDS,4HAR  /
C
      DATA LKEYWD/2,1,2,2,1,2,2/
      DATA NKEYWD/7/
      DATA NDKEY/2/
C
C  INITIALIZE LOCAL VARIABLES
C
      NCSAB = 0
      USEDUP = .FALSE.
      DO 1 I=1,4
      FOUND(I) = .FALSE.
      VAL(I) = -999.0
    1 CONTINUE
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
      GO TO (100,200,200,200,200,300,300,400), IDEST
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
      ID = IDEST - 1
C
      IF (.NOT.FOUND(ID)) GO TO 205
      CALL STER51(39,1)
      GO TO 10
C
C
  205 CONTINUE
      FOUND(ID) = .TRUE.
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
      VAL(ID) = REAL
      GO TO 10
C----------------------------------------------------------------------
C  'ENDC' FOUND. CHECK TO MAKE SURE REQUIRED HAS BEEN ENTERED AND WAS
C   SUCCESSFULLY DEFINED.
C
  300 CONTINUE
C
C  INSTANTANEOUS DISCHARGE IS REQUIRED; SIGNAL ERROR IF NOT FOUND
C
      IF (FOUND(1)) GO TO 310
      CALL STER51(50,1)
      GO TO 9999
C
C  AT LEAST THE POOL ELEVATION OR THE STORAGE MUST HAVE BEEN ENTERED
C
  310 CONTINUE
      IF (FOUND(2).OR.FOUND(3)) GO TO 320
      CALL STER51(51,1)
      GO TO 9999
C
  320 CONTINUE
C
C  NOW STORE VALUES
C
  330 CONTINUE
C
      CALL FLWK51(WK,IUSEW,LEFTW,VAL(1),501)
C
      IF(IFMSNG(VAL(2)).EQ.1 .AND. IFMSNG(VAL(3)).EQ.1) GO TO 9999
      IF(IFMSNG(VAL(2)).EQ.0 .AND. IFMSNG(VAL(3)).EQ.0) GO TO 350
C
C
C  COMPUTE ELEVATION FROM STORAGE (IF IT'S FOUND.) IF NO ELEVATION
C  WAS ENTERED.
C
      IF(IFMSNG(VAL(2)).EQ.1) THEN
        CALL TERP26(VAL(3),VAL(2),STORGE,ELSTOR,NSE,IFLAG,IBUG)
        GO TO 350
      ENDIF
C
C  IF STORAGE HAS NOT BEEN ENTERED, COMPUTE IT FROM THE ELEVATION AND
C  THE STORAGE ELEVATION CURVE.
C
      IF(IFMSNG(VAL(3)).EQ.1) THEN
        CALL TERP26(VAL(2),VAL(3),ELSTOR,STORGE,NSE,IFLAG,IBUG)
        GO TO 350
      ENDIF
C
  350 CALL FLWK51(WK,IUSEW,LEFTW,VAL(2),501)
      CALL FLWK51(WK,IUSEW,LEFTW,VAL(3),501)
      IF(IFMSNG(VAL(4)).EQ.1) VAL(4)=0.0
      CALL FLWK51(WK,IUSEW,LEFTW,VAL(4),501)
C
      NCSAB = NCSAB + 4
C
      GO TO 9999
C
C-----------------------------------------------------------------------
C  ENDSAB FOUND WHERE IT SHOULDN'T BE. SIGNAL ERROR AND RETURN
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
