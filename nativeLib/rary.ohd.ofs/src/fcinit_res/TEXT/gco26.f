C MEMBER GCO26
C  (from old member FCGCO26)
C
C DESC READ AND STORE GENERAL CARRYOVER VALUES FOR OPERATION 26
C-------------------------------------------------------------------
C
      SUBROUTINE GCO26(WORK,IUSEW,LEFTW,NCOG,LWKCOS)
C
C-------------------------------------------------------------------
C  ARGS:
C     WORK - ARRAY TO HOLD ENCODED CARRYOVER INFO
C    IUSEW - NUMBER OF WORDS ALREADY USED IN WORK ARRAY
C    LEFTW - NUMBER OF WORDS LEFT IN WORK ARRAY
C     NCOG - NUMBER OF WORDS NEEDED TO STORE CO INFO
C-------------------------------------------------------------------
C
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      INCLUDE 'common/read26'
      INCLUDE 'common/fld26'
      INCLUDE 'common/comn26'
C
      DIMENSION KEYWDS(2,9),LKEYWD(9),VAL(6),WORK(1)
      LOGICAL FOUND(6)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/gco26.f,v $
     . $',                                                             '
     .$Id: gco26.f,v 1.1 1995/09/17 18:51:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA KEYWDS/
     .   4HINFL,4HOW  ,4HQ-ME,4HAN  ,4HQ-IN,4HST  ,4HPOOL,4H0   ,
     .   4HPOOL,4H1   ,4HSTOR,4HAGE ,4HENDC,4H    ,4HENDC,4HO   ,
     .   4HENDG,4HENL /
C
      DATA LKEYWD/2,2,2,2,2,2,1,2,2/
      DATA NKEYWD/9/
      DATA NDKEY/2/
C
C  INITIALIZE LOCAL VARIABLES
C
      LWKCOS = 0
      NCOG = 0
      USEDUP = .FALSE.
      DO 1 I=1,6
      FOUND(I) = .FALSE.
      VAL(I) = -999.0
    1 CONTINUE
C
C---------------------------------------------------------------------
C  LOOK FOR VALID KEYWORD
C
   10 CONTINUE
      NUMFLD = 0
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      NUMWD = (LEN-1)/4 + 1
      IDEST = IKEY26(CHAR,NUMWD,KEYWDS,LKEYWD,NKEYWD,NDKEY) + 1
C
      GO TO (100,200,200,200,200,200,200,300,300,400), IDEST
C
C------------------------------------------------------------------
C  NO VALID KEYWORD FOUND
C
  100 CONTINUE
      CALL STER26(1,1)
      GO TO 10
C
C------------------------------------------------------------------
C  VALID CARRYOVER KEYWORD FOUND. LOOK FOR REAL VALUE IN NEXT FIELD
C
  200 CONTINUE
      ID = IDEST - 1
C
      IF (.NOT.FOUND(ID)) GO TO 205
      CALL STER26(39,1)
      GO TO 10
C
C
  205 CONTINUE
      FOUND(ID) = .TRUE.
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      IF (ITYPE.LE.1) GO TO 210
      CALL STER26(4,1)
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
C  THE INFLOW CARRYOVER IS REQUIRED.
C
      IF (FOUND(1)) GO TO 310
      CALL STER26(49,1)
C
C  AT LEAST ONE OF MEAN OR INST. DISCHARGE MUST HAVE BEEN ENTERED
C
  310 CONTINUE
      IF (FOUND(2).OR.FOUND(3)) GO TO 320
      CALL STER26(50,1)
C
C  AT LEAST ONE OF THE POOL ELEVATIONS MUST HAVE BEEN ENTERED.
C
  320 CONTINUE
      IF (FOUND(4).OR.FOUND(5).OR.FOUND(6)) GO TO 330
      CALL STER26(51,1)
C
C  NOW STORE VALUES IF THEY HAVE BEEN ENTERED
C
  330 CONTINUE
      IF(VAL(1).EQ.-999.0) GO TO 9999
C
      VAL(1) = VAL(1)/CONVLT
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL(1),501)
C
C  IF ONLY ONE OF THE DISCHARGES WERE ENTERED, SET THE OTHER TO THE ONE
C  ENTERED.
C
      IF (VAL(2).NE.-999.0.AND.VAL(3).NE.-999.0) GO TO 340
      IF (VAL(2).EQ.-999.0.AND.VAL(3).EQ.-999.0) GO TO 9999
C
      IF (VAL(2).EQ.-999.0) VAL(2) = VAL(3)
      IF (VAL(3).EQ.-999.0) VAL(3) = VAL(2)
C
C  NOW CONVERT TO METRIC
C
  340 CONTINUE
      VAL(2) = VAL(2)/CONVLT
      VAL(3) = VAL(3)/CONVLT
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL(2),501)
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL(3),501)
C
C  AT LEAST ONE OF THE POOL ELEVATIONS MUST HAVE BEEN ENTERED.
C  IF ONLY ONE WAS ENTERED, SET OTHER TO THE ENTERED ONE.
C
      IF (VAL(4).NE.-999.0.AND.VAL(5).NE.-999.0) GO TO 350
      IF (VAL(4).NE.-999.0.OR.VAL(5).NE.-999.0) GO TO 345
C
C  COMPUTE ELEVATION FROM STORAGE (IF IT'S FOUND.) IF NO ELEVATION
C  WAS ENTERED.
C
      IF (VAL(6).EQ.-999.0) GO TO 9999
      CALL NTER26(VAL(6),VAL(4),STORGE,ELSTOR,NSE,IFLAG,INTERP,IBUG)
C
  345 CONTINUE
      IF (VAL(4).EQ.-999.0) VAL(4) = VAL(5)
      IF (VAL(5).EQ.-999.0) VAL(5) = VAL(4)
C
C  CONVERT TO METRIC AND STORE
C
  350 CONTINUE
      VAL(4) = VAL(4)/CONVL
      VAL(5) = VAL(5)/CONVL
C
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL(4),501)
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL(5),501)
C
C  IF STORAGE HAS NOT BEEN ENTERED, COMPUTE IT FROM THE ELEVATION AND
C  THE STORAGE ELEVATION CURVE.
C
      IF (VAL(6).NE.-999.0) GO TO 360
C
      CALL NTER26(VAL(5),VAL(6),ELSTOR,STORGE,NSE,IFLAG,INTERP,IBUG)
C
C  NOW STORED AND WILL BE CONVERTED AT THE END OF GENL26.
C
  360 CONTINUE
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL(6),501)
      LWKCOS = IUSEW
C
      NCOG = NCOG + 6
C
C  SET /COMN26/ VALUES HERE.
C
      COQME = VAL(2)
      COQIN = VAL(3)
      COEL0 = VAL(4)
      COEL1 = VAL(5)
      GO TO 9999
C
C-----------------------------------------------------------------------
C  ENDGENL FOUND WHERE IT SHOULDN'T BE. SIGNAL ERROR AND RETURN
C
  400 CONTINUE
      USEDUP = .TRUE.
      CALL STER26(43,1)
      GO TO 9999
C
C
C---------------------------------------------------------------------
C  ERROR IN UFLD26
C
 9000 CONTINUE
      IF (IERF.EQ.1) CALL STER26(19,1)
      IF (IERF.EQ.2) CALL STER26(20,1)
      IF (IERF.EQ.3) CALL STER26(21,1)
      IF (IERF.EQ.4) CALL STER26(1,1)
C
      IF (IERF.NE.3) GO TO 10
      USEDUP = .TRUE.
C----------------------------------------------------------------
C
 9999 CONTINUE
      RETURN
      END
