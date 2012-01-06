C MEMBER CO0226
C  (from old member FCCO0226)
C
      SUBROUTINE CO0226(WORK,IUSEW,LEFTW,NCOVAL,NCO02,ITSTYP)
C--------------------------------------------------------------------
C  ROUTINE TO GET AND CHECK CO SPECIFICATIONS FOR S/U #02 -
C   PRESCRIBED DISCHARGE
C---------------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      INCLUDE 'common/comn26'
      INCLUDE 'common/fld26'
      INCLUDE 'common/read26'
C
      DIMENSION VALUE(50)
      DIMENSION WORK(1)
C
      LOGICAL COOK,ALLOK,ENDFND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/co0226.f,v $
     . $',                                                             '
     .$Id: co0226.f,v 1.2 1997/09/22 15:50:08 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA COKEY/4HOLDQ/
      DATA ENDC/4HENDC/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      ALLOK = .TRUE.
      COOK = .FALSE.
      ENDFND = .FALSE.
      NCO02 = 0
      NPR = 0
C
C  ONLY INPUT FOR THIS ROUTINE IS LINE WITH CO INFO AND AN 'ENDC'
C  LINE. ALL ELSE IS BOGUS.
C
   10 CONTINUE
      NPR = NPR+1
C
   15 CONTINUE
      IF (ENDFND) GO TO 200
      NUMFLD = 0
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      IF (NPR.GT.1) GO TO 20
C
C  SEE IF KEYWORD IS 'OLDQ'
C
      IF (IUSAME(CHAR,COKEY,1).EQ.1) GO TO 100
C
C  SEE IF KEYWORD IS 'ENDC'
C
   20 CONTINUE
      IF (IUSAME(CHAR,ENDC,1).EQ.1) GO TO 40
C
C  NOT A VALID KEYWORD
C
      CALL STER26(1,1)
      ALLOK = .FALSE.
      GO TO 15
C
C  'ENDC' FOUND. GO STORE CO INFO IF INPUT IS OK.
C
   40 CONTINUE
      ENDFND = .TRUE.
      IF (NPR.EQ.1) CALL STRN26(59,1,COKEY,1)
      GO TO 200
C
C----------------------------------------------------------------------
C  'OLDQ' FOUND. GET THE CO INFO FROM THE REST OF THE LINE.
C
  100 CONTINUE
C
      NCOLST=50
      CALL GLST26(1,1,X,VALUE,X,NCOLST,COOK)
      IF (.NOT.COOK) GO TO 10
C
C  SEE IF NUMBER READ IN EQUALS THE NUMBER EXPECTED.
C
      IF (NCOLST.EQ.NCOVAL) GO TO 110
C
      CALL STER26(26,1)
      GO TO 10
C
  110 CONTINUE
C
C  SET CONVERSION OF VALUES TO TIMD IF TIME-SERIES USED FOR INPUTTING
C  DATA IS MEAN DISCHARGE.
C
      NCON = 1
cc      IF (ITSTYP .EQ. 0) NCON = 24/MINODT
C
      DO 120 I=1,NCOVAL
      VALUE(I) = (VALUE(I)/CONVL3) * NCON
C
C  ONLY POSITIVE VALUES ARE ALLOWED
C
      IF (VALUE(I).GE.0.0) GO TO 120
      CALL STER26(95,1)
      COOK = .FALSE.
      GO TO 9999
C
  120 CONTINUE
C
C  EVERYTHING IS OK
C
      COOK = .TRUE.
      GO TO 10
C
C------------------------------------------------------------------
C  'ENDC' FOUND. STORE CO INFO IF INPUT WAS ERROR FREE
C
  200 CONTINUE
      IF (ALLOK.AND.COOK) GO TO 210
C
C  IF ERRORS OCCURRED, JUST EXIT
C
      GO TO 9999
C
  210 CONTINUE
      DO 250 I=1,NCOVAL
      CALL FLWK26(WORK,IUSEW,LEFTW,VALUE(I),501)
  250 CONTINUE
C
      NCO02 = NCOVAL
      GO TO 9999
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
      IF (NCARD .LT. NSPEC-2) GO TO 10
      USEDUP = .TRUE.
C
 9999 CONTINUE
      RETURN
      END
