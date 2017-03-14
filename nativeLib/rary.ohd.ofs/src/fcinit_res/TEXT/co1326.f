C MEMBER CO1326
C  (from old member FCCO1326)
C
      SUBROUTINE CO1326(WORK,IUSEW,LEFTW,NCO13)
C--------------------------------------------------------------------
C  ROUTINE TO GET AND CHECK CO SPECIFICATIONS FOR S/U #13 -
C   POWER GENERATION SCHEME
C---------------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
C
      INCLUDE 'common/comn26'
C
C
      INCLUDE 'common/err26'
C
C
      INCLUDE 'common/fld26'
C
C
      INCLUDE 'common/read26'
C
C
      INCLUDE 'common/suky26'
C
C
      INCLUDE 'common/warn26'
C
C
C
      DIMENSION WORK(1),OLDQ(24)
C
      LOGICAL COOK,ALLOK,ENDFND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/co1326.f,v $
     . $',                                                             '
     .$Id: co1326.f,v 1.1 1995/09/17 18:51:25 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA COKEY/4HOLDQ/
      DATA ENDC/4HENDC/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      NTIM24 = 24/MINODT
      DO 1 I=1,NTIM24
      OLDQ(I) = COQME
    1 CONTINUE
C
      ALLOK = .TRUE.
      COOK = .FALSE.
      ENDFND = .FALSE.
      NCO13 = 0
      NPR = 0
      LASTCD = NSPEC -2
C
C  ONLY INPUT FOR THIS ROUTINE IS LINE WITH CO INFO AND AN 'ENDC'
C  LINE. ALL ELSE IS BOGUS.
C
   10 CONTINUE
      NPR = NPR+1
C
   15 CONTINUE
      IF (NCARD.GE.LASTCD) GO TO 200
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
      GO TO 200
C
C----------------------------------------------------------------------
C  'OLDQ' FOUND. GET THE CO INFO FROM THE REST OF THE LINE.
C
  100 CONTINUE
C
      NOLDQ=24
      CALL GLST26(1,1,IX,OLDQ,X,NOLDQ,COOK)
      IF (.NOT.COOK) GO TO 10
      IF (NOLDQ .EQ. 0) GO TO 150
C
C  MUST HAVE SAME NO. OF VALUES AS TIME PERIODS PER DAY
C  (UNLESS NONE ARE ENTERED, THEN USE DEFAULTS)
C
      NTIMLS = NTIM24 - 1
      IF (MOD(NOLDQ,NTIMLS).EQ.0) GO TO 110
C
      CALL STER26(62,1)
      GO TO 10
C
C  ALL VALUES MUST BE .GE. ZERO
C
  110 CONTINUE
      DO 120 I=1,NOLDQ
      IF (OLDQ(I) .GE. 0.00) GO TO 120
C
      CALL STER26(61,1)
      GO TO 10
  120 CONTINUE
C
C  ALL IS OK WITH CARRYOVER. NOW CONVERT TO TIMD METRIC
C
      DO 130 I=1,NOLDQ
      OLDQ(I) = OLDQ(I)*NTIM24/CONVL3
  130 CONTINUE
C
  150 CONTINUE
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
      DO 220 I=1,NTIM24
      CALL FLWK26(WORK,IUSEW,LEFTW,OLDQ(I),501)
  220 CONTINUE
C
      NCO13 = NTIM24
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
      IF (NCARD .LT. LASTCD ) GO TO 10
      USEDUP = .TRUE.
C
 9999 CONTINUE
      RETURN
      END
