C MEMBER CO0526
C  (from old member FCCO0526)
C
      SUBROUTINE CO0526(WORK,IUSEW,LEFTW,NCO05)
C--------------------------------------------------------------------
C  ROUTINE TO GET AND CHECK CO SPECIFICATIONS FOR S/U #05 -
C   FILL AND SPILL SCHEME
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
      DIMENSION WORK(1)
C
      LOGICAL COOK,ALLOK,ENDFND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/co0526.f,v $
     . $',                                                             '
     .$Id: co0526.f,v 1.1 1995/09/17 18:51:20 dws Exp $
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
      NCO05 = 0
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
      IF (NPR.EQ.1) CALL STRN26(59,1,COKEY,1)
      GO TO 200
C
C----------------------------------------------------------------------
C  'OLDQ' FOUND. GET THE CO INFO FROM THE REST OF THE LINE.
C
  100 CONTINUE
C
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
C  NON-SPILLWAY Q MUST BE REAL AND POSITIVE.
C
      IF (ITYPE.LE.1) GO TO 110
      CALL STER26(4,1)
      GO TO 10
C
  110 CONTINUE
      IF (REAL.GE.0.00) GO TO 120
      CALL STER26(95,1)
      GO TO 10
C
  120 CONTINUE
      VALUE = REAL /CONVL3
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
      CALL FLWK26(WORK,IUSEW,LEFTW,VALUE,501)
C
      NCO05 = 1
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
