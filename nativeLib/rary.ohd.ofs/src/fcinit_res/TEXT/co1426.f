C MEMBER CO1426
C  (from old member FCCO1426)
C
      SUBROUTINE CO1426(WORK,IUSEW,LEFTW,NCO14,NPER,
     .                 LENDSU,JDEST,IERR)
C--------------------------------------------------------------------
C  ROUTINE TO GET AND CHECK CO SPECIFICATIONS FOR S/U #14 -
C   RULE CURVE ADJUSTMENT
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
      DIMENSION INPUT(2),LINPUT(2),IP(2)
      DIMENSION WORK(1),VALUE(100)
      LOGICAL ENDFND,DEVOK,MSNGOK,ALLOK,DEFALT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/co1426.f,v $
     . $',                                                             '
     .$Id: co1426.f,v 1.1 1995/09/17 18:51:26 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA INPUT/4HMSNG,4HDEV /
      DATA LINPUT/1,1/
      DATA NINPUT/2/
      DATA NDINPU/1/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      NCO14 = 0
      VMSNG = 0.01
      DEVOK = .TRUE.
      MSNGOK = .TRUE.
      ALLOK = .TRUE.
      DEFALT = .TRUE.
C
      DO 3 I = 1,2
           IP(I) = 0
    3 CONTINUE
C
      IERR = 0
C
C  CO FOUND, LOOKING FOR ENDC
C
      LPOS = LSPEC + NCARD + 1
      LASTCD = LENDSU
      IBLOCK = 1
C
    5 IF (NCARD .LT. LASTCD) GO TO 8
           CALL STRN26(59,1,SUKYWD(1,11),3)
           IERR = 99
           GO TO 9
    8 NUMFLD = 0
      CALL UFLD26(NUMFLD,IERF)
      IF(IERF .GT. 0 ) GO TO 9000
      NUMWD = (LEN -1)/4 + 1
      IDEST = IKEY26(CHAR,NUMWD,SUKYWD,LSUKEY,NSUKEY,NDSUKY)
      IF (IDEST.EQ.0) GO TO 5
C
C  IDEST = 11 IS FOR ENDC
C
      IF (IDEST.EQ.11.OR.IDEST.EQ.12) GO TO 9
          CALL STRN26(59,1,SUKYWD(1,11),3)
          JDEST = IDEST
          IERR = 89
    9 LENDP = NCARD
C
C  ENDC CARD OR TS OR PARMS FOUND AT LENDP,
C  ALSO ERR RECOVERY IF NEITHER ONE OF THEM FOUND.
C
C
      IBLOCK = 2
      CALL POSN26(MUNI26,LPOS)
      NCARD = LPOS - LSPEC -1
C
   10 CONTINUE
      NUMFLD = 0
      CALL UFLD26(NUMFLD,IERF)
      IF(IERF .GT. 0) GO TO 9000
      NUMWD = (LEN -1)/4 + 1
      IDEST = IKEY26(CHAR,NUMWD,INPUT,LINPUT,NINPUT,NDINPU)
      IF(IDEST .GT. 0) GO TO 50
      IF(NCARD .GE. LENDP) GO TO 900
C
C  NO VALID KEYWORD FOUND
C
      CALL STER26(1,1)
      ALLOK = .FALSE.
      GO TO 10
C
C  NOW SEND CONTROL TO PROPER LOCATION FOR PROCESSING EXPECTED INPUT
C
   50 CONTINUE
      GO TO (100,200) , IDEST
C-----------------------------------------------------------------
C  'MSNG' KEYWORD EXPECTED. IF FOUND, GET NEXT FIELD ON CARD
C   IT MUST BE INTEGER AND POSITIVE
C
  100 CONTINUE
C
      IP(1) = IP(1) + 1
      IF (IP(1).GT.1) CALL STER26(39,1)
C
C  AN INTEGER, POSITIVE VALUE MUST FOLLOW
C
      MSNGOK = .FALSE.
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.1) GO TO 9000
      IF (IERF.EQ.1) GO TO 140
C
      IF (ITYPE.EQ.0) GO TO 120
      CALL STER26(5,1)
      GO TO 10
C
  120 CONTINUE
C
C  NO. OF MISSING VALUES MUST BE .GE. ZERO
C
      IF (INTEGR.GE.0) GO TO 130
C
      CALL STER26(61,1)
      GO TO 10
C
  130 CONTINUE
      VMSNG = INTEGR + 0.01
C
C  EVERYTHING IS OK
C
  140 CONTINUE
      MSNGOK = .TRUE.
      GO TO 10
C
C----------------------------------------------------------------
C  'DEV' FOUND. EITHER A LIST OF REAL VALUES FOLLOWS OR A NULL FIELD.
C
  200 CONTINUE
      IP(2) = IP(2) + 1
      IF (IP(2).GT.1) CALL STER26(39,1)
C
      NCOLST=100
      CALL GLST26(1,1,X,VALUE,X,NCOLST,DEVOK)
      IF (.NOT.DEVOK) GO TO 10
C
C  IF NO VALUES HAVE BEEN ENTERED, JUST STORE THE DEFAULT VALUES.
C
      IF (NCOLST.EQ.0) GO TO 10
C
C  SEE IF NUMBER READ IN EQUALS THE NUMBER EXPECTED.
C
      DEVOK = .FALSE.
      IF (NCOLST.EQ.NPER) GO TO 210
C
      CALL STER26(62,1)
      GO TO 10
C
  210 CONTINUE
C
C  CONVERT TO METRIC UNITS
C
      DO 250 I=1,NPER
      VALUE(I) = VALUE(I)/CONVL
  250 CONTINUE
C
C  EVERYTHING IS OK
C
  290 CONTINUE
      DEVOK = .TRUE.
      DEFALT = .FALSE.
      GO TO 10
C
C------------------------------------------------------------------
C  'ENDC' FOUND. STORE CO INFO IF INPUT WAS ERROR FREE
C
  900 CONTINUE
      IF (ALLOK.AND.DEVOK.AND.MSNGOK) GO TO 910
C
C  IF ERRORS OCCURRED, JUST EXIT
C
      GO TO 9999
C
  910 CONTINUE
C
C  MUST STORE NUMBER OF ACCUMULATED MISSING OBSERVED
C  VALUES.
C
      CALL FLWK26(WORK,IUSEW,LEFTW,VMSNG,501)
C
      DO 950 I=1,NPER
      VAL = VALUE(I)
      IF (DEFALT) VAL = 0.01
      CALL FLWK26(WORK,IUSEW,LEFTW,VAL,501)
  950 CONTINUE
C
      NCO14 = NPER + 1
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
