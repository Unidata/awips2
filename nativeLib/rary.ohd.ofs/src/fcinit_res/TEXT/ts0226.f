C MEMBER TS0226
C  (from old member FCTS0226)
C
      SUBROUTINE TS0226(WORK,IUSEW,LEFTW,NTS02,ITSTYP)
C--------------------------------------------------------------------
C  ROUTINE TO GET AND CHECK TS SPECIFICATIONS FOR S/U #02 -
C   PRESCRIBED DISCHARGE
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
      DIMENSION TSID(2),DISTQ(2)
      DIMENSION WORK(1)
C
      LOGICAL TSOK,ALLOK,ENDFND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/ts0226.f,v $
     . $',                                                             '
     .$Id: ts0226.f,v 1.1 1995/09/17 18:53:12 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA TSKEY/4HSQTS/
      DATA ENDT/4HENDT/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      ALLOK = .TRUE.
      TSOK = .FALSE.
      ENDFND = .FALSE.
      NTS02 = 0
      NPR = 0
C
C  ONLY INPUT FOR THIS ROUTINE IS LINE WITH TS INFO AND AN 'ENDT'
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
C  SEE IF KEYWORD IS 'SQTS'
C
      IF (IUSAME(CHAR,TSKEY,1).EQ.1) GO TO 100
C
C  SEE IF KEYWORD IS 'ENDT'
C
   20 CONTINUE
      IF (IUSAME(CHAR,ENDT,1).EQ.1) GO TO 40
C
C  NOT A VALID KEYWORD
C
      CALL STER26(1,1)
      ALLOK = .FALSE.
      IF (NCARD .LT. NSPEC-2) GO TO 15
      USEDUP = .TRUE.
      GO TO 9999
C
C  'ENDT' FOUND. GO STORE TS INFO IF INPUT IS OK.
C
   40 CONTINUE
      ENDFND = .TRUE.
      IF (NPR.EQ.1) CALL STRN26(59,1,TSKEY,1)
      GO TO 200
C
C----------------------------------------------------------------------
C  'SQTS' FOUND. GET THE TS INFO FROM THE REST OF THE LINE.
C
  100 CONTINUE
C
      CALL TSID26(TSID,DTYPE,IDT,TSOK)
      IF (.NOT.TSOK) GO TO 10
C
C  SEE IF TIME INTERVAL MATCHES THE OPERATION TIME INTERVAL
C
      TSOK = .FALSE.
      IF (IDT.EQ.MINODT) GO TO 170
      CALL STER26(48,1)
      GO TO 10
C
C  SEE IF TIME SERIES EXISTS
C
  170 CONTINUE
      CALL CHEKTS(TSID,DTYPE,IDT,0,DCODE,1,1,IERCK)
      IF (IERCK.EQ.0) GO TO 175
C
      CALL STER26(45,1)
      GO TO 10
C
C  CHECK UNITS FOR THIS TIME-SERIES.
C
  175 CONTINUE
      CALL FDCODE(DTYPE,UCODE,X,IX,IX,X,IX,IERD)
      IF (UCODE.EQ.UCMS.OR.UCODE.EQ.UCMSD) GO TO 180
C
      CALL STRN26(93,1,UCODE,1)
      GO TO 10
C
C  CHECK DIMENSIONS AGAINST ALLOWED FOR THIS TIME SERIES.
C
  180 CONTINUE
      IF (DCODE.EQ.DIML3.OR.DCODE.EQ.DIMLT) GO TO 185
C
      CALL STER26(46,1)
      GO TO 10
C
C  SEE IF THIS TIME SERIES WAS ALREADY DEFINED, AND ADD ID TO LIST
C  OF INPUT TIME-SERIES IF IT'S NEW.
C
  185 CONTINUE
      IO = 0
      CALL MLTS26(TSID,DTYPE,IDT,IO,IERM)
      IF (IERM.GT.0) GO TO 10
C
C  SET TYPE OF TIME SERIES (NEEDED FOR CARRYOVER CONVERSION)
C
      ITSTYP = 1
      IF (DCODE .EQ. DIML3) ITSTYP = 0
C
C  INPUT IS OK
C
      TSOK = .TRUE.
      GO TO 10
C
C------------------------------------------------------------------
C  'ENDT' FOUND. STORE TS INFO IF INPUT WAS ERROR FREE
C
  200 CONTINUE
      IF (ALLOK.AND.TSOK) GO TO 210
C
C  IF ERRORS OCCURRED, JUST EXIT
C
      GO TO 9999
C
  210 CONTINUE
      DT = IDT + 0.01
      CALL FLWK26(WORK,IUSEW,LEFTW,TSID(1),501)
      CALL FLWK26(WORK,IUSEW,LEFTW,TSID(2),501)
      CALL FLWK26(WORK,IUSEW,LEFTW,DTYPE,501)
      CALL FLWK26(WORK,IUSEW,LEFTW,DT,501)
      CALL FLWK26(WORK,IUSEW,LEFTW,0.01,501)
C
      NTS02 = 5
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
