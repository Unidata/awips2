C MEMBER TS0326
C  (from old member FCTS0326)
C
      SUBROUTINE TS0326(WORK,IUSEW,LEFTW,NTS03,
     .                  LENDSU,JDEST,IERR)
C--------------------------------------------------------------------
C  ROUTINE TO GET AND CHECK TS SPECIFICATIONS FOR S/U #03 -
C   PRESCRIBED ELEVATION
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
      DIMENSION TSID(2)
      DIMENSION WORK(1)
C
      LOGICAL TSOK,ALLOK,ENDFND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/ts0326.f,v $
     . $',                                                             '
     .$Id: ts0326.f,v 1.1 1995/09/17 18:53:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA TSKEY/4HSHTS/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      ALLOK = .TRUE.
      TSOK = .FALSE.
      ENDFND = .FALSE.
      USEDUP = .FALSE.
      NTS03 = 0
C
      IERR = 0
      IT = 0
C
C  TS FOUND, LOOKING FOR ENDT
C
      LPOS = LSPEC + NCARD + 1
      LASTCD = LENDSU -1
      IBLOCK = 1
C
    5 IF (NCARD .LT. LASTCD) GO TO 8
           CALL STRN26(59,1,SUKYWD(1,9),3)
           IERR = 1
           GO TO 9
    8 NUMFLD = 0
      CALL UFLD26(NUMFLD,IERF)
      IF(IERF .GT. 0 ) GO TO 9000
      NUMWD = (LEN -1)/4 + 1
      IDEST = IKEY26(CHAR,NUMWD,SUKYWD,LSUKEY,NSUKEY,NDSUKY)
      IF (IDEST.EQ.0) GO TO 5
C
C  IDEST = 9 IS FOR ENDT
C
      IF (IDEST.EQ.9.OR.IDEST.EQ.10) GO TO 9
          CALL STRN26(59,1,SUKYWD(1,9),3)
          JDEST = IDEST
          IERR = 99
    9 LENDT = NCARD
C
C  ENDT CARD OR PARMS OR CO FOUND AT LENDT,
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
      IF(NCARD .GE. LENDT) GO TO 200
      NUMWD = (LEN -1)/4 + 1
      IDEST = IUSAME(CHAR,TSKEY,1)
      IF(IDEST .GT. 0) GO TO 100
C
C  NO VALID KEYWORD FOUND
C
      CALL STER26(1,1)
      ALLOK = .FALSE.
      GO TO 10
C
C  NOW SEND CONTROL TO PROPER LOCATION FOR PROCESSING EXPECTED INPUT
C
C
C----------------------------------------------------------------------
C  'SHTS' FOUND. GET THE TS INFO FROM THE REST OF THE LINE.
C
  100 CONTINUE
C
      IT = IT + 1
      IF (IT.GT.1) CALL STER26(39,1)
C
      CALL TSID26(TSID,DTYPE,IDT,TSOK)
      IF (.NOT.TSOK) GO TO 10
C
C  SEE IF TIME INTERVAL IS A MULTIPLE OF THE OPERATION TIME INTERVAL
C
  130 CONTINUE
      IF (MOD(IDT,MINODT).EQ.0) GO TO 140
C
      CALL STER26(47,1)
      GO TO 10
C
C  SEE IF TIME SERIES EXISTS
C
  140 CONTINUE
      CALL CHEKTS(TSID,DTYPE,IDT,0,DCODE,1,1,IERCK)
      IF (IERCK.EQ.0) GO TO 145
C
      CALL STER26(45,1)
      GO TO 10
C
C  CHECK UNITS FOR THIS TIME-SERIES.
C
  145 CONTINUE
      CALL FDCODE(DTYPE,UCODE,X,IX,IX,X,IX,IERD)
      IF (UCODE.EQ.UM) GO TO 150
C
      CALL STRN26(93,1,UCODE,1)
      GO TO 10
C
C  CHECK DIMENSIONS AGAINST ALLOWED FOR THIS TIME SERIES.
C
  150 CONTINUE
      IF (DCODE.EQ.DIML) GO TO 160
C
      CALL STER26(46,1)
      GO TO 10
C
C  SEE IF THIS TIME SERIES WAS ALREADY DEFINED, AND ADD ID TO LIST
C  OF INPUT TIME-SERIES IF IT'S NEW.
C
  160 CONTINUE
      IO = 0
      CALL MLTS26(TSID,DTYPE,IDT,IO,IERM)
      IF (IERM.GT.0) GO TO 10
C
C  INPUT IS OK
C
  190 CONTINUE
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
      NTS03 = 5
      GO TO 9999
C
C--------------------------------------------------------------
C  ERROR IN UFLD26
C
 9000 CONTINUE
      IF (IERF.EQ.1) CALL STER26(19,1)
      IF (IERF.EQ.2) CALL STER26(20,1)
      IF (IERF.EQ.3) CALL STER26(21,1)
      IF (IERF.EQ.4) CALL STER26( 1,1)
C
      IF (NCARD.GE.LASTCD) GO TO 9100
      IF (IBLOCK.EQ.1)  GO TO 5
      IF (IBLOCK.EQ.2)  GO TO 10
C
 9100 USEDUP = .TRUE.
C
 9999 CONTINUE
      RETURN
      END
