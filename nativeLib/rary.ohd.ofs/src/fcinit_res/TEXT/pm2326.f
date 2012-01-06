C MEMBER PM2326
C  (from old member FCPM2326)
C
      SUBROUTINE PM2326(WORK,IUSEW,LEFTW,NP23,
     .                  LENDSU,JDEST,IERR)
C--------------------------------------------------------------------
C  ROUTINE TO GET AND CHECK PARAMETERS FOR S/U #23 -
C   ASSIGN MINIMUM VALUE
C---------------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
C
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
      DIMENSION KEYWD(3),LKEY(3)
      DIMENSION WORK(1)
C
      LOGICAL MSPOK,ALLOK,ENDFND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/pm2326.f,v $
     . $',                                                             '
     .$Id: pm2326.f,v 1.1 1995/09/17 18:52:24 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA KEYWD/4HINST,4HMEAN,4HPOOL/
      DATA LKEY/1,1,1/
C
      DATA TYPE/4HTYPE/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      ALLOK = .TRUE.
      MSPOK = .FALSE.
      ENDFND = .FALSE.
      USEDUP = .FALSE.
C
      NP23 = 0
C
C
      IP = 0
C
      IERR = 0
C
C  PARMS FOUND, LOOKING FOR ENDP
C
      LPOS = LSPEC + NCARD + 1
      LASTCD = LENDSU
      IBLOCK = 1
C
    5 IF (NCARD .LT. LASTCD) GO TO 8
           CALL STRN26(59,1,SUKYWD(1,7),3)
           IERR = 99
           GO TO 9
    8 NUMFLD = 0
      CALL UFLD26(NUMFLD,IERF)
      IF(IERF .GT. 0 ) GO TO 9000
      NUMWD = (LEN -1)/4 + 1
      IDEST = IKEY26(CHAR,NUMWD,SUKYWD,LSUKEY,NSUKEY,NDSUKY)
      IF (IDEST.EQ.0) GO TO 5
C
C  IDEST = 7 IS FOR ENDP
C
      IF (IDEST.EQ.7.OR.IDEST.EQ.8) GO TO 9
          CALL STRN26(59,1,SUKYWD(1,7),3)
          JDEST = IDEST
          IERR = 89
    9 LENDP = NCARD
C
C  ENDP CARD OR TS OR CO FOUND AT LENDP,
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
      IF(NCARD .GE. LENDP) GO TO 900
      IF (IUSAME(CHAR,TYPE,1).EQ.1) GO TO 100
C
C  NO VALID KEYWORD FOUND
C
      CALL STRN26(59,1,TYPE,1)
      ALLOK = .FALSE.
      GO TO 10
C
C----------------------------------------------------------------------
C  'TYPE' FOUND, NOW GET SPECIFICATION
C
  100 CONTINUE
C
      IP = IP + 1
      IF (IP.GT.1) CALL STER26(39,1)
C
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
C  SEE IF WHAT'S FOUND IS AN ALLOWED SPEC
C
      IKEY = IKEY26(CHAR,1,KEYWD,LKEY,3,1)
      IF (IKEY.GT.0) GO TO 110
      CALL STER26(1,1)
      GO TO 10
C
  110 CONTINUE
      OPT = IKEY + 0.01
      MSPOK = .TRUE.
      GO TO 10
C
C------------------------------------------------------------------
C  'ENDP' FOUND. STORE TS INFO IF INPUT WAS ERROR FREE
C
  900 CONTINUE
C
      IF (IP.EQ.0) CALL STRN26(59,1,TYPE,1)
C
      IF (.NOT.(ALLOK.AND.MSPOK)) GO TO 9999
C
C  IF ERRORS OCCURRED, JUST EXIT
C
      CALL FLWK26(WORK,IUSEW,LEFTW,OPT,501)
C
      NP23 = 1
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
C
      RETURN
      END
