C MEMBER PM1526
C  (from old member FCPM1526)
C
      SUBROUTINE PM1526(WORK,IUSEW,LEFTW,NP15,
     .                  LENDSU,JDEST,IERR)
C---------------------------------------------------------------------
C  SUBROUTINE TO READ AND INTERPRET PARAMETER INPUT FOR PARAMETER #15
C    INFLOW SUMMATION
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
      DIMENSION INPUT(2,2),LINPUT(2),IP(2)
      DIMENSION WORK(1)
      LOGICAL ENDFND,QOK,ELOK,ALLOK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/pm1526.f,v $
     . $',                                                             '
     .$Id: pm1526.f,v 1.1 1995/09/17 18:52:18 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA INPUT/4HNORM,4HQ   ,4HELTE,4HST  /
      DATA LINPUT/2,2/
      DATA NINPUT/2/
      DATA NDINPU/2/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      NP15 = 0
      QOK = .FALSE.
      ELOK = .FALSE.
      ALLOK = .TRUE.
C
      DO 3 I = 1,2
           IP(I) = 0
    3 CONTINUE
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
C  'NORMQ' KEYWORD REQUIRED. IF FOUND, GET NEXT FIELD ON CARD
C   IT MUST BE REAL AND POSITIVE
C
  100 CONTINUE
C
      IP(1) = IP(1) + 1
      IF (IP(1).GT.1) CALL STER26(39,1)
C
C  A REAL, POSITIVE VALUE MUST FOLLOW
C
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      IF (ITYPE.LE.1) GO TO 120
      CALL STER26(4,1)
      GO TO 10
C
  120 CONTINUE
C
C  CONSTANT Q MUST BE .GT. ZERO
C
      IF (REAL.GT.0.0) GO TO 130
C
      CALL STER26(61,1)
      GO TO 10
C
C  CONVERT TO UNITS OF METRIC TIMD
C
  130 CONTINUE
      QNORM = REAL/CONVLT
C THE FOLLOWING CHANGE MADE ON 7/14/89
C     QNORM = QNORM * 24/MINODT
C END OF CHANGE OF 7/14/89
C
C  EVERYTHING IS OK
C
      QOK = .TRUE.
      GO TO 10
C
C----------------------------------------------------------------
C  'ELTEST' REQUIRED. GET NEXT FIELD. MUST BE REAL VALUE AND .GT. ZERO
C   AND IT MUST BE WITHIN THE BOUNDS OF THE ELEV. VS. STORAGE CURVE
C
  200 CONTINUE
      IP(2) = IP(2) + 1
      IF (IP(2).GT.1) CALL STER26(39,1)
C
C  REAL VALUE MUST HAVE BEEN INPUT
C
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      IF (ITYPE.LE.1) GO TO 215
C
      CALL STER26(4,1)
      GO TO 10
C
C  SEE IF VALUE IS POSITIVE
C
  215 CONTINUE
      IF (REAL.GT.0.00) GO TO 220
C
      CALL STER26(61,1)
      GO TO 10
C
C  SEE IF ELEVATION IS WITHIN BOUNDS OF EL VS STORAGE CURVE
C
  220 CONTINUE
      ELTEST = REAL/CONVL
      CALL ELST26(ELTEST,1,IERS)
      IF (IERS.GT.0) GO TO 10
C
C  EVERYTHING IS OK
C
      ELOK = .TRUE.
      GO TO 10
C
C--------------------------------------------------------------------
C  END OF INPUT. STORE VALUES IN WORK ARRAY IF EVERYTHING WAS ENTERED
C  WITHOUT ERROR.
C
  900 CONTINUE
C
      DO 910 I =1,2
           IF(IP(I).EQ.0) CALL STRN26(59,1,INPUT(1,I),LINPUT(I))
  910 CONTINUE
C
      IF (ELOK.AND.QOK.AND.ALLOK) GO TO 1010
      GO TO 9999
C
C  STORE IN WORK THE CONSTANT DISCHARGE AND THE TEST ELEVATION.
C
 1010 CONTINUE
C
      CALL FLWK26(WORK,IUSEW,LEFTW,QNORM,501)
      CALL FLWK26(WORK,IUSEW,LEFTW,ELTEST,501)
      NP15 = NP15 + 2
C
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
