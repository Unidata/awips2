C MEMBER PM1726
C  (from old member FCPM1726)
C
      SUBROUTINE PM1726(WORK,IUSEW,LEFTW,NP17,
     .            LENDSU,JDEST,IERR)
C---------------------------------------------------------------------
C  SUBROUTINE TO READ AND INTERPRET PARAMETER INPUT FOR S/U #17
C    ADJUST UTILITY
C---------------------------------------------------------------------
C  JTOSTROWSKI - HRL - AUGUST 1984
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
      DIMENSION INPUT(2,2),LINPUT(2),IP(2),
     .  CTYPE(2,2),LCT(2),WORK(1)
      LOGICAL ENDFND,CFOK,BLNOK,ALLOK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/pm1726.f,v $
     . $',                                                             '
     .$Id: pm1726.f,v 1.1 1995/09/17 18:52:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA INPUT/
     .            4HBLEN,4HD   ,4HCFAC,4HTOR /
      DATA LINPUT/2,2/
      DATA NINPUT/2/
      DATA NDINPU/2/
C
      DATA CTYPE/4HRATI,4HO   ,4HDIFF,4H    /
      DATA LCT/2,1/
      DATA NCT/2/
      DATA NCD/2/
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      NP17 = 0
      CFOK  = .TRUE.
      BLNOK = .TRUE.
      ALLOK = .TRUE.
      ENDFND = .FALSE.
C
C  DEFAULT FOR CORRECTION FACTOR IS 'DIFF' (= 1.01)
C  DEFAULT FOR NO. OF BLEND PERIODS IS 4
C
      CFACT = 1.01
      BLEND = 4.01
C
      DO 3  I =1,2
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
C  NOW WE'RE LOOKING FOR 'BLEND' AND 'CFACTOR'.
C  (BOTH ARE OPTIONAL)
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
C  'BLEND' FOUND. NEED POSITIVE INTEGER. NULL FIELD INDICATES DEFAULT
C   USE (DEFAULT BLEND INTERVAL IS FOUR PERIODS.)
C
  100 CONTINUE
C
      IP(IDEST) = IP(IDEST) + 1
      IF (IP(IDEST).GT.1) CALL STER26(39,1)
C
C  AN INTEGER VALUE ( OR A NULL FIELD) MUST FOLLOW
C
  110 CONTINUE
      BLNOK = .FALSE.
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.1) GO TO 9000
      IF (IERF.EQ.1) GO TO 150
C
      IF (ITYPE.EQ.0) GO TO 120
      CALL STER26(5,1)
      GO TO 10
C
C  OPTION VALUE MUST BE ZERO OR GREATER
C
  120 CONTINUE
C
      IF (INTEGR.GE.0) GO TO 130
      CALL STER26(61,1)
      GO TO 10
C
  130 CONTINUE
      BLEND = INTEGR + 0.01
C
C  EVERYTHING IS OK
C
  150 CONTINUE
      BLNOK = .TRUE.
      GO TO 10
C
C-----------------------------------------------------------------------
C  'CFACTOR' FOUND. MUST BE FOLLOWED BY 'RATIO' (=0) OR 'DIFF' (=1).
C  DEFAULT IS 'DIFF'.
C
  200 CONTINUE
      IP(IDEST) = IP(IDEST) + 1
      IF (IP(IDEST).GT.1) CALL STER26(39,1)
C
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.1) GO TO 9000
      IF (IERF.EQ.1) GO TO 250
C
C  MUST NOT BE A REAL OR INTEGER NUMBER. MUST BE ALPHA.
C
      IF (ITYPE.EQ.2) GO TO 230
      CALL STER26(1,1)
      GO TO 10
C
  230 CONTINUE
C
C  FIELD MUST BE EITHER 'MEAN' OR 'INST'
C
      NUMWD = (LEN-1)/4 + 1
      IKEY = IKEY26(CHAR,NUMWD,CTYPE,LCT,NCT,NCD)
      IF (IKEY.GT.0) GO TO 240
      CALL STER26(1,1)
      GO TO 10
C
  240 CONTINUE
      CFACT = (IKEY-1) + 0.01
C
  250 CONTINUE
      CFOK = .TRUE.
      GO TO 10
C
C--------------------------------------------------------------------
C  END OF INPUT. STORE VALUES IN WORK ARRAY IF EVERYTHING WAS ENTERED
C  WITHOUT ERROR.
C
  900 CONTINUE
C
      IF (CFOK.AND.BLNOK.AND.ALLOK) GO TO 910
      GO TO 9999
C
  910 CONTINUE
      CALL FLWK26(WORK,IUSEW,LEFTW,BLEND,501)
      CALL FLWK26(WORK,IUSEW,LEFTW,CFACT,501)
      NP17 = 2
C
      GO TO 9999
C
C---------------------------------------------------------------------
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
