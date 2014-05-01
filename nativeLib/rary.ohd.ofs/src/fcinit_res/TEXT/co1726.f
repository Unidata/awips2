C MEMBER CO1726
C  (from old member FCCO1726)
C
      SUBROUTINE CO1726(WORK,IUSEW,LEFTW,NC17,
     .                  LENDSU,JDEST,IERR)
C---------------------------------------------------------------------
C  SUBROUTINE TO READ AND INTERPRET CARRYOVER INPUT FOR S/U #17
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
      INCLUDE 'common/suid26'
C
C
      INCLUDE 'common/suin26'
C
C
      INCLUDE 'common/suky26'
C
C
      INCLUDE 'common/warn26'
C
      DIMENSION INPUT(2,2),LINPUT(2),IC(2),
     . WORK(1),OK(2)
      LOGICAL ENDFND,ALLOK,OK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/co1726.f,v $
     . $',                                                             '
     .$Id: co1726.f,v 1.1 1995/09/17 18:51:27 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA INPUT/4HQDIF,4HF   ,4HSTEP,4HS   /
      DATA LINPUT/2,2/
      DATA NINPUT/2/
      DATA NDINPU/2/
C
C
C  INITIALIZE LOCAL VARIABLES AND COUNTERS
C
      NC17 = 0
      ALLOK = .TRUE.
      QDIFF = 0.01
      STEPS = 0.01
C
      DO 1 I=1,2
      OK(I) = .TRUE.
      IC(I) = 0
    1 CONTINUE
C
C  NOW PROCESS INPUT UP TO 'ENDC'.
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
    9 LENDC = NCARD
C
C  ENDC CARD OR TS OR PARMS FOUND AT LENDC,
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
      IF(NCARD .GE. LENDC) GO TO 900
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
      NPR = IDEST
      GO TO (100,200) , IDEST
C
C-----------------------------------------------------------------------
C  'QDIFF' FOUND. MUST BE REAL VALUE (DIFF. BETWEEN OBS AND SIM)
C   SIM SUBTRACTED FROM OBS.
C
  100 CONTINUE
      IC(NPR) = IC(NPR) + 1
      IF (IC(NPR).GT.1) CALL STER26(39,1)
C
      OK(NPR) = .FALSE.
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.1) GO TO 9000
      IF (IERF.EQ.1) GO TO 150
C
C  MUST BE REAL SPEC FOR DIFF. BETWEEN OBS. AND SIM. DISCHARGES
C
      IF (ITYPE.LE.1) GO TO 130
C
      CALL STER26(4,1)
      GO TO 10
C
C  SET VALUE OF LAST DIFFERENCE
C
  130 CONTINUE
      QDIFF = REAL
C
  150 CONTINUE
      OK(NPR) = .TRUE.
      GO TO 10
C
C-----------------------------------------------------------------
C  'STEPS' KEYWORD FOUND. MUST BE POSITIVE INTEGER VALUE.
C
  200 CONTINUE
C
      IC(NPR) = IC(NPR) + 1
      IF (IC(NPR).GT.1) CALL STER26(39,1)
C
C  READ NEXT FIELD. LOOKING FOR INTEGER POSITIVE VALUE.
C
      OK(NPR) = .FALSE.
      NUMFLD= -2
      CALL UFLD26(NUMFLD,IERF)
      IF(IERF.GT.1)GO TO 9000
      IF (IERF .EQ. 1) GO TO 250
C
      IF(ITYPE.LE.0)GO TO 220
      CALL STER26(5,1)
      GO TO 10
C
  220 CONTINUE
      IF(INTEGR.GE.0)GO TO 230
      CALL STER26(61,1)
      GO TO 10
C
C  SET VALUE OF # OF PREVIOUSLY BLENDED STEPS
C
  230 CONTINUE
      STEPS = INTEGR + 0.01
C
C  EVERYTHING IS OK
C
  250 CONTINUE
      OK(NPR) = .TRUE.
      GO TO 10
C
C--------------------------------------------------------------------
C  END OF INPUT. STORE VALUES IN WORK ARRAY IF EVERYTHING WAS ENTERED
C  WITHOUT ERROR.
C
  900 CONTINUE
C
      DO 910 I=1,2
      IF (.NOT.OK(I)) GO TO 9999
  910 CONTINUE
      IF (.NOT.ALLOK) GO TO 9999
C
C  STORE ALL CARRYOVER VALUES, EITHER THOSE INPUT OR DEFAULT VALUES.
C
      NC17 = 2
      CALL FLWK26(WORK,IUSEW,LEFTW,QDIFF,501)
      CALL FLWK26(WORK,IUSEW,LEFTW,STEPS,501)
C
      GO TO 9999
C
C-----------------------------------------------------------------
C
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
