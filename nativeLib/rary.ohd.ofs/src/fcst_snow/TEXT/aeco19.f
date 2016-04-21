C MEMBER AECO19
C  (from old member FCPACK19)
C
      SUBROUTINE AECO19 (COVER,TWE,SI,ADC)
C.......................................
C     THIS SUBROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A
C        CHANGE IN THE AREAL EXTENT OF THE SNOW COVER.
C        USED IN THE 'SNOW-17' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON-HRL APRIL 1981
C
CVK      MODIFIED 4/00 BY V. KOREN: NEW STATES, SNDPT & SNTMP ADDED
C
CEA      MODIFIED 11/05 BY E. ANDERSON TO ADD TAPREV TO SNCO19 COMMON
CEA        - NO OTHER CHANGES NEEDED.
C
CEA      MODIFIED 1/06 BY E. ANDERSON FOR MODIFICATIONS TO HOW NEW
CEA         SNOWFALL VALUES BELOW SNOF ARE HANDLED.
C.......................................
      REAL NEGHS,LIQW
      DIMENSION ADC(11)
C
C     COMMON BLOCK
CVK   ADDED TWO MORE STATES
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,
CVK     1AEADJ,NEXLAG,EXLAG(7)
CEA     1AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     1AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
      COMMON/SNUP19/MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow/RCS/aeco19.f,v $
     . $',                                                             '
     .$Id: aeco19.f,v 1.4 2006/10/03 19:33:03 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     DETERMINE IF CURRENTLY ON DEPLETION CURVE OR NEW SNOW LINE.
      TEX=0.0
      DO 90 N=1,NEXLAG
   90 TEX=TEX+EXLAG(N)
      FREEW=STORGE+TEX
      SWE=TWE-FREEW
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      IF(SWE.GE.AI) GO TO 100
      IF(SWE.LE.SB) GO TO 100
C.......................................
C     CURRENTLY ON NEW SNOW LINE.
C     IF COVER LE SBAESC THEN BACK ON DEPLETION CURVE
      IF (COVER.LE.SBAESC) GO TO 100
      R=(SWE/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      AESC=ADC(N)+(ADC(N+1)-ADC(N))*R
C     IF COVER LE DEPLETION CURVE VALUE FOR CURRENT SWE THEN BACK
C       ON DEPLETION CURVE
      IF(COVER.LE.AESC) GO TO 100
C     ADJUST SBWS , LEAVE AEADJ AS IS.
      SBWS=((1.0-SBAESC)/(COVER-SBAESC))*(SWE-SB)+SB
CEA   FOLLOWING STATEMENTS NO LONGER NEEDED
CEA      IF (SBWS.GT.SB+0.75*SNOF) RETURN
CEA      SB=SBWS-0.75*SNOF
CEA      R=(SB/AI)*10.0+1.0
CEA      N=R
CEA      FN=N
CEA      R=R-FN
CEA      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      RETURN
C.......................................
C     CURRENTLY OR SHOULD BE ON THE DEPLETION CURVE.
  100 DO 101 I=2,11
      IF (COVER.GE.ADC(I)) GO TO 101
      J=I-1
      FJ=J-1
      WEAI=0.1*(FJ+ (COVER-ADC(J))/(ADC(I)-ADC(J)))
      GO TO 105
  101 CONTINUE
      WEAI=1.0
  105 AEADJ=SWE/WEAI
      SBAESC=COVER
C.......................................
      RETURN
      END