C MEMBER CKCO19
C  (from old member FCPIN19)
C
      SUBROUTINE CKCO19(IRDCO,PLWHC,SI,ADC,ITPX)
C.......................................
C     THIS SUBROUTINE CHECKS CARRYOVER VALUES FOR THE 'SNOW-17 '
C        OPERATION.
C.......................................
C     INITIALLY WRITTEN BY... ERIC ANDERSON - HRL   MAY |980

CVK   MODIFIED 4/00 BY V. KOREN TO ADD TWO MORE STATES
cav   modified 4/03 by av to add observed snow depth state
CEA     SHOULD NOT HAVE BEEN ADDED TO SNCO19 - ACTUALLY NEVER
CEA     WRITTEN TO THE CARRYOVER FILES
C
CEA   MODIFIED 11/05 BY E. ANDERSON TO ADD TAPREV
C
CEA   MODIFIED 1/06 BY E. ANDERSON TO CHANGE SB VALUE WHEN ON THE
CEA     DEPLETION CURVE.
C.......................................
      REAL NEGHS,LIQW
      DIMENSION ADC(11)
C
C     COMMON BLOCK.
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
CVK     1STORGE,AEADJ,NEXLAG,EXLAG(7)
CEA     1STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     1STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/ckco19.f,v $
     . $',                                                             '
     .$Id: ckco19.f,v 1.4 2006/10/03 19:33:31 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK IF A SNOW COVER EXISTS.
      IF(IRDCO.EQ.0) GO TO 100
      IF(WE.EQ.0.0) GO TO 100
      GO TO 110
C.......................................
C     SET TO NO SNOW CONDITIONS.
  100 WE=0.0
      NEGHS=0.0
      LIQW=0.0
      TINDEX=0.0
      ACCMAX=0.0
      SB=0.0
      SBAESC=0.0
      SBWS=0.0
C
CVK  TWO MORE STATES
      SNDPT=0.0
      SNTMP=0.0
CEA  ADDED TAPREV
      TAPREV=-99.
C
  105 STORGE=0.0
      AEADJ=0.0
      DO 101 N=1,NEXLAG
  101 EXLAG(N)=0.0
      RETURN
C.......................................
C     SNOW COVER EXISTS -- MAKE CHECKS
  110 IF(NEGHS.LT.0.0) NEGHS=0.0
      IF(LIQW.GT.PLWHC*WE) LIQW=PLWHC*WE
      IF(TINDEX.GT.0.0) TINDEX=0.0
      IF(ACCMAX.LT.(WE+LIQW)) ACCMAX=WE+LIQW
C
CVK    IF NO SNDPT & SNTMP STATES ENTERED, CALCULATE
CVK    THEM BASED ON OTHER AVAILABLE STATES
      IF(SNDPT .LE. 0.0) SNDPT=0.1*WE/0.2
      IF(SNTMP .EQ. 0. .AND. NEGHS .GT. 0.) SNTMP=TINDEX
      IF(SNTMP .GT. 0.0) SNTMP=TINDEX
CEA    IF TAPREV.EQ.0.0, ASSUME WAS NOT ENTERED
CEA      INITIAL VALUE WILL BE OBTAINED WHEN EXECUTION BEGINS
CEA      IN SUBROUTINE EX19 IN THIS CASE.
      IF (TAPREV.EQ.0.0) TAPREV=-99.
C
      IF(IRDCO.EQ.2) GO TO 120
C
C     COMPUTE CARD 10 VALUES.
      TWE=WE+LIQW
      SBWS=TWE
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(TWE.LT.AI) GO TO 111
      SB=TWE
      SBAESC=1.0
      GO TO 105
  111 R=(TWE/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(SBAESC.GT.1.0) SBAESC=1.0
      SB=TWE
CEA      SB=TWE+0.2*ITPX
      GO TO 105
C
C     CARD 10 VALUES READ IN - NO CHECKS
  120 CONTINUE
      RETURN
      END