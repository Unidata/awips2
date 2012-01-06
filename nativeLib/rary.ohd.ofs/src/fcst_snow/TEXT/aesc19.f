C MEMBER AESC19
C  (from old member FCPACK19)
C
      SUBROUTINE AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
C.......................................
C     THIS SUBROUTINE COMPUTES THE AREAL EXTENT OF SNOW COVER USING THE
C        AREAL DEPLETION CURVE FOR THE 'SNOW-17 ' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980
C
CEA     MODIFIED 1/06 BY E. ANDERSON TO CHANGE LOGIC FOR WHEN AEADJ IS
CEA       SET BACK TO ZERO AND FOR MODIFICATIONS ON HOW SNOF IS HANDLED.
C.......................................
      REAL LIQW
      DIMENSION ADC(11)
C
C     COMMON BLOCKS
      COMMON/SNUP19/MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow/RCS/aesc19.f,v $
     . $',                                                             '
     .$Id: aesc19.f,v 1.3 2006/10/03 19:33:19 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
      TWE=WE+LIQW
      IF (AEADJ.EQ.0.0) GO TO 100
C     DETERMINE IF AEADJ SHOULD BE REMOVED - SET TO ZERO
      OLDAI=ACCMAX
      IF (ACCMAX.GT.SI) OLDAI=SI
      IF ((AEADJ.LT.OLDAI).AND.(TWE.GE.OLDAI)) AEADJ=0.0
      IF ((AEADJ.GE.OLDAI).AND.(TWE.GE.AEADJ)) AEADJ=0.0
  100 IF(TWE.GT.ACCMAX) ACCMAX=TWE
CEA      IF (TWE.GE.AEADJ) AEADJ=0.0
      AI=ACCMAX
      IF(ACCMAX.GT.SI)AI=SI
      IF (AEADJ.GT.0.0) AI=AEADJ
      IF(TWE.GE.AI) GO TO 105
      IF(TWE.LE.SB) GO TO 110
      IF(TWE.GE.SBWS) GO TO 115
      AESC=SBAESC+((1.0-SBAESC)*((TWE-SB)/(SBWS-SB)))
      GO TO 120
  110 R=(TWE/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      AESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(AESC.GT.1.0) AESC=1.0
      SB=TWE
CEA      SB=TWE+SNOF
      SBWS=TWE
      SBAESC=AESC
      GO TO 120
  105 SB=TWE
      SBWS=TWE
  115 AESC=1.0
  120 IF(AESC.LT.0.05) AESC=0.05
      IF(AESC.GT.1.0) AESC=1.0
C.......................................
      RETURN
      END