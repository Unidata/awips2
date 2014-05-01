C     Module ADJC31
C
      SUBROUTINE ADJC31(X,X1,CAESC)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A CHANGE
C     IN THE WATER-EQUIVALENT.  
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 FOR THE SNOW-17 OPERATION
C     Modified by...
C        Jay Day RTi, July 1988 for use in the SNOW-43 operation
C.......................................
C     Arguments:
C
C    Argument       I/O         Description
C       X            I          Array containing updated states
C       X1           I          Array containing predicted states
C       CAESC        O          Computed areal extent of snow cover
C.......................................
C
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real     x, x1, caesc
C     --- L O C A L ---
      real     oldwe, oldai
      real     r, fn
      integer  n
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snpm31'
      include 'snow43/snco31'
      include 'snow43/snup31'
C
C----- D I M E N S I O N --------------------------------
      DIMENSION X(5), X1(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/adjc31.f,v $
     . $',                                                             '
     .$Id: adjc31.f,v 1.1 1996/05/07 10:59:07 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C
      OLDWE=X1(1)+X1(3)
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      OLDAI=AI
      WELIQW=X(1)+X(3)
      IF(OLDWE.Gt.(0.8*ACCMAX)) GO TO 110
      IF(WELIQW.GT.ACCMAX) ACCMAX=WELIQW
      GO TO 120
  110 ACCMAX=WELIQW*(ACCMAX/OLDWE)
  120 IF(WELIQW.GE.AEADJ) AEADJ=0.0
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      IF(WELIQW.LT.AI) GO TO 130
C
C   CASE 1 - 100% COVER
C
      SB=WELIQW
      SBWS=WELIQW
      CAESC=1.0
      GO TO 999
  130 IF((OLDWE.LT.OLDAI).AND.(OLDWE.GT.SB)) GO TO 140
C
C   CASE 2 - ADC CURVE
C
  135 SB=WELIQW
      SBWS=WELIQW
      R=(WELIQW/AI)*10.+1.0
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(SBAESC.GT.1.0) SBAESC=1.0
      CAESC=SBAESC
      GO TO 999
C
C   CASE 3/4 - NEW SNOW
C
  140 R=WELIQW/OLDWE
      SB=SB*R
      SBWS=SBWS*R
      R=(SB/AI)*10.+1.0
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(SBAESC.GT.1.0) SBAESC=1.0
      IF(SBWS.LT.(SB+0.75*SNOF)) SBWS=0.75*SNOF+SB
      CAESC=1.0
      IF(WELIQW.GE.SBWS) GO TO 999
      CAESC=(1.-SBAESC)*(WELIQW-SB)/(SBWS-SB)+SBAESC
C
  999 RETURN
      END
C
