C     Module CASE31
C
      SUBROUTINE CASE31
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE DETERMINES THE POSITION ON THE AREAL DEPLETION
C     CURVE FOR THE CURRENT TIME PERIOD IN THE 'SNOW-43 ' OPERATION.  
C     (SEE NWS-43, PP 38, 39)
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        JAY DAY - RTi July 1988 for use in the SNOW-43 operation.
C.......................................
C     No Arguments:
C.......................................
C
C
C
      implicit none
      integer nadc
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snpm31'
      include 'snow43/snco31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/case31.f,v $
     . $',                                                             '
     .$Id: case31.f,v 1.1 1996/05/07 11:00:13 page Exp $
     . $' /
C    ===================================================================
C
C
C........................................
C
C
      NADC=0
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      IF(WELIQW.LT.AI) GO TO 100
C
C   100 PERCENT COVER
C
      ICASE=1
      GO TO 999
  100 IF(WELIQW.GT.SB.AND.SBWS.GT.SB) GO TO 200
C
C   ADC CURVE
C
      ICASE=2
      NADC=WELIQW/AI*10.+1.
      GO TO 999
  200 IF(WELIQW.LT.SBWS) GO TO 300
C
C   NEW SNOW - 100 PERCENT COVER
C
      ICASE=3
      GO TO 999
C
C   NEW SNOW LINE
C
  300 ICASE=4
  999 RETURN
      END
