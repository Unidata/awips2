C MEMBER WEEK24
C  (from old member FCAPIC24)
C
      SUBROUTINE WEEK24(WK,WKW,WKD,F,IRISE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apicont/RCS/week24.f,v $
     . $',                                                             '
     .$Id: week24.f,v 1.1 1995/09/17 18:58:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C**********************************************************
C   COMPUTES FRACTIONAL DISTANCE THE CURRENT WEEK FALLS
C   BETWEEN THE WET AND DRY WEEKS
C***********************************************************
C
C   WRITTEN BY-- ERIC ANDERSON,  HRL, MARCH 1990
C
C***********************************************************
C
      IRISE=0
      IF ((WK.GE.WKW).AND.(WK.LE.WKD)) GO TO 100
      IF ((WK.GE.WKD).AND.(WK.LE.WKW)) GO TO 105
      IF (WKW.LT.WKD) GO TO 110
      GO TO 120
C
  100 IRISE=1
  105 F=(WK-WKW)/(WKD-WKW)
      GO TO 190
C
  110 IF (WK.LT.WKW) GO TO 115
      F=(52.143-WK+WKW)/(52.143-WKD+WKW)
      GO TO 190
C
  115 F=(WKW-WK)/(52.143-WKD+WKW)
      GO TO 190
C
  120 IRISE=1
      IF (WK.GT.WKW) GO TO 125
      F=(WK+52.143-WKW)/(52.143-WKW+WKD)
      GO TO 190
C
  125 F=(WK-WKW)/(52.143-WKW+WKD)
C
  190 IF (F.LT.0.0) F=0.0
      IF (F.GT.1.0) F=1.0
C
      RETURN
      END
