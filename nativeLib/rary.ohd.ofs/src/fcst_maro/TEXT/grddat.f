C MEMBER GRDDAT
C  (from old member PPGRDDAT)
C
      SUBROUTINE GRDDAT(MONTH, NDATE, NYEAR, IHRS, MAX)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/grddat.f,v $
     . $',                                                             '
     .$Id: grddat.f,v 1.1 1995/09/17 19:02:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C.....THIS SUBROUTINE RE-SETS THE MONTH, DATE, AND YEAR FIELDS WHEN
C.....THE HOURS FIELD IS 24 OR GREATER.
C
C.....THE ARGUMENT LIST IS:
C
C.....MONTH  - THE MONTH NUMBER.
C.....NDATE  - THE DATE.
C.....NYEAR  - THE YEAR.
C.....IHRS   - THE HOURS (24 HOUR CLOCK).
C.....MAX    - THE MAXIMUM NUMBER OF DAYS IN THE MONTH.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN     WGRFC, FT. WORTH, TEXAS     APRIL 8, 1988
C
      IHRS = IHRS - 24
      NDATE = NDATE + 1
C
      IF(NDATE .LE. MAX) GOTO 999
      IF(NDATE .NE.   2) GOTO 100
C
      NP = (NYEAR/4)*4
      IF(NP .EQ. NYEAR) MAX = 29
      NP = (NYEAR/100)*100
      IF(NP .EQ. NYEAR) MAX = 28
      NP = (NYEAR/400)*400
      IF(NP .EQ. NYEAR) MAX = 29
      IF(NDATE .LE. MAX) GOTO 999
C
  100 NDATE = 1
      MONTH = MONTH + 1
      IF(MONTH .LE. 12) GO TO 999
C
      MONTH = 1
      NYEAR = NYEAR + 1
C
  999 RETURN
      END
