C MEMBER IPRDMD
C  (from old member PRDCHECK)
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IPRDMD (ITYPE)
C
C  GIVEN A DATA TYPE, ROUTINE RETURNS MAX DAYS OF DATA FROM
C  DATA TYPE INDEX (DATFIL).
C  WILL GIVE REG OR MAX OF REG AND FUTURE IF SEPARATE TIME SERIES.
C
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/iprdmd.f,v $
     . $',                                                             '
     .$Id: iprdmd.f,v 1.1 1995/09/17 18:45:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20) ITYPE
C
      IPRDMD=0
C
      CALL PFDTYP (ITYPE,IX)
      IF (IX.LE.0) GO TO 10
      IPRDMD=DATFIL(4,IX)
      IF (DATFIL(7,IX).LE.0) GO TO 10
C
      IXX=DATFIL(7,IX)
      IF (DATFIL(4,IXX).GT.DATFIL(4,IX)) IPRDMD=DATFIL(4,IXX)
C
10    IF (IPRTR.GT.0) WRITE (IOGDB,30) IPRDMD
C
      RETURN
C
20    FORMAT (' *** ENTER IPRDMD - ITYPE=',A4)
30    FORMAT (' *** EXIT IPRDMD - IPRDMD=',I3)
C
      END
