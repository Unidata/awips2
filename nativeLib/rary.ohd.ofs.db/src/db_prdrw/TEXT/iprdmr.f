C MEMBER IPRDMR
C  (from old member PRDCHECK)
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IPRDMR (ITYPE)
C
C  GIVEN A DATA TYPE, ROUTINE RETURNS MAX DAYS OF DATA FROM
C  DATA TYPE INDEX (DATFIL) FOR REGULAR OF TYPE.
C  IF MIXED TYPE, RETURNS -1.
C
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/iprdmr.f,v $
     . $',                                                             '
     .$Id: iprdmr.f,v 1.1 1995/09/17 18:45:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,30) ITYPE
C
      IPRDMR=0
C
      CALL PFDTYP (ITYPE,IX)
      IF (IX.LE.0) GO TO 20
C
      IF (DATFIL(7,IX).LE.0) GO TO 10
      IPRDMR=DATFIL(4,IX)
      GO TO 20
C
10    IPRDMR=-1
C
20    IF (IPRTR.GT.0) WRITE (IOGDB,40) IPRDMR
C
      RETURN
C
30    FORMAT (' *** ENTER IPRDMR - ITYPE=',A4)
40    FORMAT (' *** EXIT IPRDMR - IPRDMR=',I3)
C
      END
