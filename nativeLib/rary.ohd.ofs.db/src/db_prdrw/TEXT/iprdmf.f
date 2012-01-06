C MEMBER IPRDMF
C  (from old member PRDCHECK)
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IPRDMF (ITYPE)
C
C  GIVEN A DATA TYPE, ROUTINE RETURNS MAX DAYS OF DATA FROM
C  DATA TYPE INDEX (DATFIL) FOR FUTURE OF TYPE.
C  IF NOT A FUTURE TYPE, RETURNS -1.
C
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/iprdmf.f,v $
     . $',                                                             '
     .$Id: iprdmf.f,v 1.1 1995/09/17 18:45:28 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,30) ITYPE
C
      IPRDMF=0
C
      CALL PFDTYP (ITYPE,IX)
      IF (IX.LE.0) GO TO 20
C
      IF (DATFIL(7,IX).LE.0) GO TO 10
      IXX=DATFIL(7,IX)
      IPRDMF=DATFIL(4,IXX)
      GO TO 20
C
10    IPRDMF=-1
C
20    IF (IPRTR.GT.0) WRITE (IOGDB,40) IPRDMF
C
      RETURN
C
30    FORMAT (' *** ENTER IPRDMF - ITYPE=',A4)
40    FORMAT (' *** EXIT IPRDMF - IPRDMF=',I3)
C
      END
