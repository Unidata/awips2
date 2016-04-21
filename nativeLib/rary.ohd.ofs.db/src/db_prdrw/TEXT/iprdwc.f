C MEMBER IPRDWC
C  (from old member PRDCHECK)
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IPRDWC (ITYPE)
C
C  THIS FUNCTION DETERMINES THE ICALL KEY FOR WHO CAN WRITE A TIME
C  SERIES OF A CERTAIN DATA TYPE.
C        0=PREPROCESSOR
C        1=FORECAST COMPONENT
C  THE VALUE IS TAKEN FROM DATFIL(11,X) WHERE X IS THE INDEX OF THE DATA
C  TYPE. IF DATA TYPE NOT FOUND, -1 IS RETURNED.
C
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/iprdwc.f,v $
     . $',                                                             '
     .$Id: iprdwc.f,v 1.1 1995/09/17 18:45:30 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20) ITYPE
C
      IPRDWC=-1
C
      CALL PFDTYP (ITYPE,INDXD)
      IF (INDXD.EQ.0) GO TO 10
      IPRDWC=DATFIL(11,INDXD)
C
10    IF (IPRTR.GT.0) WRITE (IOGDB,30) INDXD
C
      RETURN
C
20    FORMAT (' *** ENTER IPRDWC - ITYPE=',A4)
30    FORMAT (' *** EXIT IPRDWC - IPRDWC=',I3)
C
      END
