C MEMBER PFFTYP
C  (from old member PRDCHECK)
C-----------------------------------------------------------------------
C
      SUBROUTINE PFFTYP (ITYPE,IFTYPE,IXREG,IXFUT)
C
C  THIS ROUTINE TAKES A DATA TYPE AND RETURNS THE FUTURE DATA TYPE AND
C  THE INDEX IN DATFIL FOR THE REGULAR AND FUTURE TYPES.
C
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'urcommon/urftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pfftyp.f,v $
     . $',                                                             '
     .$Id: pfftyp.f,v 1.1 1995/09/17 18:45:37 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,30) ITYPE
C
      IFTYPE=IBLNK
      IXFUT=0
C
      CALL PFDTYP (ITYPE,IXREG)
      IF (IXREG.LE.0) GO TO 20
C
      IF (IAMORD.EQ.1) GO TO 10
C
      IF (DATFIL(7,IXREG).LE.0) GO TO 20
      IXFUT=DATFIL(7,IXREG)
      IFTYPE=DATFIL(1,IXFUT)
      GO TO 20
C
10    IF (IDATFL(7,IXREG).LE.0) GO TO 20
      IXFUT=IDATFL(7,IXREG)
      IFTYPE=IDATFL(1,IXFUT)
C
20    IF (IPRTR.GT.0) WRITE (IOGDB,40) IFTYPE,IXREG,IXFUT
C
      RETURN
C
30    FORMAT (' *** ENTER PFFTYP - ITYPE=',A4)
40    FORMAT (' *** EXIT PFFTYP - IFTYPE=',A4,3X,
     *   'IXREG=',I2,3X,'IXFUT=',I2)
      END
