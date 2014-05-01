C MODULE IPDCKR
C-----------------------------------------------------------------------
C
      FUNCTION IPDCKR (ITYPE)
C
C  ROUTINE TO CHECK FOR A VALID RRS TYPE.
C
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdtrrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/ipdckr.f,v $
     . $',                                                             '
     .$Id: ipdckr.f,v 1.2 2002/02/11 19:53:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      IPDCKR=0
C
      DO 10 I=1,NTYPE
         IF (ITYPE.EQ.IRTYPE(I)) THEN
            IPDCKR=I
            GO TO 30
            ENDIF
10       CONTINUE
C
30    IF (IPDTR.GT.0) WRITE (IOGDB,40) ITYPE,IPDCKR
40    FORMAT (' EXIT IPDCKR - ITYPE=',A4,' IPDCKR=',I2)
C
      RETURN
C
      END
