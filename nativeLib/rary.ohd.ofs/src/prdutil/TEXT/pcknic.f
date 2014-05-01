C MEMBER PCKNIC
C  (from old member PRDFUNC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/22/95.08:22:39 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PCKNIC (IX,IXF,NEWTOT)
C
C  COMPUTE THE TOTAL NUMBER OF WORDS NEEDED FOR THE INCORE BUFFER
C  AND SEE IF ROOM AVAILABLE.
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/pdatas'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/pcknic.f,v $
     . $',                                                             '
     .$Id: pcknic.f,v 1.1 1995/09/17 19:16:31 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      ITOT=0
C
C  CHECK NUMBER OF DATA TYPES
      IF (NUMDTP.EQ.0) GO TO 20
C
      DO 10 I=1,NUMDTP
         IF (I.EQ.IX.OR.I.EQ.IXF) GO TO 10
            NVAL=DATFIL(13,I)
            IF (NVAL.LE.0) NVAL=1
            IWDS=DATFIL(4,I)*(24/DATFIL(5,I))*NVAL+LENHED+DATFIL(14,I)
            ITOT=ITOT+IWDS*DATFIL(3,I)
10       CONTINUE
C
20    IF (ITOT+NEWTOT.LE.2500) GO TO 30
      IF (NEWTOT.LE.LENHED) GO TO 30
      WRITE (LP,40)
C
30    IF (IPRTR.GT.0) WRITE (IOGDB,50) ITOT,NEWTOT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('0**WARNING** INCORE BUFFER MAY NOT HOLD ADDITIONAL ',
     *   'TIME SERIES.')
50    FORMAT (' *** EXIT PCKNIC : ITOT=',I6,' NEWTOT=',I6)
C
      END
