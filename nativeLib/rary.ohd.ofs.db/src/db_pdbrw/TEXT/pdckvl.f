C MODULE PDCKVL
C-----------------------------------------------------------------------
C
      SUBROUTINE PDCKVL (STAID,IPOS,DTYPE,VALUE,ISTAT)
C
C  THIS ROUTINE CHECKS A DATA VALUE TO SEE IF IT IS WITHIN THE VALID
C  RANGE FOR THE DATA TYPE.
C
C  ARGUMENT LIST:
C
C       NAME    TYPE  I/O   DIM   DESCRIPTION
C       ------  ----  ---   ---   -----------
C       STAID    A*8   I     1    STATION IDENTIFIER
C       IPOS     I*4   I     1    POSITION IN DATA ARRAY
C       DTYPE    A*4   I     1    DATA TYPE
C       VALUE    R*4   I     1    DATA VALUE
C       ISTAT    I*4   O     1    STATUS CODE:
C                                   0=OK
C                                   30=DATA VALUE OUT OF RANGE OR
C                                      TYPE NOT FOUND
C
      CHARACTER*8 STAID
      CHARACTER*4 DTYPE,CTYPE
C
      PARAMETER (MCKVAL=12)
      DIMENSION CKVAL(3,MCKVAL)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdckvl.f,v $
     . $',                                                             '
     .$Id: pdckvl.f,v 1.2 2001/06/13 12:58:31 dws Exp $
     . $' /
C    ===================================================================
C
      DATA CKVAL
     *  /4hPP  ,   0.0,  62.5,
     *   4hTA  ,-150.0, 150.0,
     *   4hTM  ,-150.0, 150.0,
     *   4hTD  ,-150.0, 100.0,
     *   4hUS  ,   0.0, 250.0,
     *   4hRC  ,   0.0,   1.0,
     *   4hRP  ,   0.0, 100.0,
     *   4hRI  ,   0.0,1200.0,
     *   4hMD  , -36.0,  36.0,
     *   4hTF  ,-150.0,1500.0,
     *   4hTX  ,-150.0, 150.0,
     *   4hTN  ,-150.0, 150.0/
C
C
      IF (IPDDB.EQ.1) WRITE (IOGDB,*) 'ENTER PDCKVL'
C
      ISTAT=0
C
      IF (VALUE.LE.-999.0) GO TO 40
C
C  CHECK FIRST 2 CHARACTERS OF DATA TYPE
      CTYPE=' '
      CALL UMOVEX (DTYPE,1,CTYPE,1,2)
      DO 10 ICKVAL=1,MCKVAL
         CALL UCMPAR (CTYPE,CKVAL(1,ICKVAL),1,IMATCH)
         IF (IMATCH.EQ.0) GO TO 20
10       CONTINUE
      CALL UWARN (LP,0,-1)
      WRITE (LP,15) DTYPE,STAID,IPOS
15    FORMAT ('0**WARNING** IN PDCKVL - DATA TYPE ',A,
     *   ' FOR STATION ',A,
     *   ' IS INVALID.',
     *   ' POSITION IN DATA ARRAY IS ',I3,'.')
      ISTAT=30
      GO TO 40
C
C  CHECK RANGE
20    IF (VALUE.LT.CKVAL(2,ICKVAL).OR.VALUE.GT.CKVAL(3,ICKVAL)) THEN
         CALL UWARN (LP,0,-1)
         WRITE (LP,35) VALUE,STAID,DTYPE,
     *      CKVAL(2,ICKVAL),CKVAL(3,ICKVAL),
     *      IPOS
35    FORMAT ('0**WARNING** IN PDCKVL - DATA VALUE (',F12.3,')',
     *     ' FOR STATION ',A,' AND DATA TYPE ',A /
     * 12X,' IS NOT IN THE RANGE OF ',F6.1,' TO ',F6.1,
     *     '. POSITION IN DATA ARRAY IS ',I3,'.')
         ISTAT=30
         ENDIF
C
40    IF (IPDDB.EQ.1) WRITE (IOGDB,*) 'EXIT PDCKVL - ISTAT=',ISTAT
C
      RETURN
C
      END
