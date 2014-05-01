C MEMBER HGTDIN
C  (from old member HCLIOG1)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/29/95.14:27:34 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGTDIN (INTNUM,ITYPE,IRECX,IRECD,IDBUF,MAXD,ISTAT)
C
C   ROUTINE TO GET A DEFINITION FROM THE INTERNAL RECORD NUMBER
C   BY GOING TO THE INDEX AND THEN TO THE DEFINITION.  IT CAN
C   BE USED FOR TECHNIQUES AND PROCS BUT NOT FOR FUNCTIONS SINCE
C   THEIR INTERNAL NUMBERS A ASSIGNED AS 1-999 INSTEAD OF + AND -.
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C      INTNUM       I     I     1    INTERNAL DEFINITION NUMBER
C                                     +=LOCAL, -=GLOBAL
C      ITYPE        I     I     1    TYPE OF DEF, 1=PROC,3=TECH ALSO +-
C      IRECX        I     O     1    COMPUTED INDEX RECORD
C      IRECD        I     O     1    COMPUTED DEFINITION RECORD NUMBER
C      IDBUF        I     O   MAXD   ARRAY TO RECEIVE THE DEF
C      MAXD         I     I     1    SIZE OF IDBUF
C      ISTAT        I     O     1    STATUS 0=OK, ELSE ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hindx'
C
      DIMENSION IXBUF(4),IDBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgtdin.f,v $
     . $',                                                             '
     .$Id: hgtdin.f,v 1.1 1995/09/17 18:42:26 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET UP THE FILE AND WHICH PART OF INDEX ACCORDING TO TYPE
C +=LOCAL(WORDS 1-4), -=GLOBAL WORDS 5-8
C
      IF (INTNUM.LT.0) GO TO 10
         IUNITD=KDEFNL
         IX=ITYPE
         IUNITX=KINDXL
         GO TO 20
C
C  GLOBAL
10    IUNITX=KINDXG
      IUNITD=KDEFNG
      IX=ITYPE+4
C
C  COMPUTE INDEX RECORD NUMBER
20    IRECX=IABS(INTNUM)+HINDEX(2,IX)-1
      CALL UREADT (IUNITX,IRECX,IXBUF,ISTAT)
      IF (ISTAT.EQ.0) GO TO 30
      WRITE (LP,60) IRECX,IUNITX
      GO TO 40
C
30    IF (IHCLDB.GT.1) WRITE (IOGDB,50) IXBUF
C
C  GET THE DEFINITION
      IRECD=IXBUF(3)
      CALL HGTRDN (IUNITD,IRECD,IDBUF,MAXD,ISTAT)
      IF (ISTAT.NE.0) WRITE (IOGDB,60) IRECD,IUNITD
C
40    IF (IHCLDB.GT.1) WRITE (IOGDB,70) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' IN HGTDIN - XBUF= ',2A4,2I6)
60    FORMAT ('0**ERROR** IN HGTDIN - READING RECORD ',I5,
     *   ' FROM UNIT ',I2,'.')
70    FORMAT (' EXIT HGTDIN - ISTAT=',I2)
C
      END
