C MEMBER HWPROC
C  (from old member HCLEXECP)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HWPROC (ICARD,IFIRST,ISTAT)
C
C  THIS ROUTINE WRITES THE PROC CARDS TO THE TEMP FILE
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hprocc'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hsvcrd'
C
      DIMENSION ICARD(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hwproc.f,v $
     . $',                                                             '
     .$Id: hwproc.f,v 1.2 1996/01/17 21:48:18 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IFIRST.NE.1) GO TO 10
C
C  PUT FIRST CARD GOES TO COMMON BLOCK
      CALL UNPAKS (ICARD,ICBUF,18,80,ISTAT)
      ICSAV=0
      IFIRST=0
      GO TO 30
C
C  WRITE CARD TO FILE
10    WRITE (KPROCC,20) ICARD
20    FORMAT (20A4)
C
30    IF (IHCLDB.GT.0) WRITE (IOGDB,40) ICARD
40    FORMAT (' IN HWPROC - ICARD=',20A4)
C
      RETURN
C
      END
