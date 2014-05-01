C MEMBER PPCKID
C  (from old member PPPCHECK)
C-----------------------------------------------------------------------
C
      SUBROUTINE PPCKID (ID,ISTAT)
C
C  CHECK IDENTIFIER TO SEE IF IT IS ONE OF THE RESERVED WORDS.
C
      INCLUDE 'udebug'
C
      DIMENSION ID(2),IRWORD(2,5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/ppckid.f,v $
     . $',                                                             '
     .$Id: ppckid.f,v 1.1 1995/09/17 18:45:07 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NRWRDS/1/
      DATA IRWORD/4hEND ,4h    ,8*4h    /
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,10) ID
10    FORMAT (' *** ENTER PPCKID : ID=',2A4)
C
      DO 20 I=1,NRWRDS
         IF (ID(1).EQ.IRWORD(1,I).AND.ID(2).EQ.IRWORD(2,I)) GO TO 30
20       CONTINUE
      ISTAT=0
      GO TO 40
C
C  MATCH FOUND
30    ISTAT=3
C
40    IF (IPPTR.GT.0) WRITE (IOGDB,50) ISTAT
50    FORMAT (' *** EXIT PPCKID : ISTAT=',I2)
C
      RETURN
C
      END
