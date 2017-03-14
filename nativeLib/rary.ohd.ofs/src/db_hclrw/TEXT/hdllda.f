C MEMBER HDLLDA
C  (from old member HCLDELLD)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDLLDA (ITYPE,ISTAT)
C
C          ROUTINE:  HDLLDA
C
C             VERSION:  1.0.0
C
C                DATE:  8-9-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE DELETES ALL THE LOCAL DEFAULT RECORDS FOR
C    A SPECIFIED RECORD TYPE (PROC,FUNC,TECH) .
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITYPE      I     I     1    TYPE OF DEFINITION
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     1=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hunits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IXBUF(4),IBUF(16),IDBUF(16),LDEL(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdllda.f,v $
     . $',                                                             '
     .$Id: hdllda.f,v 1.1 1995/09/17 18:42:04 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LDEL/4HDELE,4HTED /
C
C***********************************************************************
C
C
      ISTAT=0
      IADD1=0
      IF (ITYPE.EQ.3) IADD1=1
C
C        SET POINTERS TO INDEX
C
      IS=HINDEX(2,ITYPE)
      IE=HINDEX(3,ITYPE)
      CALL UMEMST(0,IDBUF,16)
      DO 20 I=IS,IE
C
C READ EACH INDEX RECORD OF TYPE
C
          CALL UREADT (KINDXG,I,IXBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 30
          CALL UNAMCP(IXBUF(1),LDEL,ISTA)
          IF (ISTA.EQ.0) GO TO 20
          IREC=IXBUF(3)
C
C READ THE DEFINITION RECORD
C
          CALL UREADT (KDEFNG,IREC,IBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 30
          IDFREC=IBUF(7+IADD1)
C
C WRITE A ZEROED OUT RECORD TO THE LOCAL DEFAULT FILE
C
          CALL UWRITT(KLDFGD,IDFREC,IDBUF,ISTAT)
          IF (IHCLDB.EQ.3) WRITE (IOGDB,10) IDFREC
10        FORMAT (' DELETED DEFAULT RECORD # ',I6)
          IF (ISTAT.NE.0) GO TO 30
20    CONTINUE
      GO TO 50
30    CONTINUE
C
C        SYSTEM ERROR
C
      WRITE (LP,40)
40    FORMAT ('0**ERROR** SYSTEM ERROR')
      ISTAT=1
50    CONTINUE
C
      RETURN
C
      END
