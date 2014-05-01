C MODULE HRLDRR
C-----------------------------------------------------------------------
C
      SUBROUTINE HRLDRR (ISTRT,KBUF,LKBUF,ISTAT)
C
C          ROUTINE:  HRLDRR
C             VERSION:  1.0.
C                DATE:  3-17-82
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE READS A LOCAL DEFINITION REFERENCE RECORD.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISTRT      I     I     1    FIRST RECORD NUMBER
C       KBUF       I     O     ?    ARRAY TO RECEIVE RECORD
C       LKBUF      I     I     1    DIMENSION OF KBUF
C       ISTAT      I     O     1    STATUS INDICATOR
C                                      0=NORMAL RETURN
C                                      1=INVALID RECORD NUMBER
C                                      2=SYSTEM ERROR
C                                      3=ARRAY TOO SMALL
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hdatas'
      INCLUDE 'hclcommon/hindx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER KBUF(LKBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hrldrr.f,v $
     . $',                                                             '
     .$Id: hrldrr.f,v 1.2 1998/04/07 13:01:08 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      ISTAT=0
C
C  CHECK FIRST RECORD NUMBER
      IF (ISTRT.GE.HINDEX(2,8).AND.ISTRT.LE.HINDEX(1,8)) GO TO 20
         WRITE (LP,10)
10    FORMAT (' **ERROR** LOCAL DEFINITION RECORD OUT OF RANGE')
         ISTAT=1
         GO TO 100
C
C  READ THE FIRST RECORD
20    IF (LKBUF.LT.LRECLI) THEN
         WRITE (LP,60) LKBUF,LRECLI
60    FORMAT ('0**ERROR** SIZE OF ARRAY FOR LOCAL DEFINITION RECORD (',
     *   I4,') TOO SMALL. NUMBER OF WORDS NEEDED IS ',I4,'.')
         ISTAT=3
         GO TO 100
         ENDIF
      CALL UREADT (KINDXG,ISTRT,KBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 30
      NLREC=KBUF(1)-1
      NWDS=KBUF(1)*LRECLI
      IF (LKBUF.LT.NWDS) THEN
         WRITE (LP,60) LKBUF,NWDS
         ISTAT=3
         GO TO 100
         ENDIF         
      IPOS=LRECLI+1
      IREC=ISTRT+1
      CALL RVLRCD (KINDXG,IREC,NLREC,KBUF(IPOS),LRECLI,ISTAT)
      IF (ISTAT.NE.0) GO TO 30
      GO TO 70
C
C  SYSTEM OR DAIO ERROR
30    WRITE (LP,40)
40    FORMAT ('0**ERROR** SYSTEM OR DAIO ERROR IN HRLDRR')
      ISTAT=2
      GO TO 100
C
70    IF (IHCLDB.EQ.3) WRITE (IOGDB,80) (KBUF(I),I=1,7)
80    FORMAT (' HRLDRR EXECUTED RECORD=',2I3,2(2X,2A4),I3)
      NUM=KBUF(7)+7
      IF (NUM.GT.7) THEN
         IF (IHCLDB.EQ.3) WRITE (IOGDB,90) (KBUF(I),I=8,NUM)
         ENDIF
90    FORMAT (10I7)
C
100   RETURN
C
      END
