C MEMBER PGETTS
C  (from old member PRDRWTS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/10/95.14:27:15 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PGETTS (ITSID,ITYPE,LWKBUF,IWKBUF,IREC,ISTAT)
C
C          ROUTINE:  PGETTS
C
C   MODIFIED 5/20/87-GFS-HRL-ADD IXBUF TO PSERCH CALL
C             VERSION:  1.0.0
C
C                DATE:  2-10-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE FINDS A TIME SERIES AND READS IT
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITSID      A8    I     2    TIME SERIES ID
C
C       ITYPE      A4    I     1    DATA TYPE CODE
C
C       LWKBUF        I     I     1    DIMENSION OF IWKBUF
C
C       IWKBUF     I    I/O    ?    WORK ARRAY
C
C       IREC       I     O     1    RECORD NUMBER OF TS
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                   0=NORMAL RETURN
C                                   1=DATA TYPE NOT FOUND
C                                   2=TIME SERIES NOT FOUND
C                                   3=SYSTEM OR DAIO ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urftbl'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION ITSID(2),IXBUF(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pgetts.f,v $
     . $',                                                             '
     .$Id: pgetts.f,v 1.1 1995/09/17 18:45:39 dws Exp $
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
      IF (IPRTR.GT.0) WRITE (IOGDB,50) ITSID,ITYPE,IAMORD
C
      IREC=0
      ISTAT=0
C
C  CHECK DATA TYPE
      CALL PFDTYP (ITYPE,INDX)
      IF (INDX.NE.0) GO TO 10
         IF (IPRDB.GT.0) WRITE (IOGDB,60) ITYPE
         ISTAT=1
         GO TO 40
C
C  GET UNIT NUMBER
10    IF (IAMORD.NE.1) IUNIT=DATFIL(2,INDX)
      IF (IAMORD.EQ.1) IUNIT=IDATFL(2,INDX)-KUPRDO
C
C  FIND THE TIME SERIES
      CALL PSERCH (ITSID,ITYPE,IFREE,IXREC,IXBUF)
      IF (IXREC.NE.0) GO TO 20
      IF (IAMORD.EQ.1) GO TO 40
         IF (IPRDB.GT.0) WRITE (IOGDB,70) ITSID,ITYPE
         ISTAT=2
         GO TO 40
C
C  READ TIME SERIES
20    IREC=IXBUF(4)
      CALL RTSRCD (IREC,ITSID,ITYPE,IUNIT,LWKBUF,IWKBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 30
      CALL UNAMCP (ITSID,IWKBUF(8),ISTAT)
      IF (ISTAT.EQ.0) GO TO 40
C
30    IF (IPRDB.GT.0)
     *   WRITE (LP,80) ITSID,ITYPE,IUNIT,IREC,LWKBUF,ISTAT
      ISTAT=3
C
40    IF (IPRTR.GT.0) WRITE (IOGDB,90) ISTAT
C
      RETURN
C
50    FORMAT (' *** ENTER PGETTS - ITSID=',2A4,3X,'ITYPE=',A4,3X,
     *   'IAMORD=',I2)
60    FORMAT ('0*** ERROR - IN PGETTS - DATA TYPE ',A4,' NOT FOUND.')
70    FORMAT ('0*** ERROR - IN PGETTS - TIME SERIES ID ',2A4,
     *   ' FOR DATA TYPE ',A4,' NOT FOUND.')
80    FORMAT ('0*** ERROR - IN PGETTS - ERROR CALLING RTSRCD. ',
     *   'ITSID=',2A4,3X,'ITYPE=',A4,3X,'IUNIT=',I3,3X,'IREC=',I6,3X,
     *   'LWKBUF=',I6,3X,'ISTAT=',I2)
90    FORMAT (' *** EXIT PGETTS - ISTAT=',I2)
C
      END
