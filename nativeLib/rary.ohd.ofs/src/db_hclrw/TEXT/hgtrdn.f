C MEMBER HGTRDN
C  (from old member HCLIOG1)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/29/95.14:27:34 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGTRDN (IUNIT,IREC,IBUF,LBUF,ISTAT)
C
C
C          ROUTINE:  HGTRDN
C
C             VERSION:  1.0.0
C
C                DATE:  8-19-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO READ A VARIABLE LENGTH RECORD
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IUNIT       I    I     1     LOGICAL UNIT OF FILE
C       IREC        I    I     1     FIRST RECORD NUMBER
C       IBUF        I    O    LBUF    DESTINATION ARRAY
C       LBUF        I    I     1     DIMENSION OF IBUF
C       ISTAT       I    O     1     STATUS INDICATOR
C                                      0=NORMAL RETURN
C                                      1=ARRAY TOO SMALL
C                                      2=DAIO ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgtrdn.f,v $
     . $',                                                             '
     .$Id: hgtrdn.f,v 1.1 1995/09/17 18:42:29 dws Exp $
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
C  CHECK SIZE OF ARRAY
      LNEED=LRECLH
      IF (LBUF.LT.LNEED) GO TO 20
C
C  READ FIRST RECORD
      CALL UREADT (IUNIT,IREC,IBUF,IERR)
      IF (IERR.NE.0) THEN
         IRECP=IREC
         GO TO 30
         ENDIF
C
      IPOS=LRECLH+1
      NREC=IBUF(1)-1
      IF (NREC.LE.0) GO TO 40
C
C  CHECK SIZE OF ARRAY
      LNEED=LRECLH*(NREC+1)
      IF (LBUF.LT.LNEED) GO TO 20
C
C  READ REST OF RECORDS
      IRECN=IREC+1
      DO 10 I=1,NREC
         CALL UREADT (IUNIT,IRECN,IBUF(IPOS),IERR)
         IF (IERR.NE.0) THEN
            IRECP=IREC
            GO TO 30
            ENDIF
         IRECN=IRECN+1
         IPOS=IPOS+LRECLH
10       CONTINUE
      GO TO 40
C
20    WRITE (LP,50) LBUF,LNEED
      ISTAT=1
      GO TO 40
C
30    WRITE (LP,60) IRECP,IUNIT
      ISTAT=2
C
40    IF (IHCLDB.GT.1) WRITE (IOGDB,70) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('0**ERROR** IN HGTRDN - SIZE OF WORK ARRAY (',I5,
     *   ') TOO SMALL. ',I5,' WORDS NEEDED.')
60    FORMAT ('0**ERROR** IN HGTRDN - READING RECORD ',I5,
     *   ' FROM UNIT ',I2,'.')
70    FORMAT (' EXIT HGTRDN - ISTAT=',I2)
C
      END
