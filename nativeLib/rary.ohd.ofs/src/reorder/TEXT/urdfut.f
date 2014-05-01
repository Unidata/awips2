C MODULE URDFUT
C-----------------------------------------------------------------------
C
      SUBROUTINE URDFUT (IREC,IUNIT,LTSHDR,LTSREC,ITSREC,LWORK,IWORK,
     *   ITYPE,NUMTS,IFUT,ISTAT)
C
C  THIS ROUTINE READS FUTURE TIME SERIES RECORDS FOR THOSE TYPES
C  THAT HAVE FUTURE RECORDS. AFTER IT OBTAINS THE TIME SERIES IT
C  CALLS PGETTS TO SEE IF THE RECORD ALREADY EXIST ON THE NEW FILES.
C  THE RECORD NUMBER OR THE AVAILABLE SLOT IS RETURNED.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       ------   ----   ---   ---   -----------
C       IREC       I    I      1    RECORD NUMBER OF FUTURE RECORD
C       IUNIT      I    I      1    LOGICAL UNIT OF FUTURE RECORDS
C       LTSHDR     I    O      1    RECORD NUMBER OF FUTURE IF FOUND
C       LTSREC     I    I      1    LENGTH OF ARRAY ITSREC
C       ITSREC     I    I/O  LTSREC FUTURE TIME SERIES WORK ARRAY
C       LWORK      I    I      1    LENGTH OF ARRAY IWORK
C       IWORK      I    I/O  LWORK  WORK ARRAY
C       ITYPE      I    I      1    DATA TYPE
C       ISTAT      I    O      1    STATUS CODE:
C                                     0=NORMAL RETURN
C                                     OTHER=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'ucommon/uordrx'
C
      DIMENSION IWORK(LWORK),ITSREC(LTSREC)
      DIMENSION ITSID(2),ITBUF(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urdfut.f,v $
     . $',                                                             '
     .$Id: urdfut.f,v 1.2 1999/01/20 15:03:19 page Exp $
     . $' /
C    ===================================================================

C
C
      IF (IPRDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,10)
         ENDIF
C
      ISTAT=0
C
C  GET TIMES SERIES RECORD
      IAMORD=0
      CALL UMEMST (IBLNK,ITSID,2)
      IXTYPE=ITYPE
      CALL RTSRCD (IREC,ITSID,IXTYPE,IUNIT,LTSREC,ITSREC,ISTAT)
      IF (ISTAT.GT.0) GO TO 20
C
C  SET IDENTIFIER
      CALL UMEMOV (ITSREC(8),ITSID,2)
C
C  SET FUTURE DATA TYPE
      IXTYPE=ITSREC(10)
C
C  SEE IF THIS FUTURE RECORD EXISTS IN THE NEW FILE
      IAMORD=1
      CALL PGETTS (ITSID,IXTYPE,LWORK,IWORK,LTSHDR,ISTAT)
      IAMORD=0
      IF (ISTAT.NE.0) GO TO 20
      ISTAT=0
C
C  CHECK IF IN NEW FILE
      IF (LTSHDR.NE.0) GO TO 70
C
C  WRITE TIME SERIES TO NEW FILE
      CALL URWRTS (ITSID,IXTYPE,LTSREC,ITSREC,LTSHDR,LWORK,IWORK,
     *   NUMTS,IFUT,ISTAT)
      IF (ISTAT.EQ.0) GO TO 70
      GO TO 40
C
20    WRITE (LP,30) IXTYPE,ITSID
      CALL SUERRS (LP,2,-1)
30    FORMAT ('0*** ERROR - IN URDFUT - READING ',A4,' TIME SERIES ',
     *   'RECORD FOR IDENTIFIER ',2A4,'.')
      GO TO 60
C
40    WRITE (LP,50) IXTYPE,ITSID
      CALL SUERRS (LP,2,-1)
50    FORMAT ('0*** ERROR - IN URDFUT - WRITING ',A4,' TIME SERIES ',
     *   'RECORD FOR IDENTIFIER ',2A4,'.')
C
60    IWURFL=1
C
70    IF (IPRDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,80)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT(' *** ENTER URDFUT')
80    FORMAT(' *** EXIT URDFUT')
C
      END
