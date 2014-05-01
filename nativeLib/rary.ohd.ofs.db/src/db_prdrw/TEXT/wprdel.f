C MODULE WPRDEL
C-----------------------------------------------------------------------
C
      SUBROUTINE WPRDEL (ITSID,IXTYPE,IFUT,ICKREF,IPRERR,ISTAT)
C
C  THIS ROUTINE DELETES A TIME SERIES FROM THE PROCESSED DATA BASE.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       -----    ----   ---   ---   -----------
C       ITSID     A8     I     2    TIME SERIES ID
C       IXTYPE    A4     I     1    DATA TYPE
C       IFUT      I      I     1    REGULAR/FUTURE INDICATOR:
C                                     0=REGULAR
C                                     1=FUTURE
C       ICKREF    I      I     1    CHECK IF REFERENCED INDICATOR:
C                                     0=NO
C                                     1=YES
C       IPRERR    I      I     1    ERROR MESSAGE PRINT INDICATOR:
C                                     0=DO NOT PRINT
C                                     1=PRINT
C       ISTAT     I      O     1    STATUS:
C                                     0=TIME SERIES DELETED
C                                     1=NOT FOUND IN FILE
C                                     2=STILL REFERENCED
C                                     3=INVALID DATA TYPE
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pdatas'
C
      CHARACTER*4 IXTYPE,ITYPE,IFTYPE
      CHARACTER*8 ITSID
      CHARACTER*8 IDELET/'DELETED'/
      DIMENSION IXBUF(4)
      DIMENSION IWKBUF(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/wprdel.f,v $
     . $',                                                             '
     .$Id: wprdel.f,v 1.2 1999/04/23 19:45:58 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,90)
C
      IF (IFUT.EQ.1) THEN
C     CHECK IF VALID FUTURE TYPE
         IFTYPE=IXTYPE
         CALL PFFTYP (IFTYPE,ITYPE,INDEXR,INDEXD)
         IF (INDEXD.EQ.0) THEN
            IF (IPRERR.EQ.1) WRITE (LP,95) IFTYPE
            ISTAT=3
            GO TO 80
            ENDIF
         ELSE
            ITYPE=IXTYPE
         ENDIF
C
C  GET FILE NUMBER
      CALL PFDTYP (ITYPE,INDEXD)
      IF (INDEXD.EQ.0) GO TO 50
C
C  CHECK IF TIME SERIES EXISTS
      CALL PSERCH (ITSID,ITYPE,IFREE,IXREC,IXBUF)
      IF (IXREC.EQ.0) GO TO 40
C
C  SET RECORD NUMBER
      IREC=IXBUF(4)
C
      IF (IFUT.EQ.1.AND.ICKREF.EQ.1) THEN
C     GET RECORD NUMBER OF FIRST TIME SERIES OF THIS TYPE
         ISREC=DATFIL(8,INDEXR)
         IF (ISREC.LE.0) GO TO 30
C     READ TIME SERIES HEADER
20       IUNIT=DATFIL(2,INDEXR)
         NREC=2
         CALL RVLRCD (IUNIT,ISREC,NREC,IWKBUF,LRECLT,ISTAT)
         IF (ISTAT.NE.0) GO TO 60
         IF (IPRDB.GT.0) WRITE (LPD,100) (IWKBUF(J),J=1,13)
C     CHECK IF TIME SERIES IS REFERENCED
         IF (IWKBUF(11).EQ.IREC) GO TO 70
         ISREC=IWKBUF(13)
         IF (ISREC.NE.0) GO TO 20
         ENDIF
C
C  READ TIME SERIES HEADER
30    IUNIT=DATFIL(2,INDEXD)
      NREC=1
      CALL RVLRCD (IUNIT,IREC,NREC,IWKBUF,LRECLT,ISTAT)
      IF (ISTAT.NE.0) GO TO 60
C
C  DELETE FROM INDEX
      CALL UMEMST (-1,IXBUF,4)
      CALL UWRITT (KINDEX,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 60
C      
C  UPDATE COUNTERS
      NUMTMS=NUMTMS-1
      DATFIL(15,INDEXD)=DATFIL(15,INDEXD)-1
C
C  SET NAME TO DELETED - HEADER IS NOT EXPANDED SO NAME IS IN WORD 4, 
C  FUTURE POINTER IS IN WORD 11
      CALL UMEMOV (IDELET,IWKBUF(4),2)
      IWKBUF(11)=0
      CALL WVLRCD (IUNIT,IREC,1,IWKBUF,LRECLT,ISTAT)
      IF (ISTAT.NE.0) GO TO 60
      IF (IPRDB.GT.0) WRITE (LPD,110) ITSID,ITYPE
      GO TO 80
C
C  TIME SERIES NOT FOUND
40    IF (IPRDB.GT.0) WRITE (LP,120) ITYPE,ITSID
      ISTAT=1
      GO TO 80
C
C  INVALID DATA TYPE
50    IF (IPRERR.EQ.1) WRITE (LP,130) ITYPE
      ISTAT=3
      GO TO 80
C
C  READ/WRITE ERROR
60    IF (IPRERR.EQ.1) WRITE (LP,140) ISTAT
      GO TO 80
C
C  FUTURE TIME SERIES STILL USED BY REGULAR
70    IF (IPRERR.EQ.1) WRITE (LP,150) (IWKBUF(I),I=4,5)
      ISTAT=2
C
80    IF (IPRTR.GT.0) WRITE (IOGDB,160)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (' *** ENTER WPRDEL')
95    FORMAT ('0**ERROR** DATA TYPE ',A4,' DOES NOT HAVE SEPARATE ',
     *   'FUTURE TIME SERIES.')
100   FORMAT (' IWKBUF=',3I6,2X,4A4,2X,2F5.0,4I6)
110   FORMAT (' ITSID=',A,3X,'ITYPE=',A4)
120   FORMAT ('0**ERROR** ',A,' TIME SERIES ',A,' NOT FOUND.')
130   FORMAT ('0**ERROR** DATA TYPE ',A,' NOT FOUND.')
140   FORMAT ('0**ERROR** READING OR WRITING RECORD. ISTAT=',I4)
150   FORMAT ('0**ERROR** FUTURE TIME SERIES STILL REFERENCED BY ',
     *   'REGULAR TIME SERIES ',2A4,'.')
160   FORMAT (' *** EXIT WPRDEL')
C
      END
