C MODULE PGTNXP
C-----------------------------------------------------------------------
C
      SUBROUTINE PGTNXP (IX,JSLOT,IPTREC,IREC,ISTAT)
C
C  THIS ROUTINE UPDATES THE INDEX OF A POINTER RECORD AS
C  ENTRIES ARE BEING MADE.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ----      ----  ---   ---   -----------
C       IX          I    IO     1   SUBSCRIPT OF DAILY DATA DIRECTORY
C       JSLOT       I    O      1   SUBSCRIPT OF POINTER RECORD
C       IPTREC     I*2   O      ?   POINTER RECORD
C       IREC        I   I/O     1   RECORD NUMBER
C       ISTAT       I    O      1   STATUS CODE:
C                                     0=OK
C                                     1=READ/WRITE ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdunts'
C
      INTEGER*2 IPTREC(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pgtnxp.f,v $
     . $',                                                             '
     .$Id: pgtnxp.f,v 1.2 1999/04/23 19:32:21 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' ENTER PGTNXP')
C
      ISTAT=0
C
      LRCDD2=LRCPDD*2
C
C  CHECK IF NEED TO READ NEW RECORD
      IF (JSLOT.LT.LRCDD2) GO TO 40
C
C  WRITE RECORD
      IFILE=IDDTDR(4,IX)
      CALL UWRITT (KPDDDF(IFILE),IREC,IPTREC,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
      IREC=IREC+1
      MAX=IDDTDR(15,IX)+IDDTDR(7,IX)*IDDTDR(21,IX)
      IF (IREC.GT.MAX) THEN
         ISTAT=1
         WRITE (LP,20) IREC,MAX,IX
20    FORMAT ('0**ERROR** IN PGTNXP - RECORD NUMBER (',I5,') ',
     *   'IS LARGER THAN MAXIMUM VALUE (',I5,') ',
     *   'FOR FILE NUMBER ',I2,'.')
         GO TO 70
         ENDIF
C
C  READ NEXT RECORD
      CALL UREADT (KPDDDF(IFILE),IREC,IPTREC,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
      JSLOT=1
      GO TO 70
C
C  UPDATE SLOT NUMBER
40    JSLOT=JSLOT+1
      GO TO 70
C
C  READ OR WRITE ERROR
50    WRITE (LP,60) IFILE,ISTAT
60    FORMAT ('0**ERROR** READING OR WRITING DAILY DATA FILE NUMBER ',
     *   I3,'. STATUS CODE = ',I2)
C
70    IF (IPDTR.GT.0) WRITE (IOGDB,80) JSLOT
80    FORMAT (' EXIT PGTNXP - JSLOT=',I3)
C
      RETURN
C
      END
