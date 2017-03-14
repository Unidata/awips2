C MODULE PDZDAT
C-----------------------------------------------------------------------
C
      SUBROUTINE PDZDAT (DTYPE,IX,ISLOT,IPTREC,IREC,ISTAT)
C
C  THIS ROUTINE RESETS SLOTS IN THE DATA RECORDS.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM    DESCRIPTION
C       ------    ----  ---   ---    ------------
C       DTYPE      A4    I     1     DATA TYPE
C       IX         I     I     1     DIRECTORY SUBSCRIPT
C       ISLOT      I     I     1     FIRST SLOT
C       IPTREC     I     I     ?     POINTER RECORD
C       IREC       I     I     1     RECORD NUMBER
C       ISTAT      I     O     1     STATUS CODE:
C                                      O=OK
C                                     >0=ERRORS ENCOUNTERED
C
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdunts'
C
      CHARACTER*4 DTYPE
      INTEGER*2 IPTREC(32),IDTREC(32)
      INTEGER*2 LPP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdzdat.f,v $
     . $',                                                             '
     .$Id: pdzdat.f,v 1.3 1999/04/26 11:21:27 page Exp $
     . $' /
C    ===================================================================
C
      DATA LPP/2hPP/
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,70) DTYPE,ISLOT,IX
C
      MISS=MISSNG
C
C  CHECK IF VR TYPE
      IF (IDDTDR(6,IX).LT.0) GO TO 10
C
C  NOT A VR TYPE - USE ISLOT DIRECTLY FOR RESET
      NDATA=IDDTDR(6,IX)
      IDATPT=ISLOT
      IF (IDDTDR(2,IX).EQ.LPP) MISS=MISSPP
      GO TO 20
C
C  VR TYPE
10    JSLOT=ISLOT
      CALL PGTNXP (IX,JSLOT,IPTREC,IREC,ISTAT)
      IF (IDDTDR(5,IX).EQ.4) CALL PGTNXP (IX,JSLOT,IPTREC,IREC,ISTAT)
      IDELTA=IPTREC(JSLOT)
      NDATA=24/IDELTA
C
C  GET ONE MORE TO GET DATA POINTER
      CALL PGTNXP (IX,JSLOT,IPTREC,IREC,ISTAT)
      IDATPT=IPTREC(JSLOT)
C
C  COMPUTE DATA RECORD NUMBER
20    LRCPD2=LRCPDD*2
      NREC=IUNRCD(IDATPT,LRCPD2)-1
      IREC=NREC+IDDTDR(15,IX)
      JSAVE=IDATPT-(NREC*LRCPD2)
      NDAYS=IDDTDR(7,IX)
      IF (IPDDB.GT.0) WRITE (IOGDB,80) NREC,IDATPT,NDATA,MISS
C
C  RESET SLOTS FOR EACH DAY
      DO 50 J=1,NDAYS
C     READ DATA RECORD
         JSLOT=JSAVE
         IRECSV=IREC
         IF (IPDDB.GT.0) WRITE (IOGDB,90) J,IREC,JSLOT,NDATA
         CALL UREADT (KPDDDF(IDDTDR(4,IX)),IREC,IDTREC,ISTAT)
         IF (ISTAT.NE.0) GO TO 60
         IDTREC(JSLOT)=MISS
         IF (NDATA.LT.2) GO TO 40
C     DO NEXT SLOTS
         DO 30 I=2,NDATA
            CALL PGTNXP (IX,JSLOT,IDTREC,IREC,ISTAT)
            IF (ISTAT.NE.0) GO TO 60
            IDTREC(JSLOT)=MISS
30          CONTINUE
C     WRITE RECORD
40       CALL UWRITT (KPDDDF(IDDTDR(4,IX)),IREC,IDTREC,ISTAT)
         IF (ISTAT.NE.0) GO TO 60
C     GET RECORD NUMBER FOR NEXT DAY
         IREC=IRECSV+IDDTDR(21,IX)
50       CONTINUE
C
60    IF (IPDTR.GT.0) WRITE (IOGDB,100) JSAVE,ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' ENTER PDZDAT : DTYPE=',A,' ISLOT=',I5,' IX=',I5)
80    FORMAT (' NREC=',I6,3X,'IDATPT=',I6,3X,'NDATA=',I6,3X,
     *   'MISS=',I6)
90    FORMAT (' J=',I6,3X,'IREC=',I6,3X,'JSLOT=',I6,3X,
     *   'NDATA=',I6)
100   FORMAT (' EXIT PDZDAT : JSAVE=',I4,' ISTAT=',I4)
C
      END
