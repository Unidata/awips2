C MODULE PDZAPP
C-----------------------------------------------------------------------
C
      SUBROUTINE PDZAPP (DTYPE,ISLOT,IX,LDELTP,DELTP,NDELTP,ISTAT)
C
C  THIS ROUTINE RESETS SLOTS IN THE POINTER RECORDS.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       DTYPE      A4    I     1     DATA TYPE
C       ISLOT      I     I     1     FIRST SLOT
C       IX         I     I     1     DIRECTORY SUBSCRIPT
C       LDELTP     I     I     1     DIMENSION OF ARRAY DELTP
C       DELTP      I     O   LDELTP  ARRAY OF DELETED TYPES
C       NDELTP     I     I     1     NUMBER OF ENTRIES IN DELTP
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
      CHARACTER*4 DTYPE,DELTP(LDELTP)
      INTEGER*2 IPTREC(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdzapp.f,v $
     . $',                                                             '
     .$Id: pdzapp.f,v 1.4 2002/02/11 20:54:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,50) DTYPE,ISLOT
C
C  COMPUTE RECORD NUMBER
      LRCPD2=LRCPDD*2
      NREC=IUNRCD(ISLOT,LRCPD2)-1
      IREC=NREC+IDDTDR(14,IX)
      JSLOT=ISLOT-(NREC*LRCPD2)
C
C  READ POINTER RECORD
      CALL UREADT (KPDDDF(IDDTDR(4,IX)),IREC,IPTREC,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
C
C  RESET FIRST SLOT
      IPTREC(JSLOT)=0
C
C  CHECK NUMBER OF POINTERS
      IF (IDDTDR(5,IX).LT.2) GO TO 20
C
C  CHECK IF TAVR OR PPVR (REST OF SLOTS NOT RESET SO CAN REUSE)
      IF (IDDTDR(6,IX).LT.0) GO TO 20
C
C  RESET REST OF SLOTS
      NUMSLT=IDDTDR(5,IX)
      DO 10 I=2,NUMSLT
         CALL PGTNXP (IX,JSLOT,IPTREC,IREC,ISTAT)
         IF (ISTAT.NE.0) GO TO 40
         IPTREC(JSLOT)=0
10       CONTINUE
C
C  WRITE RECORD
20    CALL UWRITT (KPDDDF(IDDTDR(4,IX)),IREC,IPTREC,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
C
C  CHECK IF PPVR OR TAVR
      IF (IDDTDR(6,IX).GT.0) GO TO 30
C
C  RESET DATA SLOTS
      CALL PDZDAT (DTYPE,IX,JSLOT,IPTREC,IREC,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
C
C  DECREASE NUMBER OF STATIONS
30    IDDTDR(17,IX)=IDDTDR(17,IX)-1
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'IN PDZAPP - IX=',IX,
     *   ' IDDTDR(17,IX)=',IDDTDR(17,IX)
C
C  UPDATE ARRAY OF DELETED TYPES
      IF (NDELTP+1.GT.LDELTP) THEN
         ISTAT=-1
         GO TO 40
         ENDIF
      NDELTP=NDELTP+1
      CALL UMEMOV (DTYPE,DELTP(NDELTP),1)
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,60) ISLOT,ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' ENTER PDZAPP : DTYPE=',A,' ISLOT=',I5)
60    FORMAT (' EXIT PDZAPP : ISLOT=',I4,' ISTAT=',I4)
C
      END
