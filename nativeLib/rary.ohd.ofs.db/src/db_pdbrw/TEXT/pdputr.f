C MODULE PDPUTR
C-----------------------------------------------------------------------
C
       SUBROUTINE PDPUTR (ISIBUF,IFREEC,IFREEI,ISTAT)
C
C  ROUTINE TO ENTER A STATION INFORMATION RECORD IN THE PREPROCESSOR
C  DATA BASE INDEX.
C
C  HASH SLOTS ARE IFREEC FOR CHAR HASH AND IFREEI FOR INTEGER
C  HASH. RECORD GOES IN NEXT AVAILABLE SIF RECORD
C
C  ARGUMENT LIST:
C
C       NAME    TYPE  I/O   DIM   DESCRIPTION
C       ------  ----  ---   ---   -----------
C       ISIBUF   I*2   I     ?     STATION INFORMATION BUFFER
C       IFREEC   I     I     1     SUBSCRIPT OF CHARACTER HASH
C       IFREEI   I     I     1     SUBSCRIPT OF INTEGER HASH (ZERO IF
C                                    NO ENTRY)
C       ISTAT    I     O     1     STATUS CODE:
C                                    0=OK
C                                    1=READ/WRITE ERROR
C                                    2=NO UNUSED SIF RECORDS
C
      INTEGER*2 ISIBUF(*)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdhshi'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdputr.f,v $
     . $',                                                             '
     .$Id: pdputr.f,v 1.2 2002/02/11 20:56:50 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0.OR.IPDDB.GT.0) WRITE (IOGDB,*) 'ENTER PDPUTR -',
     *   ' IFREEC=',IFREEC,' IFREEI=',IFREEI
C
      ISTAT=0
      LRCPD2=LRCPDI*2
C
C  GET NEXT SIF RECORD NUMBER
      IREC=LSTSIF+1
      NWDS=ISIBUF(1)
      NREC=IUNRCD(NWDS,LRCPD2)
C
C  CHECK FOR ENOUGH RECORDS IN FILE
      IF (IREC+NREC.GT.MXSIFR) THEN
         IF (IPDDB.GT.0) WRITE (IOGDB,5) IREC,NREC,MXSIFR
5     FORMAT (' NOT ENOUGH UNUSED STATION INFORMATION RECORDS:',
     *   ' NEXT AVAILABLE RECORD=',I5,
     *   ' RECORDS NEEDED=',I2,
     *   ' MAXIMUM RECORDS=',I5)
         ISTAT=2
         GO TO 10
         ENDIF
C
      IF (IREC.EQ.3345) THEN
         WRITE (IOGDB,*) 'IREC=',IREC
         ENDIF 
C
C  WRITE RECORDS
      CALL WVLRCD (KPDSIF,IREC,NREC,ISIBUF,LRCPDI,ISTAT)
      IF (ISTAT.NE.0) THEN
         ISTAT=1
         GO TO 10
         ENDIF
C
C  SET HASH POINTERS
      IPDHSC(IFREEC)=IREC
      IF (IFREEI.NE.0) IPDHSI(IFREEI)=IREC
      IF (IPDDB.GT.0) WRITE (IOGDB,*)  'IN PDPUTR -',
     *   'IFREEC=',IFREEC,'IFREEI=',IFREEI,'IREC=',IREC,NREC
C
C  INCREMENT LAST USED
      LSTSIF=LSTSIF+NREC
C
10    IF (IPDTR.GT.0.OR.IPDDB.GT.0) WRITE (IOGDB,*) 'EXIT PDPUTR -',
     *   ' ISTAT=',ISTAT
C
      RETURN
C
      END
