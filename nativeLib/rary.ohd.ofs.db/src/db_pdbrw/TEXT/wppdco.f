C MODULE WPPDCO
C-----------------------------------------------------------------------
C
      SUBROUTINE WPPDCO (ISTAT)
C
C  THIS ROUTINE WRITES THE CONTROL RECORDS TO THE PREPROCESSOR DATA
C  DATA BASE.
C
C  ARGUMENT LIST:
C
C       NAME    TYPE   I/O   DIM   DESCRIPTION
C       ----    ----   ---   ---   -----------
C       ISTAT    I      O     1    STATUS: 0=OK, NOT 0=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdenqx'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urpddd'
      INCLUDE 'urcommon/urpddt'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urrrsc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wppdco.f,v $
     . $',                                                             '
     .$Id: wppdco.f,v 1.2 1999/01/19 19:42:49 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,30)
C
      ISTAT=0
C
C  SET UNIT NUMBER
      IF (IAMORD.EQ.0) IUNIT=KPDSIF
      IF (IAMORD.EQ.1) IUNIT=KURSIF
C
C  SET RECORD LENGTH
      IF (IAMORD.EQ.0) LRECL=LRCPDI
      IF (IAMORD.EQ.1) LRECL=LRLURI
C
C  WRITE SIF CONTROL RECORD
      IREC=1
      IF (IAMORD.EQ.0) NWORDS=NWDCTL
      IF (IAMORD.EQ.1) NWORDS=NWCTL
      NREC=IUNRCD(NWORDS,LRECL)
      IF (NREC.EQ.0) THEN
         WRITE (LP,35) 'SIF',IUNIT
         ISTAT=1
         GO TO 20
         ENDIF
      IF (IAMORD.EQ.0) THEN
         CALL WVLRCD (IUNIT,IREC,NREC,NWDCTL,LRECL,ISTAT)
         IF (ISTAT.NE.0) GO TO 10
         ENDIF
      IF (IAMORD.EQ.1) THEN
         CALL WVLRCD (IUNIT,IREC,NREC,NWCTL,LRECL,ISTAT)
         IF (ISTAT.NE.0) GO TO 10
         ENDIF
C
C  WRITE DAILY DATA FILE CONTROLS - 4 I*4 WORDS/ENTRY
      IREC=NREC+1
      IF (IAMORD.EQ.0) NWORDS=NUMDDF*4
      IF (IAMORD.EQ.1) NWORDS=NMDDF*4
      NREC=IUNRCD(NWORDS,LRECL)
      IF (NREC.EQ.0) THEN
         WRITE (LP,35) 'DAILY DATA FILE CONTROL',IUNIT
         ISTAT=1
         GO TO 20
         ENDIF
      IF (IAMORD.EQ.0) THEN
         CALL WVLRCD (IUNIT,IREC,NREC,IPDDFC,LRECL,ISTAT)
         IF (ISTAT.NE.0) GO TO 10
         ENDIF
      IF (IAMORD.EQ.1) THEN
         CALL WVLRCD (IUNIT,IREC,NREC,JPDDFC,LRECL,ISTAT)
         IF (ISTAT.NE.0) GO TO 10
         ENDIF
C
C  WRITE DAILY DATA DIRECTORY - 24 I*2 WORDS/ENTRY
      IF (IAMORD.EQ.0) NWORDS=NMDTYP*12
      IF (IAMORD.EQ.1) NWORDS=NPDTYP*12
      NREC=IUNRCD(NWORDS,LRECL)
      IF (NREC.EQ.0) THEN
         WRITE (LP,35) 'DAILY DATA FILE DIRECTORY',IUNIT
         ISTAT=1
         GO TO 20
         ENDIF
      IF (IAMORD.EQ.0) THEN
         CALL WVLRCD (IUNIT,IPTTYP,NREC,IDDTDR,LRECL,ISTAT)
         IF (ISTAT.NE.0) GO TO 10
         ENDIF
      IF (IAMORD.EQ.1) THEN
         CALL WVLRCD (IUNIT,IPTDTP,NREC,JDDTDR,LRECL,ISTAT)
         IF (ISTAT.NE.0) GO TO 10
         ENDIF
C
C  WRITE RRS CONTROL RECORD - RESET INUSE FLAG
      IF (IAMORD.EQ.0) IUNIT=KPDRRS
      IF (IAMORD.EQ.1) IUNIT=KURRRS
      IREC=1
      INUSEF=0
      IF (IAMORD.EQ.0) THEN
         CALL UWRITT (IUNIT,IREC,MXRRSF,ISTAT)
         IF (ISTAT.NE.0) GO TO 10
         ENDIF
      IF (IAMORD.EQ.1) THEN
         CALL UWRITT (IUNIT,IREC,MAXRSF,ISTAT)
         IF (ISTAT.GT.0) GO TO 10
         ENDIF
C
C  DEQ PPDB
      IF (IPDENQ.NE.0) CALL PDDEQ()
      GO TO 20
C
C  ERRORS
10    WRITE (LP,40) IUNIT
C
20    IF (IPDTR.GT.0) WRITE (IOGDB,50) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER WPPDCO')
35    FORMAT ('0**ERROR** IN WPPDCO - NUMBER OF ',A,' RECORDS TO BE ',
     *   'WRITTEN TO UNIT ',I2,' IS ZERO.')
40    FORMAT ('0**ERROR** IN WPPDCO - WRITING PPDB CONTROLS TO UNIT ',
     *   I2,'.')
50    FORMAT (' *** EXIT WPPDCO : ISTAT=',I3)
C
      END
