C MEMBER RPPDCO
C  (from old member PDCOIO1)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPPDCO (ISTAT)
C
C          ROUTINE:  RPPDCO
C
C             VERSION:  1.0.0
C
C                DATE:  01-05-83
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE READS THE CONTROL RECORDS FROM THE PREPROCESSOR DATA
C  DATA BASE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISTAT       I    O    1     STATUS 0=OK, NOT 0=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdenqx'
      INCLUDE 'pdbcommon/pdsifc'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rppdco.f,v $
     . $',                                                             '
     .$Id: rppdco.f,v 1.1 1995/09/17 18:44:45 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
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
C  READ SIF CONTROL RECORD
      IREC=1
      IF (IAMORD.EQ.0)
     *   CALL UREADT (IUNIT,IREC,NWDCTL,ISTAT)
      IF (IAMORD.EQ.1)
     *   CALL UREADT (IUNIT,IREC,NWCTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C  SET RECORD LENGTH
      IF (IAMORD.EQ.0) LRECL=LRCPDI
      IF (IAMORD.EQ.1) LRECL=LRLURI
C
C  COMPUTE NUMBER OF RECORDS PROCESSED
      IF (IAMORD.EQ.0) NWORDS=NWDCTL
      IF (IAMORD.EQ.1) NWORDS=NWCTL
      NREC=IUNRCD(NWORDS,LRECL)
C
C  READ DAILY DATA FILE CONTROLS - 4 I*4 WORDS/ENTRY
      IREC=NREC+1
      IF (IAMORD.EQ.0) NWORDS=NUMDDF*4
      IF (IAMORD.EQ.1) NWORDS=NMDDF*4
      NREC=IUNRCD(NWORDS,LRECL)
      IF (IAMORD.EQ.0)
     *   CALL RVLRCD (IUNIT,IREC,NREC,IPDDFC,LRECL,ISTAT)
      IF (IAMORD.EQ.1)
     *   CALL RVLRCD (IUNIT,IREC,NREC,JPDDFC,LRECL,ISTAT)
C
C  READ DAILY DATA DIRECTORY - 24 I*2 WORDS/ENTRY
      IREC=IREC+NREC
      IF (IAMORD.EQ.0) NWORDS=NMDTYP*12
      IF (IAMORD.EQ.1) NWORDS=NPDTYP*12
      NREC=IUNRCD(NWORDS,LRECL)
      IF (IAMORD.EQ.0)
     *   CALL RVLRCD (IUNIT,IPTTYP,NREC,IDDTDR,LRECL,ISTAT)
      IF (IAMORD.EQ.1)
     *   CALL RVLRCD (IUNIT,IPTDTP,NREC,JDDTDR,LRECL,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C  READ RRS CONTROL RECORD
      IF (IAMORD.EQ.0) IUNIT=KPDRRS
      IF (IAMORD.EQ.1) IUNIT=KURRRS
      IREC=1
      IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,MXRRSF,ISTAT)
      IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,MAXRSF,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C  CHECK INUSE FLAG AND PRINT WARNING IF SET
      IF (IPUSER(1).EQ.0) CALL UREPET ('?',IPUSER,8)
      IF (INUSEF.NE.0) WRITE (LP,40) IPUSER
C
C  READ RRS DATA TYPES
      CALL PDTRRS (ISTAT)
C
C  CHECK IF TO ENQ PPDB
      IF (IPDENQ.EQ.0) GO TO 20
      CALL PDENDQ ('ENQ ',IERR)
      IF (IERR.EQ.0) IPDENQ=-IPDENQ
      GO TO 20
C
C  ERRORS
10    WRITE (LPE,50) IUNIT
C
20    IF (IPDTR.GT.0) WRITE (IOGDB,60) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER RPPDCO')
40    FORMAT (' **WARNING ** THE PPDB IN-USE FLAG IS SET ',
     *      'FOR USER ',2A4,'. FILES ARE BEING UPDATED BY AN ',
     *      'AUTOMATIC TRANSFER PROGRAM OR ' /
     *   T15,'A PREVIOUS TRANSFER RUN TERMINATED ABNORMALLY.')
50    FORMAT (' **ERROR** IN RPPDCO - READING PPDB CONTROLS FROM UNIT ',
     *   I2,'.')
60    FORMAT (' *** EXIT RPPDCO : ISTAT',I3)
C
      END
