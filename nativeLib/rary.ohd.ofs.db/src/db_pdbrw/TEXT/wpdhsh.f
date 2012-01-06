C MEMBER WPDHSH
C  (from old member PDCOIO2)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/23/94.16:09:53 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPDHSH (IDXTYP,ISTAT)
C
C          ROUTINE:  WPDHSH
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
C   THIS ROUTINE WRITES THE CHARACTER AND INTEGER INDEX ARRAYS
C   TO THE PREPROCESSOR DATA BASE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        IDXTYP     I    I    1     INDEX TYPE
C                                     0=WRITE CHARACTER AND INTEGER
C                                       INDEX
C                                     1=WRITE CHARACTER INDEX
C                                     2=WRITE INTEGER INDEX
C        ISTAT      I    O    1     STATUS
C                                     0=OK
C                                     OTHER=ERROR
C
C***********************************************************************
C
C          DIMENSION:
C
      PARAMETER (LBUF=32)
      INTEGER*2 IBUF(LBUF)
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urhshc'
      INCLUDE 'urcommon/urhshi'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpdhsh.f,v $
     . $',                                                             '
     .$Id: wpdhsh.f,v 1.2 1996/07/11 20:24:42 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,80)
C
      ISTAT=0
C
C  SET UNIT NUMBER
      IUNIT=0
      IF (IAMORD.EQ.0) IUNIT=KPDSIF
      IF (IAMORD.EQ.1) IUNIT=KURSIF
      IF (IUNIT.EQ.0) THEN
         WRITE (LP,85) 'UNIT NUMBER FOR FILE'
         ISTAT=2
         GO TO 70
         ENDIF
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' IAMORD=',IAMORD,
     *   ' IUNIT=',IUNIT,
     *   ' '
C
C  SET LENGTH OF RECORDS - INDEXES ARE STORED AS I*2 VARIABLES
      LRCPD2=0
      IF (IAMORD.EQ.0) LRCPD2=LRCPDI*2
      IF (IAMORD.EQ.1) LRCPD2=LRLURI*2
      IF (LRCPD2.EQ.0) THEN
         WRITE (LP,85) 'LENGTH OF RECORDS'
         ISTAT=2
         GO TO 70
         ENDIF
C
C  COMPUTE NUMBER OF RECORDS TO BE WRITTEN
      MHASHR=0
      IF (IAMORD.EQ.0) MHASHR=NHASHR
      IF (IAMORD.EQ.1) MHASHR=IHASHR
      IF (MHASHR.EQ.0) THEN
         WRITE (LP,85) 'SIZE OF INDEX ARRAY'
         ISTAT=2
         GO TO 70
         ENDIF
      NREC=IUNRCD(MHASHR,LRCPD2)
C
C  CHECK IF MAXIMUM WORDS IN INDEXES EXCEEDED
      NWORDS=NREC*LRCPD2
      ILAST=0
      IPOS=0
      NPOS=0
      IF (IAMORD.EQ.0.AND.NWORDS.LE.MXHSHC) GO TO 10
      IF (IAMORD.EQ.1.AND.NWORDS.LE.MURHSC) GO TO 10
C
C  CHECK LENGTH OF ARRAY
      IF (LRCPD2.GT.LBUF) THEN
         WRITE (LP,90) LRCPD2,LBUF
         ISTAT=2
         GO TO 70
         ENDIF
C
C  SET VARIABLES FOR READING INDEX POSITIONS FROM LAST RECORD
      ILAST=1
      NREC=NREC-1
      IPOS=NREC*LRCPD2
      NPOS=MHASHR-IPOS
C
10    IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' NREC=',NREC,
     *   ' LRCPD2=',LRCPD2,
     *   ' NWORDS=',NWORDS,
     *   ' MHASHR=',MHASHR,
     *   ' ILAST=',ILAST,
     *   ' '
C
C  CHECK IF CHARACTER INDEX RECORDS TO BE WRITTEN
      IF (IDXTYP.EQ.2) GO TO 40
C
C  WRITE CHARACTER INDEX RECORDS
      IF (IAMORD.EQ.0) THEN
         CALL WVLRCD (IUNIT,IH8CHR,NREC,IPDHSC,LRCPDI,ISTAT)
         IF (ISTAT.GT.0) GO TO 60
         ENDIF
      IF (IAMORD.EQ.1) THEN
         CALL WVLRCD (IUNIT,NH8CHR,NREC,IURHSC,LRLURI,ISTAT)
         IF (ISTAT.GT.0) GO TO 60
         ENDIF
C
C  CHECK IF LAST RECORD NEEDS TO BE WRITTEN
      IF (ILAST.EQ.0) GO TO 30
C
C  UPDATE LAST RECORD
      IF (IAMORD.EQ.0) IREC=IH8CHR+NREC
      IF (IAMORD.EQ.1) IREC=NH8CHR+NREC
      CALL UREADT (IUNIT,IREC,IBUF,ISTAT)
      IF (ISTAT.GT.0) GO TO 60
      DO 20 I=1,NPOS
         IF (IAMORD.EQ.0) IBUF(I)=IPDHSC(IPOS+I)
         IF (IAMORD.EQ.1) IBUF(I)=IURHSC(IPOS+I)
20       CONTINUE
      CALL UWRITT (IUNIT,IREC,IBUF,ISTAT)
      IF (ISTAT.GT.0) GO TO 60
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' IUNIT=',IUNIT,
     *   ' IREC=',IREC,
     *   ' IPOS=',IPOS,
     *   ' NPOS=',NPOS,
     *   ' '
C
C  CHECK IF INTEGER INDEX RECORDS TO BE WRITTEN
30    IF (IDXTYP.EQ.1) GO TO 70
C
C  WRITE INTEGER INDEX RECORDS
40    IF (IAMORD.EQ.0) THEN
         CALL WVLRCD (IUNIT,IHINRC,NREC,IPDHSI,LRCPDI,ISTAT)
         IF (ISTAT.GT.0) GO TO 60
         ENDIF
      IF (IAMORD.EQ.1) THEN
         CALL WVLRCD (IUNIT,NHINRC,NREC,IURHSI,LRLURI,ISTAT)
         IF (ISTAT.GT.0) GO TO 60
         ENDIF
C
C  CHECK IF LAST RECORD NEEDS TO BE WRITTEN
      IF (ILAST.EQ.0) GO TO 70
C
C  UPDATE LAST RECORD
      IF (IAMORD.EQ.0) IREC=IHINRC+NREC
      IF (IAMORD.EQ.1) IREC=NHINRC+NREC
      CALL UREADT (IUNIT,IREC,IBUF,ISTAT)
      IF (ISTAT.GT.0) GO TO 60
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' IUNIT=',IUNIT,
     *   ' IREC=',IREC,
     *   ' IPOS=',IPOS,
     *   ' NPOS=',NPOS,
     *   ' '
      DO 50 I=1,NPOS
         IF (IAMORD.EQ.0) IBUF(I)=IPDHSI(IPOS+I)
         IF (IAMORD.EQ.1) IBUF(I)=IURHSI(IPOS+I)
         IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *      ' IPOS=',IPOS,
     *      ' I=',I,
     *      ' IAMORD=',IAMORD,
     *      ' IPDHSI(IPOS+I)=',IPDHSI(IPOS+I),
     *      ' '
50       CONTINUE
      CALL UWRITT (IUNIT,IREC,IBUF,ISTAT)
      IF (ISTAT.GT.0) GO TO 60
      GO TO 70
C
C  WRITE ERROR
60    IF (IPDDB.GT.0) WRITE (IOGDB,100) IUNIT
      ISTAT=1
C
70    IF (IPDTR.GT.0) WRITE (IOGDB,110) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER WPDHSH')
85    FORMAT ('0*** ERROR - IN WPDHSH - ',A,' TO BE ',
     *   'WRITTEN IS ZERO.')
90    FORMAT ('0*** ERROR - IN WPDHSH - LENGTH OF RECORD TO BE ',
     *   'WRITTEN (',I3,') EXCEEDS LENGTH OF WORK ARRAY (',I3,').')
100   FORMAT ('0*** ERROR - IN WPDHSH - WRITING PPDB HASH RECORDS ',
     *   'TO UNIT ',I2,'.')
110   FORMAT (' *** EXIT WPDHSH : ISTAT=',I3)
C
      END
