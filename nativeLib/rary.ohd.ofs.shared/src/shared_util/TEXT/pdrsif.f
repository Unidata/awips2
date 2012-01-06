C MODULE PDRSIF
C-----------------------------------------------------------------------
C
      SUBROUTINE PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,ISTAT)
C
C   THIS ROUTINE READS A STATION INFORMATION FILE (SIF) RECORDS FROM
C   THE PREPROCESSOR DATA BASE.
C
C   ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM     DESCRIPTION
C       ------   ----   ---   ---     -----------
C       IREC       I     I     1      NUMBER OF SIF RECORD TO BE READ
C       NXREC      I     O     1      RECORD NUMBER OF NEXT SIF RECORD
C       LSIBUF     I     I     1      DIMENSION OF ARRAY ISIBUF
C       ISIBUF    I*2    I   LSIBUF   SIF RECORD BUFFER
C       ISTAT      I     I     1      STATUS CODE
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urunts'
C
      CHARACTER*8 STAID
      INTEGER*2 ISIBUF(LSIBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/pdrsif.f,v $
     . $',                                                             '
     .$Id: pdrsif.f,v 1.7 2000/12/18 21:39:00 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,40)
C
      ISTAT=0
C
      LRCPD2=LRCPDI*2
C
C  SET UNIT NUMBER
      IF (IAMORD.EQ.0) IUNIT=KPDSIF
      IF (IAMORD.EQ.1) IUNIT=KURSIF
C
C  SET RECORD NUMBER OF LAST SIF RECORD
      IF (IAMORD.EQ.0) LSTREC=LSTSIF
      IF (IAMORD.EQ.1) LSTREC=LTSIFR
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' IUNIT=',IUNIT,
     *   ' LSTSIF=',LSTSIF,
     *   ' '
C
C  CHECK IF SIF RECORD NUMBER IS GREATER THAN LAST RECORD USED
10    IF (IREC.GT.LSTREC) THEN
         IF (IDELTD.EQ.0) THEN
            WRITE (LP,60) IREC,LSTREC
            CALL UEROR (LP,0,-1)
            ISTAT=1
            ENDIF
         GO TO 30
         ENDIF
C
      IDELTD=0
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*) ' IREC=',IREC
C
C  CHECK SIZE OF SIF ARRAY
      IF (LRCPDI.GT.LSIBUF) THEN
         WRITE (LP,80) IREC,LRCPDI,LSIBUF
         CALL UEROR (LP,0,-1)
         ISTAT=1
         GO TO 30
         ENDIF
C
C  READ FIRST PART OF SIF RECORD
      CALL RVLRCD (IUNIT,IREC,1,ISIBUF,LRCPDI,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (LP,50) IREC,IUNIT
         CALL UEROR (LP,0,-1)
         GO TO 30
         ENDIF
C
C  GET STATION IDENTIFIER
      CALL UMEMOV (ISIBUF(2),STAID,2)
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*) ' STAID=',STAID
C
C  CHECK IF A DELETED RECORD
      IF (STAID.EQ.'DELETED') THEN
         IDELTD=1
         ELSE
C        CHECK IF STATION FOUND IN INDEX
            CALL PDFNDR (STAID,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
            IF (IFIND.EQ.0) THEN
               WRITE (LP,90) IREC,IREC+1
               CALL UEROR (LP,0,-1)
               IREC=IREC+1
               ISTAT=1
               GO TO 10
               ENDIF
            IF (ISTAT.NE.0) THEN
               WRITE (LP,100) ISTAT,STAID,IREC
               CALL UEROR (LP,0,-1)
               ENDIF
         ENDIF
      IF (STAID.EQ.'?' ) THEN
         IPDDB=1
         ENDIF
C
C  SET NUMBER OF RECORDS
      NWORDS=ISIBUF(1)
      NRECT=IUNRCD(NWORDS,LRCPD2)
C
C  SET NUMBER OF RECORDS TO BE READ TO GET REST OF SIF RECORD
      NREC=NRECT-1
      IF (IPDDB.GT.0) WRITE (IOGDB,*) ' NREC=',NREC
C
      LREC=IREC+1
C
C  CHECK IF NO ADDITIONAL RECORDS NEED TO BE READ
      IF (NREC.EQ.0) GO TO 20
C
      IF (NREC.LT.0) THEN
         WRITE (LP,70) IREC,NREC,IREC+1
         CALL UEROR (LP,0,-1)
         IREC=IREC+1
         ISTAT=1
         GO TO 10
         ENDIF
C
C  CHECK SIZE OF SIF ARRAY
      IF (NWORDS.GT.LSIBUF) THEN
         WRITE (LP,80) IREC,NWORDS,LSIBUF,IREC+1
         CALL UEROR (LP,0,-1)
         IREC=IREC+1
         ISTAT=1
         GO TO 10
         ENDIF
C
C  READ REST OF SIF RECORD
      CALL RVLRCD (IUNIT,LREC,NREC,ISIBUF(LRCPD2+1),LRCPDI,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (LP,50) IREC,IUNIT
         CALL UEROR (LP,0,-1)
         GO TO 30
         ENDIF
C
C  SET NEXT RECORD NUMBER
20    NRECA=NREC
      NWORDS2=NWORDS+1
      NREC2=IUNRCD(NWORDS2,LRCPD2)
      IF (NREC2.LE.NRECT) THEN
C     GET NUMBER OF WORDS IN OLD SIF RECORD
         NWORDSO=ISIBUF(NWORDS2)
         IF (NWORDSO.GT.0) THEN
            IF (NWORDSO.GT.LSIBUF) THEN
               WRITE (LP,110) NWORDSO,IREC,STAID,LSIBUF,IREC+1
               CALL UWARN (LP,0,-1)
               IREC=IREC+1
               ISTAT=1
               GO TO 10
               ELSE
                  NRECO=IUNRCD(NWORDSO,LRCPD2)-1
                  IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *               ' NRECO=',NRECO,
     *               ' '
                  NRECA=NRECO
               ENDIF
            ENDIF
         ENDIF
      NXREC=LREC+NRECA
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' STAID=',STAID,
     *   ' IREC=',IREC,
     *   ' NXREC=',NXREC,
     *   ' '
C
      IF (IDELTD.EQ.1) THEN
         IREC=NXREC
         GO TO 10
         ENDIF
C
30    IF (IPDTR.GT.0) WRITE (IOGDB,120)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' ENTER PDRSIF')
50    FORMAT ('0**ERROR** IN PDRSIF - ERROR READING RECORD ',I6,
     *   ' FROM UNIT ',I2,'.')
60    FORMAT ('0**ERROR** IN PDRSIF - ',
     *   'RECORD NUMBER OF SIF RECORD TO BE READ (',I4,
     *   ') EXCEEDS RECORD NUMBER OF LAST SIF RECORD (',I4,').')
70    FORMAT ('0**ERROR** IN PDRSIF - ',
     *   'NUMBER OF RECORDS IN SIF RECORD NUMBER ',I4,' ',
     *   '(',I4,') IS LESS THAN ZERO. ' :
     *   'RECORD ',I4,' WILL BE TRIED.')
80    FORMAT ('0**ERROR** IN PDRSIF - ',
     *   'NUMBER OF WORDS NEEDED TO READ SIF RECORD NUMBER ',I4,' ',
     *   '(',I3,') EXCEEDS ARRAY SIZE (',I3,'). ' :
     *   'RECORD ',I4,' WILL BE TRIED.')
90    FORMAT ('0**ERROR** IN PDRSIF - ',
     *   'STATION IDENTIFIER FOUND AT RECORD ',I4,' ',
     *   'NOT FOUND IN INDEX. ',
     *   'RECORD ',I4,' WILL BE TRIED.')
100   FORMAT ('0**ERROR** IN PDRSIF - ',
     *   'STATUS CODE ',I2,' RETURNED BY ROUTINE PDFNDR ',
     *   'PROCESSING STATION ',A,' AT RECORD ',I4,'.')
110   FORMAT ('0**WARNING** IN PDRSIF - ',
     *   'NUMBER OF WORDS (',I5,') ',
     *   'IN OLD SIF RECORD NUMBER ',I4,' ',
     *   'FOR STATION ',A,' ',
     *   'EXCEEDS ARRAY SIZE (',I3,'). ',
     *   'RECORD ',I4,' WILL BE TRIED.')
120   FORMAT (' EXIT PDRSIF')
C
      END
